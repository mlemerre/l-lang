(* Copyright 2012 Matthieu Lemerre *)

open Cpsdef;;

type fresh = Fresh.t

(*s Replace the uplinks in the elements in [term.term], to point to
  [term].

  This is the low-level counterpart to the [disconnect] operation;
  but rather than giving a separate empty and fresh term, it is
  simple to expect a pre-assembled term that just needs to be
  updated.

  Note that the purpose of the [reconnect] argument in the [Build]
  module is to avoid this operation. *)
let update_uplinks term =
  (* Change uplinks of subterms and variables to term. *)
  let new_enclosing = Enclosing_term term in
  (match Term.get term with
  | Apply(_,_,_,_) | Apply_cont(_,_) | Halt(_) -> ()
  | Let_prim(x,p,body) ->
    Cpscheck.And.set_enclosing body new_enclosing;
    Var.Var.set_binding_site x new_enclosing;
    (match p with
    | Value(Lambda(_,k,vl,body)) ->
      Cpscheck.And.set_enclosing body new_enclosing;
      List.iter (fun x -> Var.Var.set_binding_site x new_enclosing) vl
    | _ -> ())
  | Let_cont(k,x,t,body) ->
    Cpscheck.And.set_enclosing body new_enclosing;
    Cpscheck.And.set_enclosing t new_enclosing;
    Var.Var.set_binding_site x new_enclosing)
;;

(*s Disconnect a term [t] from its term_ [t]; [t] becomes [empty] and
  is returned; a [fresh] term is created around [t_] and is
  returned. *)
let disconnect term =
  Cpscheck.Uplinks.one_term term;
  let newterm = Term.make (Term.get term) in
  update_uplinks newterm;
  Cpscheck.Uplinks.one_term newterm;
  (Empty.empty term,newterm);;

(****************************************************************)
(*s Deletion functions. Their main role is to remove the occurrences
  that appear in the deleted terms. *)

let delete_apply term =
  match Term.get term with
  | Apply(_,f,k,ol) ->
    Var.Occur.delete f;
    Cont_var.Occur.delete k;
    List.iter Var.Occur.delete ol;
    Empty.empty term
  | _ -> assert false
;;

let delete_apply_cont term =
  match Term.get term with
  | Apply_cont(k,x) ->
    Cont_var.Occur.delete k;
    Var.Occur.delete x;
    Empty.empty term
  | _ -> assert false
;;

let delete_halt term =
  match Term.get term with
  | Halt(x) ->
    Var.Occur.delete x;
    Empty.empty term
  | _ -> assert false
;;

(* Replace a term with its body. Body must be the immediate subterm of
   term. *)
let replace_with_body term body =
  Cpscheck.Uplinks.one_term body;
  (* Note: we cannot use the safe operation [Cpscheck.And.set_term]
     here; we temporarily violate the invariant untill we call
     [update_uplinks]. *)
  Term.set term (Term.get body);
  update_uplinks term;
  Cpscheck.Uplinks.one_term term;
  Term.discard body
;;


let delete_let_prim term =
  match Term.get term with
  | Let_prim(var,prim,body) ->
    (* TODO: recursively delete prim. *)
    failwith "Deletion of `prim' not yet handled";
    (assert (Var.Var.occurrence_number var == Var.Var.No_occurrence);
     replace_with_body term body)
  | _ -> assert false;;

let delete_let_cont term =
  match Term.get term with
  | Let_cont(k,_,cont,body) ->
    (* TODO: recursively delete cont. *)
    failwith "Deletion of `cont' not yet handled";
    (assert (Cont_var.Var.occurrence_number k == Cont_var.Var.No_occurrence);
     replace_with_body term body)
  | _ -> assert false
;;

(* TODO: Change [function_type] and arguments separately?  *)
let update_function_type_and_arguments term ft new_args =
  match Term.get term with
  | Let_prim(x,Value(Lambda(_,k,old_args,lambda_body)),body) ->
    let old_set = List.fold_left (fun set elt -> Var.Var.Set.add elt set) Var.Var.Set.empty old_args in
    let new_set = List.fold_left (fun set elt -> Var.Var.Set.add elt set) Var.Var.Set.empty new_args in
    let removed = Var.Var.Set.diff old_set new_set in
    let created = Var.Var.Set.diff new_set old_set in

    let enclosing = Enclosing_term term in

    (* Sets the enclosing of created variables. The variables must be fresh,
    created with a [with_var_in_term] (this is checked by init, that
    throws an exception if the variable is not fresh). *)
    Var.Var.Set.iter (fun v -> Var.Var.init v enclosing) created;

    (* Checks that removed variables have no occurrences left. *)
    Var.Var.Set.iter
      (fun v -> assert ((Var.Var.occurrence_number v) == Var.Var.No_occurrence)) removed;

    Cpscheck.And.set_term term
      (Let_prim(x,Value(Lambda(ft,k,new_args,lambda_body)),body))
  | _ -> assert(false)

(****************************************************************)
(*s Occurrence-replacement functions.  *)

(* [replace_all_occurrences] is fast thanks to the [Var] data
   structure. *)
let replace_all_occurrences v_old v_new =
  Var.Var.replace_with v_old v_new;;

(* For each variable, the f function should either return None (if it
   must not be changed), or Some(variable) (if it must be replaced by
   the variable).

   We replace the occurrences by replacing the [term_] that links to
   them; this is because [term_]s are immutable.

   Also, we cannot just change the occurrence so that it points to the
   new variable instead of the old one). Indeed the occurrence is part
   of a union-find data structure, and contains other informations
   such as unique occurrence id for the variable, and changing the
   pointer would mess with these data structures. Instead, we delete
   the occurrence and create a new one. *)
let replace_some_occurrences_in_one_term t f_ =

  let f occ = f_ (Var.Occur.binding_variable occ) in

  let choose o n = match n with
    | None -> o
    | Some(thing) -> Var.Occur.delete o; Var.Occur.make thing in

  (* We keep the [term], and replace only the [term_] (if needed), to
     preserve uplinks. *)
  match Term.get t with
    | Apply(ft,func,k,args) ->
      let (all_none,fargs) = List.fold_right
        (fun x (curc,curl) ->
          let res = f x in
          match res with
          | None -> (curc,res::curl)
          | _ -> (false, res::curl)) args (true,[]) in
      let newfunc = f func in
      if (newfunc == None && all_none)
      then ()
      else Cpscheck.And.set_term t (Apply(ft,choose func newfunc, k, List.map2 choose args fargs))
    | Apply_cont(k,arg) ->
      (match f arg with
      | None -> ()
      | newarg -> Cpscheck.And.set_term t (Apply_cont(k, choose arg newarg)))
    | Let_prim(x,p,body) -> begin
      (* [newp] is None if there is no need to change the term, and
         [Some(np)] if [p] in [t] must be changed to [np]. *)
      let newp =
        (match p with
        | Integer_binary_op(op,a,b) ->
          (match (f a,f b) with
          | (None,None) -> None
          | (newa,newb) -> Some (Integer_binary_op(op,choose a newa, choose b newb)))
	| Integer_comparison(pred,a,b) ->
          (match (f a,f b) with
          | (None,None) -> None
          | (newa,newb) -> Some (Integer_comparison(pred,choose a newa, choose b newb)))
        | Projection(i, occ) ->
          (match f occ with
          | None -> None
          | newocc -> Some (Projection(i, choose occ newocc)))
        | Value v ->
          (match v with
          | Constant(_) -> None
          | Lambda(_,_,_,_) -> None
          | Tuple(l) ->
            let newl = List.map f l in
            if List.for_all (fun x -> x == None) newl
            then None
            else Some( Value (Tuple (List.map2 choose l newl)))
          | Injection(i,j, occ) ->
            (match f occ with
            | None -> None
            | newocc -> Some ( Value (Injection(i, j, choose occ newocc))))
          )) in
      (match newp with
      | None -> ()
      | Some(np) -> Cpscheck.And.set_term t (Let_prim(x,np,body)))
    end
    | Let_cont(k,x,term,body) -> ()
    | Case(occ,cases,default) ->
      (match f occ with
      | None -> ()
      | newocc -> Cpscheck.And.set_term t (Case(choose occ newocc, cases, default)))

;;

let replace_some_occurrences term f =
  Cpstraverse.iter_on_terms ~enter_lambdas:true term (fun t -> replace_some_occurrences_in_one_term t f);;
