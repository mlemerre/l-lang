(* Copyright 2012 Matthieu Lemerre *)

open Cpsdef;;

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
  | Apply(_,_,_) | Apply_cont(_,_) | Halt(_) -> ()
  | Let_prim(x,p,body) ->
    Cpscheck.And.set_enclosing body new_enclosing;
    Var.Var.set_binding_site x new_enclosing;
    (match p with
    | Value(Lambda(k,x,body)) ->
      Cpscheck.And.set_enclosing body new_enclosing;
      Var.Var.set_binding_site x new_enclosing
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
  | Apply(f,k,x) ->
    Var.Occur.delete f;
    Cont_var.Occur.delete k;
    Var.Occur.delete x;
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
    | Apply(func,k,arg) ->
      (match (f func, f arg) with
      | (None,None) -> ()
      | (newfunc, newarg) -> Cpscheck.And.set_term t (Apply(choose func newfunc, k, choose arg newarg)))
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
        | Projection(occ, i) ->
          (match f occ with
          | None -> None
          | newocc -> Some (Projection(choose occ newocc,i)))
        | Value v ->
          (match v with
          | Void | Constant(_) -> None
          | Lambda(_,_,_) -> None
          | Tuple(l) ->
            let newl = List.map f l  in
            if List.memq None newl
            then Some( Value (Tuple (List.map2 choose l newl)))
            else None)) in
      (match newp with
      | None -> ()
      | Some(np) -> Cpscheck.And.set_term t (Let_prim(x,np,body)))
    end
    | Let_cont(k,x,term,body) -> ()
;;

let replace_some_occurrences term f =
  Cpstraverse.iter_on_terms ~enter_lambdas:true term (fun t -> replace_some_occurrences_in_one_term t f);;
