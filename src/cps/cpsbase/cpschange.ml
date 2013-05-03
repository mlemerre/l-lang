(* Copyright 2012 Matthieu Lemerre *)

open Cpsdef;;

type fresh = Fresh.t

(*s Replace the uplinks in the elements in [expression.expression], to
  point to [expression].

  This is the low-level counterpart to the [disconnect] operation;
  but rather than giving a separate empty and fresh expression, it is
  simple to expect a pre-assembled expression that just needs to be
  updated.

  Note that the purpose of the [reconnect] argument in the [Build]
  module is to avoid this operation. *)
let update_uplinks expression =
  (* Change uplinks of subexpressions and variables to expression. *)
  let new_enclosing = Enclosing_expression expression in
  (match Expression.get expression with
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

(*s Disconnect an expression [e] from its expression_ [e_]; [e] becomes
  [empty] and is returned; a [fresh] expression is created around [e_]
  and is returned. *)
let disconnect expression =
  Cpscheck.Uplinks.one_expression expression;
  let newexpression = Expression.make (Expression.get expression) in
  update_uplinks newexpression;
  Cpscheck.Uplinks.one_expression newexpression;
  (Empty.empty expression,newexpression);;

(****************************************************************)
(*s Deletion functions. Their main role is to remove the occurrences
  that appear in the deleted expressions. *)

let delete_apply expression =
  match Expression.get expression with
  | Apply(_,f,k,ol) ->
    Var.Occur.delete f;
    Cont_var.Occur.delete k;
    List.iter Var.Occur.delete ol;
    Empty.empty expression
  | _ -> assert false
;;

let delete_apply_cont expression =
  match Expression.get expression with
  | Apply_cont(k,x) ->
    Cont_var.Occur.delete k;
    Var.Occur.delete x;
    Empty.empty expression
  | _ -> assert false
;;

let delete_halt expression =
  match Expression.get expression with
  | Halt(x) ->
    Var.Occur.delete x;
    Empty.empty expression
  | _ -> assert false
;;

(* Replace an expression with its body. Body must be the immediate
   subexpression of expression. *)
let replace_with_body expression body =
  Cpscheck.Uplinks.one_expression body;
  (* Note: we cannot use the safe operation [Cpscheck.And.set_expression]
     here; we temporarily violate the invariant untill we call
     [update_uplinks]. *)
  Expression.set expression (Expression.get body);
  update_uplinks expression;
  Cpscheck.Uplinks.one_expression expression;
  Expression.discard body
;;


let delete_let_prim expression =
  match Expression.get expression with
  | Let_prim(var,prim,body) ->
    (* TODO: recursively delete prim. *)
    failwith "Deletion of `prim' not yet handled";
    (assert (Var.Var.number_of_occurrences var == Var.Var.No_occurrence);
     replace_with_body expression body)
  | _ -> assert false;;

let delete_let_cont expression =
  match Expression.get expression with
  | Let_cont(k,_,cont,body) ->
    (* TODO: recursively delete cont. *)
    failwith "Deletion of `cont' not yet handled";
    (assert (Cont_var.Var.number_of_occurrences k == Cont_var.Var.No_occurrence);
     replace_with_body expression body)
  | _ -> assert false
;;

(* TODO: Change [function_type] and arguments separately?  *)
let update_function_type_and_arguments expression ft new_args =
  match Expression.get expression with
  | Let_prim(x,Value(Lambda(_,k,old_args,lambda_body)),body) ->
    let old_set = List.fold_left (fun set elt -> Var.Var.Set.add elt set) Var.Var.Set.empty old_args in
    let new_set = List.fold_left (fun set elt -> Var.Var.Set.add elt set) Var.Var.Set.empty new_args in
    let removed = Var.Var.Set.diff old_set new_set in
    let created = Var.Var.Set.diff new_set old_set in

    let enclosing = Enclosing_expression expression in

    (* Sets the enclosing of created variables. The variables must be fresh,
    created with a [with_var_in_expression] (this is checked by init, that
    throws an exception if the variable is not fresh). *)
    Var.Var.Set.iter (fun v -> Var.Var.init v enclosing) created;

    (* Checks that removed variables have no occurrences left. *)
    Var.Var.Set.iter
      (fun v -> assert ((Var.Var.number_of_occurrences v) == Var.Var.No_occurrence)) removed;

    Cpscheck.And.set_expression expression
      (Let_prim(x,Value(Lambda(ft,k,new_args,lambda_body)),body))
  | _ -> assert(false)

(****************************************************************)
(*s Occurrence-replacement functions.  *)

(* [replace_all_non_recursive_occurrences] is fast thanks to the [Var]
   data structure. *)
let replace_all_non_recursive_occurrences_of_with v_old v_new =
  Var.Var.replace_all_non_recursive_occurrences_of_with v_old v_new;;

(* For each variable, the f function should either return None (if it
   must not be changed), or Some(variable) (if it must be replaced by
   the variable).

   We replace the occurrences by replacing the [expression_] that links to
   them; this is because [expression_]s are immutable.

   Also, we cannot just change the occurrence so that it points to the
   new variable instead of the old one). Indeed the occurrence is part
   of a union-find data structure, and contains other informations
   such as unique occurrence id for the variable, and changing the
   pointer would mess with these data structures. Instead, we delete
   the occurrence and create a new one. *)
let replace_some_occurrences_in_one_expression t f_ =

  let f occ = f_ (Var.Occur.binding_variable occ) in

  let choose o n = match n with
    | None -> o
    | Some(thing) -> Var.Occur.delete o; Var.Occur.make thing in

  (* We keep the [expression], and replace only the [expression_] (if
     needed), to preserve uplinks. *)
  match Expression.get t with
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
      else let newargs = List.map2 choose args fargs in
           let expr_ = Apply(ft,choose func newfunc, k, newargs) in
           Cpscheck.And.set_expression t expr_
    | Apply_cont(k,arg) ->
      (match f arg with
      | None -> ()
      | newarg -> let expr_ = Apply_cont(k, choose arg newarg) in
                  Cpscheck.And.set_expression t expr_)
    | Let_prim(x,p,body) -> begin
      (* [newp] is None if there is no need to change the expression, and
         [Some(np)] if [p] in [t] must be changed to [np]. *)
      let newp =
        (match p with
        | Integer_binary_operation(op,a,b) ->
          (match (f a,f b) with
          | (None,None) -> None
          | (newa,newb) -> Some (Integer_binary_operation(op,choose a newa, choose b newb)))
	| Integer_binary_predicate(pred,a,b) ->
          (match (f a,f b) with
          | (None,None) -> None
          | (newa,newb) -> Some (Integer_binary_predicate(pred,choose a newa, choose b newb)))
        | Projection(i, occ) ->
          (match f occ with
          | None -> None
          | newocc -> Some (Projection(i, choose occ newocc)))
        | Value v ->
          (match v with
          | Constant(_) | External _ -> None
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
      | Some(np) -> let expr_ = Let_prim(x,np,body) in
                    Cpscheck.And.set_expression t expr_)
    end
    | Let_cont(k,x,expression,body) -> ()
    | Case(occ,cases,default) ->
      (match f occ with
      | None -> ()
      | newocc -> let expr_ = Case(choose occ newocc, cases, default) in
                  Cpscheck.And.set_expression t expr_)
    | Halt(occ) ->
      (match f occ with
      | None -> ()
      | newocc -> let expr_ = (Halt (choose occ newocc)) in
                  Cpscheck.And.set_expression t expr_)
;;

let replace_some_occurrences expression f =
  Cpstraverse.iter_on_expressions ~enter_lambdas:true expression (fun t ->
    replace_some_occurrences_in_one_expression t f);;
