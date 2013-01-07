(* Copyright 2012 Matthieu Lemerre *)

(*s This module provides means to build CPS terms, using an
  easy-to-use higher-order abstract syntax API.

  In particular, this module deals with the problem of mutual
  recursion caused by the presence of uplinks in some elements (from
  the elements to their enclosing terms).

  \paragraph{The [~reconnect] argument}

  The building functions take an additional [~reconnect] argument of
  type [term]; the purpose of this argument is to be replaced by the
  building functions while preserving the up- and down-links.

  When performing a replacement of a term [t] of type [term], we keep
  the [term] structure and change only [t.term] (of type [term_]).
  This preserves the downlinks from enclosing terms, and the uplink
  from [t] to its enclosing term.

  But we must take care that any element (term or variable) that [t]
  encloses points appropriately to [t]. We could first create a new
  [term_] [t'], then change [t.term] to [t'], and then change the
  uplinks of elements in [t'] to point to [t]. Changing the uplinks
  is an O(1) operation, but not very efficient, that can be
  completely avoided if the uplinks to [t] are set directly when
  building the term [t']. This is why the functions in the [Build]
  module provide the [~reconnect] argument. *)
open Cpsdef;;

type fresh = term;;

(*s We create a [Make] functor factorizes the code for variables and
  continuation variables. It provides a function, [with_var], that
  allows creating new variables; [with_var] is used by the [Build]
  functions, but can also be called by the user.

  In addition to the [Var] or [Cont_var] module, and to [set_uplink],
  a function to change the uplink of a variable, the functor takes as
  an argument [var_in_term]. The [with_var] function takes as
  argument a function [f], that when called, returns a new term [t];
  the variable created by [with_var] is supposed to be bound by [t]
  (and not by the subterms of [t]). [var_in_term] allows to check
  that. *)
module type VAR_PARAM = sig
  module Var : Cpsdef.VAR_RW
  val var_in_term: Var.var -> Cpsdef.term -> bool
  val set_uplink: Var.var -> Cpsdef.term -> unit
end

module Var_param = struct
  module Var = Cpsdef.Var
  let var_in_term v t = Cpscheck.Contains.var (Term.get t) v;;
  let set_uplink v t =
    (* Note: set_description checks that var does not yet have a
       description, i.e. that the variable is fresh. *)
    assert ( try ( Var.Var.binding_site v); false with Assert_failure _ -> true);
    Var.Var.init v (Enclosing_term t);;
end;;

module Cont_var_param = struct
  module Var = Cpsdef.Cont_var
  let var_in_term v t = Cpscheck.Contains.cont_var (Term.get t) v;;
  let set_uplink v t = ();;
end


module Make(M:VAR_PARAM) = struct
  (* [with_var] creates a new var, and pass it to [f]. [f] returns a
     term, and [with_var] creates an uplink from the new var to that
     term. The [?var] optional argument allows to reuse an existing
     variable, instead of creating a new one; this variable must not
     be bound elsewhere (and has been created using a call to the
     user-accessible functions [with_var_in_*]). *)
  let with_var ?var f =
    let v = match var with
      | None -> M.Var.Var.make()
      | Some(v) ->v in
    let t = f v in
    assert( M.var_in_term v t);
    M.set_uplink v t;
    t
end

module With_var = Make(Var_param)
let with_var = With_var.with_var;;

let with_n_vars ?vars n f =
  match vars with
  | Some(l) -> assert(List.length l == n); f l
  | None ->
    let rec loop i accu =
      if i == 0
      then f accu
      else with_var (fun var ->
        loop (i-1) (var::accu))
  in loop n [];;

module With_cont_var = Make(Cont_var_param)
let with_cont_var = With_cont_var.with_var;;

(* Similarly to the [with_var] functions, [with_subterm] is used to
   update a fresh term.  *)
let with_subterm fresh f =
  let t = f fresh in
  assert (Cpscheck.Contains.subterm (Term.get t) fresh);
  assert( Fresh.is_fresh fresh);
  Cpscheck.And.set_enclosing fresh (Enclosing_term t);
  t;;

(*s The term building functions are straightforward. Note that we do
  not expose the primitive functions, like [let_prim] or [let_value],
  in the interface, and prefer exposing more specific functions (such
  as [let_constant] or [let_proj]). This makes translation to CPS more
  agnostic to changes in the [Cpsdef.term_] data structure. *)

let let_prim ?reconnect ?var prim fterm =
  with_var ?var (fun v ->
    with_subterm (fterm v) (fun body ->
      Term.make ?reconnect (Let_prim(v,prim,body))))

let let_value ?reconnect ?var value fterm =
  let_prim ?reconnect ?var (Value value) fterm

let let_proj ?reconnect ?var i to_proj fterm =
  let_prim ?reconnect ?var (Projection(i, Var.Occur.make to_proj)) fterm

let let_inj ?reconnect ?var i j  to_inj fterm =
  let_prim ?reconnect ?var (Value (Injection(i,j,Var.Occur.make to_inj))) fterm


let let_pair ?reconnect ?var (x0,x1) fterm =
  let (o0,o1) = (Var.Occur.make x0, Var.Occur.make x1) in
  let_value ?reconnect ?var (Tuple [o0;o1]) fterm;;

(* Note: contrary to the [Ast] level, we do allow tuples with 0 or 1
   element. 0-element tuples correspond to "void" or "undef values"
   (they are pointers that should not be dereferenced), and 1-element
   tuples to simple references (pointers to a single element). *)
let let_tuple ?reconnect ?var l_vars fterm =
  let l_occurs = List.map Var.Occur.make l_vars in
  let_value ?reconnect ?var (Tuple l_occurs) fterm;;

let let_void ?reconnect ?var fterm =
  let_value ?reconnect ?var (Tuple []) fterm

let match_pair ?reconnect ?var0 ?var1 x fterm =
  let_proj ?reconnect ?var:var0 0 x (fun v0 ->
    let_proj ?var:var1 1 x (fun v1 ->
      fterm (v0, v1)))

let match_tuple ?reconnect n x fterm =
  let rec f k vars =
    if k == n then (fterm vars)
    else let_proj (n-1-k) x  ( fun var ->
      f (k+1) (var::vars) )
  in

  if reconnect != None
  (* We use [let_void] just to reconnect, in particular for the
     special case n == 0. The additional term can be removed by later
     cleanup passes, or in the machine code translation. *)
  then let_void ?reconnect (fun _ -> f 0 [])
  else f 0 [];;

let let_constant ?reconnect ?var c fterm =
  let_value ?reconnect ?var (Constant c) fterm

let let_integer_binary_op ?reconnect ?var op a b fterm =
  let (oa, ob) = (Var.Occur.make a, Var.Occur.make b) in
  let_prim ?reconnect ?var (Integer_binary_op(op,oa,ob)) fterm

let let_integer_comparison ?reconnect ?var op a b fterm =
  let (oa, ob) = (Var.Occur.make a, Var.Occur.make b) in
  let_prim ?reconnect ?var (Integer_comparison(op,oa,ob)) fterm


let let_lambda ?reconnect ?lambda_var ?param_var ftermlambda ftermparam =
  with_cont_var ( fun cv ->
    with_var ?var:lambda_var ( fun lambdav ->
      with_var ?var:param_var ( fun paramv ->
        with_subterm (ftermparam lambdav) (fun body_param ->
          with_subterm (ftermlambda (cv,paramv)) (fun body_lambda ->
            Term.make ?reconnect
              (Let_prim(lambdav,
                        Value (Lambda(Closure,cv,[paramv], body_lambda)),
                        body_param)))))));;

let let_function ?reconnect ?fun_var ?cont_arg ?args nb_args fterm_fun ftermparam =
    with_var ?var:fun_var ( fun funv ->
      with_cont_var ?var:cont_arg ( fun cv ->
        with_n_vars ?vars:args nb_args (fun params ->
          with_subterm (ftermparam funv) (fun body_param ->
            with_subterm (fterm_fun (cv,params)) (fun body_fun ->
              Term.make ?reconnect
                (Let_prim(funv,
                          Value (Lambda(No_environment,cv,params, body_fun)),
                          body_param)))))));;

let apply ?reconnect ft f k xl =
  Term.make ?reconnect
    (Apply (ft,
            (Var.Occur.make f),
            (Cont_var.Occur.make k),
            List.map Var.Occur.make xl));;

let apply_closure ?reconnect f k xl = apply Closure ?reconnect f k xl
let apply_function ?reconnect f k xl = apply No_environment ?reconnect f k xl

let let_cont ?reconnect fterm1 fterm2 =
  with_cont_var ( fun cv ->
    with_var( fun v ->
      with_subterm (fterm1 v) (fun t1 ->
        with_subterm (fterm2 cv) (fun t2 ->
          Term.make ?reconnect (Let_cont(cv, v, t1, t2 ))))));;

let apply_cont ?reconnect k x =
  Term.make ?reconnect
    (Apply_cont ((Cont_var.Occur.make k),
                 (Var.Occur.make x)));;

let case ?reconnect ?default x l =
  let f t = Term.make ?reconnect
    (Case ((Var.Occur.make x),
           (List.map (fun (i,k) -> (i,Cont_var.Occur.make k)) l),
           t)) in
  match default with
  | None -> f default
  | Some(t) -> with_subterm t (fun _ -> f default)
;;

let halt ?reconnect x =
  Term.make ?reconnect (Halt (Var.Occur.make x));;

(*s The user-accessible [with_var_in*] Functions differ from
  [with_var], in that:

  \begin{enumerate}

  \item They do not need the [?var] optional argument;

  \item As the uplink is already set with the final [with_var], there
  is no need to set it again here. We just check that the var has been
  used in f.

  \end{enumerate} *)
let with_var_in_term f =
  let v = Var.Var.make() in
  let t = f v in
  (* If the variable has no binding site (e.g. it has not been given
  as an argument to with_var), it will raise an exception. *)
  assert ( try ( Var.Var.binding_site v); true with Assert_failure _ -> false);
  t;;

let with_cont_var_in_term f =
  let v = Cont_var.Var.make() in
  let t = f v in
  t;;

let with_var_in_def f =
  let v = Cpsdef.Var.Var.make() in
  let d = f v in
  Cpsdef.Var.Var.init v (Cpsdef.Enclosing_definition d);
  d;;


(*s The definition building functions are also straightforward. *)
let def_constant c =
  with_var_in_def (fun v ->
    Definition(Public v, Static_value(Constant c)))
