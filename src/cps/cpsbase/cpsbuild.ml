(* Copyright 2012 Matthieu Lemerre *)

(*s This module provides means to build CPS terms, using an
  easy-to-use higher-order abstract syntax API.

  In particular, this module deals with the problem of mutual
  recursion caused by the presence of uplinks in some elements (from
  the elements to their enclosing expressions).

  \paragraph{The [~reconnect] argument}

  The building functions take an additional [~reconnect] argument of
  type [expression]; the purpose of this argument is to be replaced by the
  building functions while preserving the up- and down-links.

  When performing a replacement of an expression [e] of type
  [expression], we keep the [expression] structure and change only
  [e.expression] (of type [expression_]). This preserves the downlinks
  from enclosing expressions, and the uplink from [e] to its enclosing
  expression.

  But we must take care that any element (expression or variable) that [e]
  encloses points appropriately to [e]. We could first create a new
  [expression_] [e'], then change [e.expression] to [e'], and then change the
  uplinks of elements in [e'] to point to [e]. Changing the uplinks
  is an O(1) operation, but not very efficient, that can be
  completely avoided if the uplinks to [e] are set directly when
  building the expression [e']. This is why the functions in the [Build]
  module provide the [~reconnect] argument. *)
open Cpsdef;;

type fresh = expression;;

(*s We create a [Make] functor factorizes the code for variables and
  continuation variables. It provides a function, [with_var], that
  allows creating new variables; [with_var] is used by the [Build]
  functions, but can also be called by the user.

  In addition to the [Var] or [Cont_var] module, and to [set_uplink],
  a function to change the uplink of a variable, the functor takes as
  an argument [var_in_expression]. The [with_var] function takes as
  argument a function [f], that when called, returns a new expression [e];
  the variable created by [with_var] is supposed to be bound by [e]
  (and not by the subexpressions of [e]). [var_in_expression] allows to check
  that. *)
module type VAR_PARAM = sig
  module Var : Cpsdef.VAR_RW
  val var_in_expression: Var.var -> Cpsdef.expression -> bool
  val set_uplink: Var.var -> Cpsdef.expression -> unit
end

module Var_param = struct
  module Var = Cpsdef.Var
  let var_in_expression v t = Cpscheck.Contains.var (Expression.get t) v;;
  let set_uplink v t =
    (* Note: set_description checks that var does not yet have a
       description, i.e. that the variable is fresh. *)
    assert ( try ( Var.Var.binding_site v); false with Assert_failure _ -> true);
    Var.Var.init v (Enclosing_expression t);;
end;;

module Cont_var_param = struct
  module Var = Cpsdef.Cont_var
  let var_in_expression v t = Cpscheck.Contains.cont_var (Expression.get t) v;;
  let set_uplink v t = ();;
end


module Make(M:VAR_PARAM) = struct
  (* [with_var] creates a new var, and pass it to [f]. [f] returns a
     expression, and [with_var] creates an uplink from the new var to that
     expression. The [?var] optional argument allows to reuse an existing
     variable, instead of creating a new one; this variable must not
     be bound elsewhere (and has been created using a call to the
     user-accessible functions [with_var_in_*]). *)
  let with_var ?var f =
    let v = match var with
      | None -> M.Var.Var.make()
      | Some(v) ->v in
    let t = f v in
    assert( M.var_in_expression v t);
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

(* Similarly to the [with_var] functions, [with_subexpr] is used to
   update a fresh expression.  *)
let with_subexpr fresh f =
  let t = f fresh in
  assert (Cpscheck.Contains.subexpression (Expression.get t) fresh);
  assert( Fresh.is_fresh fresh);
  Cpscheck.And.set_enclosing fresh (Enclosing_expression t);
  t;;

(*s The expression building functions are straightforward. Note that we do
  not expose the primitive functions, like [let_prim] or [let_value],
  in the interface, and prefer exposing more specific functions (such
  as [let_constant] or [let_proj]). This makes translation to CPS more
  agnostic to changes in the [Cpsdef.expression_] data structure. *)

let let_prim ?reconnect ?var prim fexpression =
  with_var ?var (fun v ->
    with_subexpr (fexpression (Var.Occur.maker v)) (fun body ->
      Expression.make ?reconnect (Let_prim(v,prim,body))))

let let_value ?reconnect ?var value fexpression =
  let_prim ?reconnect ?var (Value value) fexpression

let let_proj ?reconnect ?var i to_proj fexpression =
  let_prim ?reconnect ?var (Projection(i, Var.Occur.make to_proj)) fexpression

let let_inj ?reconnect ?var i j  to_inj fexpression =
  let value = Injection(i,j,Var.Occur.make to_inj) in
  let_prim ?reconnect ?var (Value value) fexpression


let let_pair ?reconnect ?var (x0,x1) fexpression =
  let (o0,o1) = (Var.Occur.make x0, Var.Occur.make x1) in
  let_value ?reconnect ?var (Tuple [o0;o1]) fexpression;;

(* Note: contrary to the [Ast] level, we do allow tuples with 0 or 1
   element. 0-element tuples correspond to "void" or "undef values"
   (they are pointers that should not be dereferenced), and 1-element
   tuples to simple references (pointers to a single element). *)
let let_tuple ?reconnect ?var l_vars fexpression =
  let l_occurs = List.map Var.Occur.make l_vars in
  let_value ?reconnect ?var (Tuple l_occurs) fexpression;;

let let_void ?reconnect ?var fexpression =
  let_value ?reconnect ?var (Tuple []) fexpression

let match_pair ?reconnect ?var0 ?var1 x fexpression =
  let_proj ?reconnect ?var:var0 0 x (fun v0 ->
    let_proj ?var:var1 1 x (fun v1 ->
      fexpression (v0, v1)))

let match_tuple ?reconnect n x fexpression =
  let rec f k vars =
    if k == n then (fexpression vars)
    else let_proj (n-1-k) x  ( fun var ->
      f (k+1) (var::vars) )
  in

  if reconnect != None
  (* We use [let_void] just to reconnect, in particular for the
     special case n == 0. The additional expression can be removed by later
     cleanup passes, or in the machine code translation. *)
  then let_void ?reconnect (fun _ -> f 0 [])
  else f 0 [];;

let let_constant ?reconnect ?var c fexpression =
  let_value ?reconnect ?var (Constant c) fexpression

let let_integer_binary_operation ?reconnect ?var op a b fexpression =
  let (oa, ob) = (Var.Occur.make a, Var.Occur.make b) in
  let_prim ?reconnect ?var (Integer_binary_operation(op,oa,ob)) fexpression

let let_integer_binary_predicate ?reconnect ?var op a b fexpression =
  let (oa, ob) = (Var.Occur.make a, Var.Occur.make b) in
  let_prim ?reconnect ?var (Integer_binary_predicate(op,oa,ob)) fexpression

let let_external ?reconnect ?var string fexpression =
  let_value ?reconnect ?var (External string) fexpression


let let_lambda ?reconnect ?lambda_var ?param_var flambda fparam =
  with_cont_var ( fun cv ->
    with_var ?var:lambda_var ( fun lambdav ->
      with_var ?var:param_var ( fun paramv ->
        with_subexpr (fparam (Var.Occur.maker lambdav)) (fun body_param ->
          let body_lambda = flambda (Cont_var.Occur.maker cv,Var.Occur.maker paramv) in
          with_subexpr body_lambda (fun body_lambda ->
            Expression.make ?reconnect
              (Let_prim(lambdav,
                        Value (Lambda(Closure,cv,[paramv], body_lambda)),
                        body_param)))))));;

let let_function ?reconnect ?fun_var ?cont_arg ?args nb_args ffun fparam =
    with_var ?var:fun_var ( fun funv ->
      with_cont_var ?var:cont_arg ( fun cv ->
        with_n_vars ?vars:args nb_args (fun params ->
          with_subexpr (fparam (Var.Occur.maker funv)) (fun body_param ->
            let body_fun = ffun (Cont_var.Occur.maker cv,
                                 List.map Var.Occur.maker params) in
            with_subexpr body_fun (fun body_fun ->
              Expression.make ?reconnect
                (Let_prim(funv,
                          Value (Lambda(No_environment,cv,params, body_fun)),
                          body_param)))))));;

let apply ?reconnect ft f k xl =
  Expression.make ?reconnect
    (Apply (ft,
            (Var.Occur.make f),
            (Cont_var.Occur.make k),
            List.map Var.Occur.make xl));;

let apply_closure ?reconnect f k xl = apply Closure ?reconnect f k xl
let apply_function ?reconnect f k xl = apply No_environment ?reconnect f k xl

let let_cont ?reconnect fexpression1 fexpression2 =
  with_cont_var ( fun cv ->
    with_var( fun v ->
      with_subexpr (fexpression1 (Var.Occur.maker v)) (fun t1 ->
        with_subexpr (fexpression2 (Cont_var.Occur.maker cv)) (fun t2 ->
          Expression.make ?reconnect (Let_cont(cv, v, t1, t2 ))))));;

let apply_cont ?reconnect k x =
  Expression.make ?reconnect
    (Apply_cont ((Cont_var.Occur.make k),
                 (Var.Occur.make x)));;

let case ?reconnect ?default x l =
  let maybe_k_occur = match default with
    | None -> None
    | Some(k) -> Some(Cont_var.Occur.make k) in
  Expression.make ?reconnect
    (Case ((Var.Occur.make x),
           (CaseMap.map Cont_var.Occur.make l),
           maybe_k_occur))
;;

let halt ?reconnect x =
  Expression.make ?reconnect (Halt (Var.Occur.make x));;

let let_match_failure ?reconnect f =
  let_cont (fun x ->
    let_cont ?reconnect (fun res -> halt res (* Never called. *))(fun k ->
      let_external "match_failure" (fun mf ->
        apply No_environment mf k [x]))) f

(*s The user-accessible [with_var_in*] Functions differ from
  [with_var], in that:

  \begin{enumerate}

  \item They do not need the [?var] optional argument;

  \item As the uplink is already set with the final [with_var], there
  is no need to set it again here. We just check that the var has been
  used in f.

  \end{enumerate} *)
let with_var_in_expression f =
  let v = Var.Var.make() in
  let t = f v in
  (* If the variable has no binding site (e.g. it has not been given
  as an argument to with_var), it will raise an exception. *)
  assert ( try ( Var.Var.binding_site v); true with Assert_failure _ -> false);
  t;;

let with_cont_var_in_expression f =
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
    Definition(Public v, Static_value(Constant c)));;
