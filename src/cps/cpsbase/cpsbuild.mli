(* Copyright 2012 Matthieu Lemerre *)

open Cpsdef;;

(* The functions of this [Build] module are the only provided mean to
   build CPS terms (expressions or definitions).

   These functions must be used following the higher-order abstract
   syntax style, i.e. using OCaml bindings to represent CPS bindings.
   For instance, the expression

   [ let x = 11 in
   halt x ]

   Is built using

   [ Build.let_constant (Constant.Int 11) (fun x ->
   Build.halt x) ]

   This API simplifies the creation of bindings (by avoiding the need to
   explicitly call functions to create bindings, like Lisp's
   [gensym]), and provides some guarantee that the binding cannot
   escape its scope (i.e. in the above [x] can be used only in
   subexpressions of [let_constant]).

   Whenever a function of the Build API "uses" a variable, to create a
   new occurrence, it takes an "[occur_maker = variable * occur_type]"
   as an argument (instead of a variable). The additional [occur_type]
   allows the [Build] function to know if it should created a normal,
   or recursive, occurrence of a variable. This is used especially by
   the CPS transformation algorithm.

   However, instead of automatically creating new variables, the build
   functions can also use an existing variable, using the [~var] named
   parameter. New variables and continuation variables can also be
   explicitely created using [with_var] and [with_cont_var]. Thus the
   above exemple can be rewritten as:

   [ with_var_in_expression (fun x ->
   Build.let_constant ~var:x (Constant.Int 11) (fun x' ->
   Build.halt x)) ]

   And we have [x == x']. This is especially useful when in need to
   act on the occurrences of a newly created variable.

   By default, the functions of the [Build] module create a new, fresh
   expression. However, they can also be used to replace the contents of an
   existing expression [t]: first [t] needs to be disconnected using the
   [Change] module; then it can be reconnected using the [~reconnect]
   argument. For instance, the previous exemple can be changed like
   the following:

   [let Let_prim(_,_,body) = Expression.get t in
   let (dangling_body, body_to_reuse) = Change.disconnect body in
   ignore(
   Build.let_constant ~reconnect:dangling_body (Constant.Int 12) (fun y ->
   body_to_reuse))]

   which has transformed t to:

   [let x = 11 in
   let y = 12 in
   halt x]

   Note: the API automatically handles creation of occurrences of
   variables. It is not possible to "re-use" an occurrence of a
   variable, or pass an occurrence to [Build] functions; you should
   delete the occurrence and let [Build] re-create it instead. *)

type fresh = Fresh.t

(*s Functions used to explicitely create new variables. *)
val with_var_in_expression : (var -> fresh) -> fresh
val with_cont_var_in_expression : (cont_var -> fresh) -> fresh
val with_var_in_def : (var -> definition) -> definition

(*s Functions used to build expressions. *)

(* Usage: [let_constant c (fun var -> ...)] corresponds to
   [let var = c in ...] *)
val let_constant :
  ?reconnect:Empty.t ->
  ?var:var ->
  Constant.t -> (occur_maker -> fresh) -> fresh

(* Usage: [let_void (fun var -> ...)] corresponds to
   [let var = void in ...] *)
val let_void :
  ?reconnect:Empty.t ->
  ?var:var -> (occur_maker -> fresh) -> fresh

(* Usage: [let_external str (fun var -> ...)] corresponds to
   [let var = external(str) in ...] *)
val let_external :
  ?reconnect:Empty.t ->
  ?var:var ->
  string -> (occur_maker -> fresh) -> fresh


(* Usage: [let_proj i tuple (fun var -> ...) corresponds to
   [let var = #i(tuple) in ... ] *)
val let_proj :
  ?reconnect:Empty.t ->
  ?var:var ->
  int -> occur_maker -> (occur_maker -> fresh) -> fresh

(* Usage: [let_inj i j occ (fun var -> ...) corresponds to
   [let var = #i(tuple) in ... ] *)
val let_inj :
  ?reconnect:Empty.t ->
  ?var:var ->
  int -> int -> occur_maker -> (occur_maker -> fresh) -> fresh


(* Usage: [match_pair pair (fun (var0,var1) -> ...)] corresponds to
   [let (var0,var1) = pair in ...], i.e. to
   [let var0 = #0(pair) in
    let var1 = #1(pair) in ... ] *)
val match_pair :
  ?reconnect:Empty.t ->
  ?var0:var ->
  ?var1:var ->
  occur_maker -> (occur_maker * occur_maker -> fresh) -> fresh

(* Usage: [let_pair (var0,var1) (fun var -> ...)] corresponds to
   [let var = (var0,var1) in ...] *)
val let_pair :
  ?reconnect:Empty.t ->
  ?var:var ->
  occur_maker * occur_maker -> (occur_maker -> fresh) -> fresh

(* Usage: [let_tuple [var0;var1;...] (fun var -> ...)] corresponds to
   [let var = (var0,var1,...) in ...]. Note that 0- and 1-tuples are
   allowed. *)
val let_tuple :
  ?reconnect:Empty.t ->
  ?var:var ->
  occur_maker list -> (occur_maker -> fresh) -> fresh

(* Usage: [match_tuple t (fun vars -> ...)] corresponds to
   [let [var0;var1;...] = t in ...], i.e. to
   [let var0 = #0(t) in
    let var1 = #1(t) in ... ] *)
val match_tuple :
  ?reconnect:Empty.t ->
  int -> occur_maker -> (occur_maker list -> fresh) -> fresh

(* Usage: [let_integer_binary_operation op a b (fun var -> ...)] corresponds to
   [let var = a op b in ...] *)
val let_integer_binary_operation :
  ?reconnect:Empty.t ->
  ?var:var ->
  Constant.Ibop.t -> occur_maker -> occur_maker -> (occur_maker -> fresh) -> fresh

(* Usage: [let_integer_binary_predicate predicate a b (fun var -> ...)] corresponds to
   [let var = predicate(a, b) in ...] *)
val let_integer_binary_predicate :
  ?reconnect:Empty.t ->
  ?var:var ->
  Constant.Ibpred.t -> occur_maker -> occur_maker -> (occur_maker -> fresh) -> fresh

(* Usage: [let_lambda (fun (k,x) -> ...) (fun var -> ... )] corresponds to
   [let var = { (k,x) -> ... } in ...] *)
val let_lambda :
  ?reconnect:Empty.t ->
  ?lambda_var:var ->
  ?param_var:var ->
  (cont_occur_maker * occur_maker -> fresh) -> (occur_maker -> fresh) -> fresh


(* Usage: [let_function (fun nb_args (k,[x1,x2,...xn]) -> ...)
   (fun var -> ... )] corresponds to
   [let var = { (k,x1,x2,...,xn) -> ... } in ...] *)
val let_function :
  ?reconnect:Empty.t ->
  ?fun_var:var ->
  ?cont_arg: cont_var ->
  ?args: var list ->
  int ->
  (cont_occur_maker * occur_maker list -> fresh) -> (occur_maker -> fresh) -> fresh

(* Usage: [apply_closure f ft k [x1,...,xn]] corresponds to
   f(k,(x1,...xn)). ft designates the function type: Function or
   Closure. *)
val apply :
  ?reconnect:Empty.t -> function_type ->
  occur_maker -> cont_occur_maker -> occur_maker list -> fresh

(* Same as apply Closure. *)
val apply_closure :
  ?reconnect:Empty.t ->
  occur_maker -> cont_occur_maker -> occur_maker list -> fresh

(* Same as apply Function. *)
val apply_function :
  ?reconnect:Empty.t ->
  occur_maker -> cont_occur_maker -> occur_maker list -> fresh


(* Usage: [let_cont (fun x -> ...) (fun k -> ...)] corresponds to
   [do ... where k(x) = ...]
   TODO: The first argument of [let_cont] should also receive [k], so
   as to allow loops. *)
val let_cont :
  ?reconnect:Empty.t ->
  (occur_maker -> fresh) -> (cont_occur_maker -> fresh) -> fresh

(* Usage: [apply_cont k x] corresponds to k(x) *)
val apply_cont :
  ?reconnect:Empty.t ->
  cont_occur_maker -> occur_maker -> fresh

(* Usage: [case v [(i1,k1);...;(in,kn)]; d] corresponds to
   [case(v) { i1 -> k1 ... in -> kn default -> d }] *)
val case:
  ?reconnect:Empty.t ->
  ?default:cont_occur_maker ->
  occur_maker -> cont_occur_maker case_map -> fresh


(* Usage: [halt x] corresponds to [halt x]. *)
val halt :
  ?reconnect:Empty.t ->
  occur_maker -> fresh

(* Usage: [let_match_failure (fun k -> ...)] creates a continuation k
   that raises a "[Match_failure]" exception. *)
val let_match_failure :
  ?reconnect:Cpsdef.Empty.t ->
  (cont_occur_maker -> fresh) -> fresh

(*s Functions used to build definitions. *)
val def_constant: Constant.t -> definition

(*i TODO:

  - As I did with other modules, [Cpsbuild] should be split into three
  submodules: [Expression], [Value], and [Definition]. [Values] should
  become first-class in this module: this is especially important for
  staticalisation (that means transforming [let_value] expressions
  into [def_value] definitions; and encompass lambda-lifting).
  [let_constant] could be renamed to [Expression.constant]; which is
  the (CPS) composition of [Expression.value] and [Value.constant].

  - The CPS language should have forward references. There is no need
  for a letrec style construct: the advantage of letrec is type
  inference of mutually recursive constructs, but this is useless in
  the CPS language, as type inference is performed in the
  higher-level languages. Forward declaration is more flexible
  (allows to split definitions in several modules), and sufficient.

  i*)
