(* Copyright 2012 Matthieu Lemerre *)

open Cpsdef;;

(* The functions of this [Build] module are the only provided mean to
   build [term]s.

   These functions must be used following the higher-order abstract
   syntax style, i.e. using OCaml bindings to represent CPS bindings.
   For instance, the term

   [ let x = 11 in
   halt x ]

   Is built using

   [ Build.let_constant (Constant.Int 11) (fun x ->
   Build.halt x) ]

   This API simplifies the creation of bindings (by avoiding the need to
   explicitly call functions to create bindings, like Lisp's
   [gensym]), and provides some guarantee that the binding cannot
   escape its scope (i.e. in the above [x] can be used only in
   subterms of [let_constant]).

   However, instead of automatically creating new variables, the build
   functions can also use an existing variable, using the [~var] named
   parameter. New variables and continuation variables can also be
   explicitely created using [with_var] and [with_cont_var]. Thus the
   above exemple can be rewritten as:

   [ with_var (fun x ->
   Build.let_constant ~var:x (Constant.Int 11) (fun x' ->
   Build.halt x)) ]

   And we have [x == x']. This is especially useful when in need to
   act on the occurrences of a newly created variable.

   By default, the functions of the [Build] module create a new, fresh
   term. However, they can also be used to replace the contents of an
   existing term [t]: first [t] needs to be disconnected using the
   [Change] module; then it can be reconnected using the [~reconnect]
   argument. For instance, the previous exemple can be changed like
   the following:

   [let Let_prim(_,_,body) = Term.get t in
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


(* The functions used to explicitely create new variables. *)
val with_var_in_term : (var -> Fresh.t) -> Fresh.t
val with_cont_var_in_term : (cont_var -> Fresh.t) -> Fresh.t
val with_var_in_def : (var -> Cpsdef.definition) -> Cpsdef.definition

(* Usage: [let_constant c (fun var -> ...)] corresponds to
   [let var = c in ...] *)
val let_constant :
  ?reconnect:Empty.t ->
  ?var:var ->
  Constant.t -> (var -> Fresh.t) -> Fresh.t

(* Usage: [let_void (fun var -> ...)] corresponds to
   [let var = void in ...] *)
val let_void :
  ?reconnect:Empty.t ->
  ?var:var -> (var -> Fresh.t) -> Fresh.t

(* Usage: [let_proj tuple i (fun var -> ...) corresponds to
   [let var = #i(tuple) in ... ] *)
val let_proj :
  ?reconnect:Empty.t ->
  ?var:var ->
  var -> int -> (var -> Fresh.t) -> Fresh.t

(* Usage: [match_pair pair (fun (var0,var1) -> ...)] corresponds to
   [let (var0,var1) = pair in ...], i.e. to
   [let var0 = #0(pair) in
    let var1 = #1(pair) in ... ] *)
val match_pair :
  ?reconnect:Empty.t ->
  ?var0:var ->
  ?var1:var ->
  var -> (var * var -> Fresh.t) -> Fresh.t

(* Usage: [let_pair (var0,var1) (fun var -> ...)] corresponds to
   [let var = (var0,var1) in ...] *)
val let_pair :
  ?reconnect:Empty.t ->
  ?var:var ->
  var * var -> (var -> Fresh.t) -> Fresh.t

(* Usage: [let_tuple [var0;var1;...] (fun var -> ...)] corresponds to
   [let var = (var0,var1,...) in ...]. Note that 0- and 1-tuples are
   allowed. *)
val let_tuple :
  ?reconnect:Empty.t ->
  ?var:var ->
  var list -> (var -> Fresh.t) -> Fresh.t

(* Usage: [match_tuple t (fun vars -> ...)] corresponds to
   [let [var0;var1;...] = t in ...], i.e. to
   [let var0 = #0(t) in
    let var1 = #1(t) in ... ] *)
val match_tuple :
  ?reconnect:Empty.t ->
  int -> var -> (var list -> Fresh.t) -> Fresh.t

(* Usage: [let_binary_op op a b (fun var -> ...)] corresponds to
   [let var = a op b in ...] *)
val let_binary_op :
  ?reconnect:Empty.t ->
  ?var:var ->
  Constant.integer_binary_op -> var -> var -> (var -> Fresh.t) -> Fresh.t

(* Usage: [let_integer_comparison predicate a b (fun var -> ...)] corresponds to
   [let var = predicate(a, b) in ...] *)
val let_integer_comparison :
  ?reconnect:Empty.t ->
  ?var:var ->
  Constant.Icmp.predicate -> var -> var -> (var -> Fresh.t) -> Fresh.t



(* Usage: [let_lambda (fun (k,x) -> ...) (fun var -> ... )] corresponds to
   [let var = { (k,x) -> ... } in ...] *)
val let_lambda :
  ?reconnect:Empty.t ->
  ?lambda_var:var ->
  ?param_var:var ->
  (cont_var * var -> Fresh.t) -> (var -> Fresh.t) -> Fresh.t

(* Usage: [apply f k x] corresponds to f(k,x) *)
val apply :
  ?reconnect:Empty.t ->
  var -> cont_var -> var -> Fresh.t

(* Usage: [let_cont (fun x -> ...) (fun k -> ...)] corresponds to
   [do ... where k(x) = ...]
   TODO: The first argument of [let_cont] should also receive [k], so
   as to allow loops. *)
val let_cont :
  ?reconnect:Empty.t ->
  (var -> Fresh.t) -> (cont_var -> Fresh.t) -> Fresh.t

(* Usage: [apply_cont k x] corresponds to k(x) *)
val apply_cont :
  ?reconnect:Empty.t ->
  cont_var -> var -> Fresh.t

(* Usage: [halt x] corresponds to [halt x]. *)
val halt :
  ?reconnect:Empty.t ->
  var -> Fresh.t
