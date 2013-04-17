(* Copyright 2012 Matthieu Lemerre *)

(* The functions in this module allows iteration on various elements
   in an CPS term. Using this module is encouraged; in particular it
   allows independence to any further change to the [expression_] data
   structure. *)

open Cpsdef;;

(* This function calls [f] recursively on every expression (order
   unspecified). It traverses [Lambda]s if [enter_lambdas] parameter
   is true. The [f] function can perform changes to the expression given as
   argument, but the function will iterate on the original expressions. *)
val iter_on_expressions: enter_lambdas:bool ->
  expression -> (expression -> unit) -> unit

(* The expression data structure requires that the expression containing the
   binding site of a variable [v] is an ancestor of all the expressions
   containing occurrences of [v]. Thus a depth-first traversal of
   expressions will encounter the binding site of the variable on the way
   down, then all occurrences, then the binding site of the variable
   on the way up.

   [fold_on_variables_and_occurrences] implements such a traversal,
   and calls a different function when a variable is encountered for
   the first time, for each occurrence, and when it is encountered for
   the last time; and similarly for continuation variables. It is a
   fold, meaning that each function can update the "state" of the
   traversal given as an argument.
   
   The next functions are specializations of
   [fold_on_variables_and_occurrences], that either do not take state
   arguments ([iter_*]), or do not call functions on every case. *)
val fold_on_variables_and_occurrences :
  Cpsdef.expression ->
  'a ->
  before_var:('a -> Cpsdef.var -> 'a) ->
  occ:('a -> Cpsdef.occur -> 'a) ->
  after_var:('a -> Cpsdef.var -> 'a) ->
  before_cont_var:('a -> Cpsdef.cont_var -> 'a) ->
  cont_occ:('a -> Cpsdef.cont_occur -> 'a) ->
  after_cont_var:('a -> Cpsdef.cont_var -> 'a) ->
  'a

val fold_on_occurrences :
  Cpsdef.expression ->
  'a -> 
  occ:('a -> Cpsdef.occur -> 'a) -> 
  cont_occ:('a -> Cpsdef.cont_occur -> 'a) ->
  'a

val iter_on_occurrences :
  Cpsdef.expression ->
  occ:(Cpsdef.occur -> unit) ->
  cont_occ:(Cpsdef.cont_occur -> unit) ->
  unit
