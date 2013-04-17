(* Copyright 2012 Matthieu Lemerre.  *)

(* This module perform various checks on invariants on the CPS data
   structure. This is useful to checks that code transformation
   functions do not mess (too much) with the data structures.

   These invariants come the fact that there are redundant
   information: for instance, uplinks from a subexpression to its enclosing
   expression is redundant, because one could obtain this information by
   exhaustive traversal of the tree (of course, this would be slower).
   The invariants state that these redundant informations are in sync;
   and the module allows to check that. 

  The checks that it performs are:
  \begin{description}

  \item[uplink]: Checks that each entity contains the correct uplink.
  There are currently uplinks from subexpressions to their enclosing
  [Let_prim] or [Let_cont] expression, and from binding variables to the
  "binding site" (the expression where the variable is bound); the check
  must be completed when more the CPS structure contains more
  uplinks.

   \item[occurrences]: Checks that the doubly linked list of
   occurrences in [Cpsvar], maintained for each variable, is accurate;
   i.e. that all variables in CPS expressions are in the doubly linked
   lists, and that all expressions in the doubly linked list are in a
   CPS expression. The latter can happen, for instance, if one forget
   to call the [Var.Occur.delete] function.

  \end{description}

  There are further checks that could be done; for instance:

  \begin{itemize}

  \item That all variables are bound (i.e. no variable is used before
  it is bound, also stated as "a variable always dominates its
  uses"). Normal use of the [Cpsbuild] function should already
  prevent that.

  \item That all occurrences of a continuation variable is always
  bound in the same [Lambda]; that all occurrences of a variable is
  always bound in the same [Lambda] once closure conversion was
  performed.

  \end{itemize}

  Also the module should be extended to handle not only expressions, but
  whole definitions. 
  
  The performed checks can be \emph{complete}, meaning that the whole
  expression and all its subexpressions are checked. On the other
  hand, the [And_set] module allows to perform some local checks
  together with changing an expression. *)

module Uplinks: sig
  (* [one_expression t] checks that the elements in the [expression_]
     of [t] point to [t]. *)
  val one_expression: Cpsdef.expression -> unit

  (* This function recursively performs a complete check of
     uplinks. *)
  val expression: Cpsdef.expression -> unit
end;;

(* This module performs a complete check of occurrences. *)
module Occurrences: sig
  val expression: Cpsdef.expression -> unit
end;;

(* This module provides predicates useful for error checking. They
   return true if, and only if, [expression_] immediately contains a
   specific [var], [cont_var], or [subexpression].  *)
module Contains : sig
  val var: Cpsdef.expression_ -> Cpsdef.var -> bool
  val cont_var: Cpsdef.expression_ -> Cpsdef.cont_var -> bool
  val subexpression: Cpsdef.expression_ -> Cpsdef.expression -> bool
end

(* This module implements wrappers around [Cpsdef.Expression.set] and
   [Cpsdef.Expression.set_enclosing], that checks that the uplinks
   invariants are fulfilled by the modification. They should be
   preferred over their unchecked counterparts. *)
module And: sig
  val fill: Cpsdef.Empty.t -> Cpsdef.expression_ -> Cpsdef.expression
  val set_expression: Cpsdef.expression -> Cpsdef.expression_ -> unit
  val set_enclosing: Cpsdef.expression -> Cpsdef.enclosing -> unit
end;;
