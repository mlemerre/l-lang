(* Copyright 2012 Matthieu Lemerre.  *)

(* This module perform various checks on invariants on the CPS data
   structure. This is useful to checks that code transformation
   functions do not mess (too much) with the data structures.

   These invariants come the fact that there are redundant
   information: for instance, uplinks from a subterm to its enclosing
   term is redundant, because one could obtain this information by
   exhaustive traversal of the tree (of course, this would be slower).
   The invariants state that these redundant informations are in sync;
   and the module allows to check that. 

  The checks that it performs are:
  \begin{description}

  \item[uplink]: Checks that each entity contains the correct uplink.
  There are currently uplinks from subterms to their enclosing
  [Let_prim] or [Let_cont] term, and from binding variables to the
  "binding site" (the term where the variable is bound); the check
  must be completed when more the CPS structure contains more
  uplinks.

  \item[occurrences]: Checks that the doubly linked list of
  occurrences in [Cpsvar], maintained for each variable, is accurate;
  i.e. that all variables in CPS terms are in the doubly linked lists,
  and that all terms in the doubly linked list are in a CPS term. The
  latter can happen, for instance, if one forget to call the
  [Var.Occur.delete] function.

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

  Also the module should be extended to handle not only terms, but
  whole definitions. 
   
  The performed checks can be \emph{complete}, meaning that the whole
  term and all its subterms are checked. On the other hand, the
  [And_set] module allows to perform some local checks together with
  changing a term. *)

module Uplinks: sig
  (* [one_term t] checks that the elements in the [term_] of [t] point
     to [t]. *)
  val one_term: Cpsdef.term -> unit

  (* This function recursively performs a complete check of
     uplinks. *)
  val term: Cpsdef.term -> unit
end;;

(* This module performs a complete check of occurrences. *)
module Occurrences: sig
  val term: Cpsdef.term -> unit
end;;

(* This module provides predicates useful for error checking. They
   return true if, and only if, [term_] immediately contains a
   specific [var], [cont_var], or [subterm].  *)
module Contains : sig
  val var: Cpsdef.term_ -> Cpsdef.var -> bool
  val cont_var: Cpsdef.term_ -> Cpsdef.cont_var -> bool
  val subterm: Cpsdef.term_ -> Cpsdef.term -> bool
end

(* This module implements wrappers around [Cpsdef.Term.set] and
   [Cpsdef.Term.set_enclosing], that checks that the uplinks
   invariants are fulfilled by the modification. They should be
   preferred over their unchecked counterparts. *)
module And: sig
  val fill: Cpsdef.Empty.t -> Cpsdef.term_ -> Cpsdef.term
  val set_term: Cpsdef.term -> Cpsdef.term_ -> unit
  val set_enclosing: Cpsdef.term -> Cpsdef.enclosing -> unit
end;;
