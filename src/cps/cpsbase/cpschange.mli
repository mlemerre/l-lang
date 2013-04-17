(* Copyright 2012 Matthieu Lemerre *)

(* The [Change] module provide high-level functions to perform
   (in-place) changes to terms, i.e. expressions and definitions. It
   should be used by any CPS transformation pass.

   The canonical way to \emph{insert} code in a term is to:

   \begin{itemize}

   \item First locate the expression [te where code should be
   inserted; i.e. the transformation should be logically replacing [e]
   by some [f(e)].

   \item Use the [disconnect] function on [e]. This separates the
   [expression] of [e] from its [expression_]; the [expression]
   becomes empty, and is used as a pointer to where the new code
   should be inserted; the [expression_] becomes fresh, and must be
   reused once by the new code.

   \item Use the [Build] module to create new code, using the fresh
   expression, and insert the new code at the right place using the
   [reconnect] argument.

   \end{itemize}

   The canonical way to \emph{delete} code from an expression is to use the
   \emph{delete} functions.

   Notes:
   \begin{itemize}

   \item Deleting leaf expressions produce an empty expression, that
   can also be used for reconnection.

   \item A expression cannot be used at several places in a tree (sharing is
   forbidden).

   \item There is a lower-level function, called [update_uplinks],
   that can reconnect a fresh expression with an empty expression without using
   the [reconnect] argument of the [Build] function. However
   reconnection using [Build] is preferable, and there is no need to
   provide access to the [update_uplinks] function, at least for now.
   This would provide an alternative way to reconnect an expression.

   \item Functions that traverse the terms must not be used when a
   term may contain an empty expression; to avoid any error it is
   preferrable to reconnect empty expressions as soon as possible
   (e.g. disconnect and reconnect the expression in the same
   function).

   \end{itemize} *)

open Cpsdef;;

type fresh = Fresh.t;;

(* [disconnect e] removes the link between an expression [e] and its
   [expression_], i.e. 1. makes [e] empty, and 2. creates a fresh
   expression that contains the [expression_]. *)
val disconnect : expression -> Empty.t * fresh

(* The following functions allows to delete a part of the expression. The
   behavior is different for interior and leaf expressions:

   \begin{itemize}

   \item For an interior expression [t] ([let x = {prim|cont} in
   body]), the function disconnects [t], delete it, and reconnects
   [body] where [t] was. The function returns [unit]. Note that these
   expressions bind a variable or continuation variable [x], and [x]
   that must have no occurrences left before deleting the expression.
   The [expression]s in [prim] or [cont] are recursively deleted.

   \item For a leaf expression ([halt x, k(x), f(k,x)]), [t] is disconnected
   and deleted. The empty expression is returned, and can be used by further
   reconnection.

   \end{itemize}

   Note that deletion of an expression includes deletion of the
   occurrences used in the expression. *)
val delete_let_prim: expression -> unit
val delete_let_cont: expression -> unit
val delete_apply: expression -> Empty.t
val delete_apply_cont: expression -> Empty.t
val delete_halt: expression -> Empty.t

(* [update_function_type_and_arguments t ft l], when t = "[let x =
   Lambda(old_ft,k,old_l,body)]" replaces [old_ft] and [old_l] with
   [ft] and [l], without changing the other arguments. (If [body] must
   be changed, one just need to call [disconnect body]).

   Arguments in [old_l] and not in [l] must not have any remaining
   occurrences. Arguments in [l] and not in [old_l] must be fresh (no
   enclosing set yet), and will have their enclosing properly set. *)
val update_function_type_and_arguments:
  expression -> function_type -> var list -> unit


(* [replace_all_non_recursive_occurrences_of_with v_old v_new] makes
   all non_recursive occurrences of [v_old] become occurrences of
   [v_new]. Takes amortized quasi-constant time. *)
val replace_all_non_recursive_occurrences_of_with: var -> var -> unit

(* [replace_some_occurrences f t] replaces all occurrences in subexpressions
   of [t] according to the [f] function. If [v] is the binding
   variable of an occurrence [o], [o] is not changed if [f v] returns
   None; if [f v] returns [Some(r)], [o] is deleted and replaced by an
   occurrence of [x]. [replace_some_occurrences] traverses [Lambda]
   expressions. This operation takes time proportional to the size of
   the expression.*)
val replace_some_occurrences: expression -> (var -> occur_maker option) -> unit

(* [replace_some_occurrences f expression] is just
   [iter_on_expressions expression
   (replace_some_occurrences_in_one_expression f)]. We provide the
   [replace_some_occurrences_in_one_expression] function so that the
   traversal of subexpressions can be combined with other
   transformations (so as to combine several transformation passes
   into one); or if traversal of expressions inside [Lambda]s is not
   needed. *)
val replace_some_occurrences_in_one_expression:
  expression -> (var -> occur_maker option) -> unit
