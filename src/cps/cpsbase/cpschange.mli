(* Copyright 2012 Matthieu Lemerre *)

(* The [Change] module provide high-level functions to perform
   (in-place) changes to terms and definitions. It should be used by
   any CPS transformation pass.

   The canonical way to \emph{insert} code in a term is to:

   \begin{itemize}

   \item First locate the term [t] where code should be inserted; i.e.
   the transformation should be logically replacing [t] by some [f(t)].

   \item Use the [disconnect] function on [t]. This separates the
   [term] of [t] from its [term_]; the [term] becomes empty, and is
   used as a pointer to where the new code should be inserted; the
   [term_] becomes fresh, and must be reused once by the new code.

   \item Use the [Build] module to create new code, using the fresh
   term, and insert the new code at the right place using the
   [reconnect] argument.

   \end{itemize}

   The canonical way to \emph{delete} code from a term is to use the
   \emph{delete} functions.

   Notes:
   \begin{itemize}

   \item Deleting leaf terms produce an empty term, that can also be
   used for reconnection.

   \item A term cannot be used at several places in a tree (sharing is
   forbidden).

   \item There is a lower-level function, called [update_uplinks],
   that can reconnect a fresh term with an empty term without using
   the [reconnect] argument of the [Build] function. However
   reconnection using [Build] is preferrable, and there is no need to
   provide access to the [update_uplinks] function, at least for now.
   This would provide an alternative way to reconnect a term.

   \item Functions that traverse the term must not be used when a term
   contains an empty term; to avoid any error it is preferrable to
   reconnect empty terms as soon as possible (e.g. disconnect and
   reconnect the term in the same function).

   \end{itemize} *)

open Cpsdef;;

type fresh = Fresh.t;;

(* [disconnect t] removes the link between a term [t] and its
   [term_], i.e. 1. makes [t] empty, and 2. creates a fresh term
   that contains the [term_]. *)
val disconnect : term -> Empty.t * fresh

(* The following functions allows to delete a part of the term. The
   behavior is different for interior and leaf terms:

   \begin{itemize}

   \item For an interior term [t] ([let x = {prim|cont} in body]), the
   function disconnects [t], delete it, and reconnects [body] where
   [t] was. The function returns [unit]. Note that these terms bound a
   variable or continuation variable [x], and [x] that must have no
   occurrences left before deleting the term. The [term]s in [prim] or
   [cont] are recursively deleted.

   \item For a leaf term ([halt x, k(x), f(k,x)]), [t] is disconnected
   and deleted. The empty term is returned, and can be used by further
   reconnection.

   \end{itemize}

   Note that deletion of a term includes deletion of the occurrences
   used in the term. *)
val delete_let_prim: term -> unit
val delete_let_cont: term -> unit
val delete_apply: term -> Empty.t
val delete_apply_cont: term -> Empty.t
val delete_halt: term -> Empty.t

(* [update_function_type_and_arguments t ft l], when t = "[let x =
   Lambda(old_ft,k,old_l,body)]" replaces [old_ft] and [old_l] with
   [ft] and [l], without changing the other arguments. (If [body] must
   be changed, one just need to call [disconnect body]).

   Arguments in [old_l] and not in [l] must not have any remaining
   occurrences. Arguments in [l] and not in [old_l] must be fresh (no
   enclosing set yet), and will have their enclosing properly set. *)
val update_function_type_and_arguments: term -> function_type -> var list -> unit


(* [replace_all_occurrences v_old v_new] makes all occurrences of
   [v_old] become occurrences of [v_new]. Takes amortized
   quasi-constant time. *)
val replace_all_occurrences: var -> var -> unit

(* [replace_some_occurrences f t] replaces all occurrences in subterms
   of [t] according to the [f] function. If [v] is the binding
   variable of an occurrence [o], [o] is not changed if [f v] returns
   None; if [f v] returns [Some(r)], [o] is deleted and replaced by an
   occurrence of [x]. [replace_some_occurrences] traverses [Lambda]
   expressions. This operation takes time proportional to the size of
   the term.*)
val replace_some_occurrences: term -> (var -> occur_maker option) -> unit

(* [replace_some_occurrences f term] is just [iter_on_terms term
   (replace_some_occurrences_in_one_term f)]. We provide the
   [replace_some_occurrences_in_one_term] function so that the
   traversal of subterms can be combined with other transformations
   (so as to combine several transformation passes into one); or if
   traversal of terms inside [Lambda]s is not needed. *)
val replace_some_occurrences_in_one_term: term -> (var -> occur_maker option) -> unit
