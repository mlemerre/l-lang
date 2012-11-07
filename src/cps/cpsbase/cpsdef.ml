(* Copyright 2012 Matthieu Lemerre.  *)
(* Cpsdef: definitions of data types for CPS representation. *)

(*s These definitions originates from the "compiling with
  continuations, continued" paper, by Andrew Kennedy (we currently
  use the simplified, non-graph version).

  CPS (for continuation passing style) puts constraints on functional
  programs so that a function $f$ never returns; instead it is passed
  a continuation $k$, which is a function that represents what is
  executed on $f$ has finished its execution. So instead of returning
  a value $x$, $f$ "returns" by calling $k(x)$. CPS style makes
  returning from functions, and more generally control flow,
  explicit, at the expense or more verbosity.

  This file presents a particular representation of CPS terms that
  separates continuations, calling a continuations, variables holding
  continations from respectively normal functions, normal function
  calls, and normal variables. This distinction allows to compile the
  CPS program using a stack (see the [Cpsllvm] module for an
  implementation of that). 

  The representation also forces all values (including constants such
  as integers) to be held in variables, which simplify later
  transformation algorithms.

  Finally, we implement mutable links between variables, the terms
  that use and bind a variable, and between a term and its enclosing
  parent. This allows efficient transformation of CPS terms.*)

(* \subsection*{Term representation} 

   First we present the logical structure of terms. *)
(*s Following module [Cpsvar], we have different structures for
  variables (to used where the variable is bound) from occurrence of a
  variable (to use where a variable is used). 

  Note that this module makes variables and continuation variables
  unique, and thus avoid any need for alpha conversion. *)
type var = (var_desc,occur_desc) Cpsvar.variable
and occur =  (var_desc,occur_desc) Cpsvar.occurrence
and cont_var = (cont_var_desc,cont_occur_desc) Cpsvar.variable
and cont_occur =  (cont_var_desc,cont_occur_desc) Cpsvar.occurrence

(*s The representation of CPS terms separates continuations from usual
  functions. The various terms are:

  \begin{itemize}
  \item ${\bf let} x = primitive; body$ creates a binding to a primitive
  value, or to the result of a primitive operation (to be used in
  body)

  \item ${\bf let} k(x) = t; body $ creates a binding to a
  continuation $k$. $x$ is bound in $t$, but not in $body$. The $k$
  continuation variable is bound both in $body$ and $t$ (this allows
  loops).

  \item $k(x)$ calls the \emph{continuation} $k$ with $x$. It can be
  seen as a "jump with argument $x$"

  \item $v(k,x)$ calls the \emph{function} $v$, $k$ being the return
  continuation, and $x$ a parameter. $v$ does not return; instead it
  will call $k$ with the "return value" as a parameter.

  \item $halt(x)$ is used only as a base case, to stop induction. Its
  semantics is that it returns the value [x], which is the result of
  the computation, to the caller. \end{itemize} *)
and term_ = 
  | Let_prim of var * primitive *  term
  | Let_cont of cont_var * var * term * term
  | Apply_cont of cont_occur * occur
  | Apply of occur * cont_occur * occur
  | Halt of occur

(*s Primitive are values, or operations that return a value. The
  various operations do not take values as parameters (even constants
  such as int), only variables: the representation forces all values
  to be bound in a variable. This allows a uniform treatment that
  helps transformation passes.

  The various operations are:

  \begin{itemize}

  \item $x[i]$ get the $i$th element out of $x$. $x$ is a variable bound to
  a tuple.

  \item $x_1 op x_2$ applies binary op to two arguments.

  \end{itemize}

  Note that there are no primitive that would allow to write ${\bf
  let} x = y$, where $y$ is a variable; thus there cannot be two
  variables that directly share the same value. *)
and primitive =
  | Value of value
  | Projection of occur * int
  | Integer_binary_op of Constant.integer_binary_op * occur * occur

(*s Values are primitive objects, held in normal variables.

  Note that contrary to the source language, we can have Tuple objets
  with lists of 0 or 1 element; they correspond to void * pointer and
  simple pointers. [Void] is different from [Tuple []], and [x] is
  different from [Tuple [x]]. *)
(*i Note: it is unclear now whether it is interesting to put these
  values together in a value data type, or split them directly in the
  "primitive" data structure. It depends on whether we can take
  advantage of the definition of "value", i.e. values need no further
  computations; but this is not truly the case for tuples and
  lambdas, which may require dynamic allocation. i*)
(*i TODO: We should have separate "Lambda" and "Function" primitives
  (that may not be in values, but directly in primitives). The goal
  of lambda-lifting would be to remove all lambda primitives, while
  still allowing to obtain Functions directly from the source code,
  e.g. when interfacing with C code. i*)
and value =
  | Void
  | Constant of Constant.t
  | Tuple of occur list
  | Lambda of cont_var * var *  term

(* \subsection*{Definitions representation}

   This section presents the logical structure of toplevel
   definitions.

   The problem of top-level definitions is that a definition is really
   like a term, except that the variable that it binds is global. This
   introduce some redundancies, in particular concerning handling
   backlinks from terms to their parent terms or definitions. The
   definition structure is thus likely to be heavily changed, or even
   disappear in favor of an extended notion of "top-level term". *)

(*s A [module] is composed of a list of "toplevel definitions". Each
  [toplevel definition] simultaneously defines several objects (this
  allows mutually recursive definitions).

  Note: Alternatively, we could maybe define a module as a list of
  toplevel definitions that are all defined at the same time; this
  would avoid the "list of list". *)
and modul = Module of toplevel list
and toplevel = Top of definition list

(*s A [definition] binds a (global) variable to a value, with some
  [visibility]. Public global variables may be used by other
  modules. Private ones can be used by other definitions of the
  module. Unused ones cannot be used by any code, they are used to
  represent computations used only for their side effects. *)
and definition = visibility * definition_type
and visibility = Public of var | Private of var | Unused

(* Values may either be functions (compiled to code in .text section),
   statically computed and allocated values (put in .data section), or
   dynamic values, computed at initialization time and compiled into
   constructors calls (.ctors and .bss). *)
and definition_type = 
  | Function of var list * term
  | Static_value of value
  | Dynamic_value of term
  | External_value

(* \subsection*{Backlinks and mutability for efficient operations} *)

(*s The [term_] type defines the logical structure of a term, and we
  insert this structure between any two [term_]s, or between a [term_]
  and its [definition]. It implements a mutable double-link between a
  term and its parent, (which can be a term or a top-level
  definition).

  Note that a term has only one parent, and thus can appear only once
  in a tree. *)
and term = { mutable enclosing:enclosing; mutable term:term_}

(* Enclosing are uplinks from expressions to the enclosing expression
   or toplevel definition. [Enclosing_uninitialized] is a temporary
   state, encountered only during CPS term creation or
   transformation. *)
and enclosing =
  | Enclosing_definition of definition
  | Enclosing_term of term
  | Enclosing_uninitialized

(*s For now we have only implemented uplinks from variables to their
  binding sites. We should hade uplinks from occurrences to the site
  that use them, and do the same for continuation variables.  *)
and var_desc = { mutable binding_site_var:enclosing; }
and occur_desc = unit
and cont_var_desc = unit
and cont_occur_desc = unit

(* We also instantiate Cpsvar for variables and continuation
   variables; see [Cpsvar] for details about this module. *)
module Var = Cpsvar.Make(struct
  type variable_description = var_desc
  type occurrence_description = occur_desc
  let var_prefix = "x"
end);;

module Cont_var = Cpsvar.Make(struct
  type variable_description = unit
  type occurrence_description = unit
  let var_prefix = "k"
end);;


