(* Copyright 2012 Matthieu Lemerre. *)

(* This file presents the terms, i.e. the abstract syntax, of the CPS
   intermediate language. *)
module type S = sig


  (*s The representation use separate types for variables (to use
    where the variable is bound) from occurrence of a variable (to use
    where a variable is used). Variables and occurrences are globally
    unique, which avoids any need for alpha conversion. *)
  type var
  type occur
  type cont_var
  type cont_occur

  (*s We distinguish the type [term_], that holds the sum type
    representing the various terms, and the type [term], that also
    holds other information (such as the uplink to the enclosing
    term).

    The representation of CPS terms separates continuations from
    usual functions. The various terms are:

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
    the computation, to the caller.
    \end{itemize} *)
  type term;;
  type term_ =
  | Let_prim of var * primitive *  term
  | Let_cont of cont_var * var * term * term
  | Apply_cont of cont_occur * occur
  | Apply of occur * cont_occur * occur
  | Halt of occur

  (* Primitive are values, or operations that return a value. The
     various operations do not take values as parameters (even
     constants such as int), only variables: the representation forces
     all values to be bound in a variable. This allows a uniform
     treatment that helps transformation passes.

     The various operations are:

     \begin{itemize}

     \item $x[i]$ get the $i$th element out of $x$. $x$ is a variable
     bound to a tuple.

     \item $x_1 op x_2$ applies binary op to two arguments.

     \item $x_i pred x_2$ applies a binary predicate to two arguments.

     \end{itemize}

     Note that there are no primitive that would allow to write ${\bf
     let} x = y$, where $y$ is a variable; thus there cannot be two
     variables that directly share the same value. *)
  and primitive =
  | Value of value
  | Projection of occur * int
  | Integer_binary_op of Constant.integer_binary_op * occur * occur
  | Integer_comparison of Constant.Icmp.predicate * occur * occur

  (* Values are primitive objects, held in normal variables.

     Note that contrary to the source language, we can have Tuple objets
     with lists of 0 or 1 element; they correspond to void * pointer and
     simple pointers. [Void] is different from [Tuple []], and [x] is
     different from [Tuple [x]]. *)
  and value =
  | Void
  | Constant of Constant.t
  | Tuple of occur list
  | Lambda of cont_var * var *  term

  (*s We now present the logical structure of toplevel definitions.
    The main difference with terms is that definitions bind terms
    globally (and are thus accessible to the following definitions.) *)

  (* A [module] is composed of a list of "toplevel definitions". Each
     [toplevel definition] simultaneously defines several objects (this
     allows mutually recursive definitions). *)
  (*i Note: Alternatively, we could maybe define a module as a list of
    toplevel definitions that are all defined at the same time; this
    would avoid the "list of list". i*)
  type modul = Module of toplevel list
  and toplevel = Top of definition list

  (*s A [definition] binds a (global) variable to a value, with some
    [visibility]. Public global variables may be used by other modules.
    Private ones can be used by other definitions of the module. Unused
    ones cannot be used by any code, they are used to represent
    computations used only for their side effects. *)
  and definition = visibility * definition_type
  and visibility = Public of var | Private of var | Unused

  (* Variables can be bound either to functions (compiled to code in
     .text section), statically computed and allocated values (put in
     .data section), or dynamic values, computed at initialization time
     and compiled into constructors calls (.ctors and .bss).

     Note: currently, constants are considered as static values, but
     this may change. *)
  and definition_type =
  | Function of var list * term
  | Static_value of value
  | Dynamic_value of term
  | External_value

  (*s Enclosing are uplinks from any element (variables, occurrences,
    expressions...) to the enclosing expression or toplevel
    definition. They are thus not part of the AST stricto sensu (the
    AST provides only downlinks from expressions to elements).

    [Enclosing_uninitialized] is a temporary state, encountered only
    during CPS term creation or transformation. *)
  type enclosing =
  | Enclosing_definition of definition
  | Enclosing_term of term
  | Enclosing_uninitialized;;
end ;;
