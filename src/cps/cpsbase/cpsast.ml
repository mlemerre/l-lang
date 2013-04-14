(* Copyright 2012 Matthieu Lemerre. *)

(*s This file presents the terms, i.e. the abstract syntax, of the CPS
  intermediate language. These definitions originates from the
  "compiling with continuations, continued" paper, by Andrew Kennedy.

  CPS (for continuation passing style) puts constraints on functional
  programs so that a function $f$ never returns; instead it is passed
  a continuation $k$, which is a function that represents what is
  executed on $f$ has finished its execution. So instead of returning
  a value $x$, $f$ "returns" by calling $k(x)$. CPS style makes
  returning from functions, and more generally control flow,
  explicit, at the expense of more verbosity.

  This file presents a particular representation of CPS terms that
  separates continuations, calling a continuations, variables holding
  continations from (respectively) normal functions, normal function
  calls, and normal variables. This distinction allows to compile the
  CPS program using a stack (see the [Cpsllvm] module for an
  implementation of that).

  The representation also forces all values (including constants such
  as integers) to be held in variables, which simplify later
  transformation algorithms. *)

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

    \item $v(k,xl)$ calls the \emph{function} $v$, $k$ being the
    return continuation, and $xl$ a list of parameters. $v$ does not
    return; instead it will call $k$ with the "return values" as
    parameters.

    \item ${\bf case}(x)\{ $i_1 \to k_1 \ldots $i_n \to k_n _ \to
    kdefault \}$ extracts the tag $i$ and value $v$ from $x$; then
    compares $i$ to all the $i_1 \ldots i_n$, and if equal calls the
    corresponding continuation $k$ with argument $v$. If no suitable
    $i$ is found, a default must have been provided, and $kdefault$ is
    called with argument $x$.
    Note: [Apply_cont] can be seen as a special case of [Case] with a
    default argument and no pattern rule; and could be removed.

    \item $halt(x)$ is used only as a base case, to stop induction. Its
    semantics is that it returns the value [x], which is the result of
    the computation, to the caller.
    \end{itemize} *)
  type term;;
  type term_ =
  | Let_prim of var * primitive *  term
  | Let_cont of cont_var * var * term * term
  | Apply_cont of cont_occur * occur
  | Apply of function_type * occur * cont_occur * occur list
  | Case of occur * (int * cont_occur) list * cont_occur option
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

     \item $x_1 op x_2$ applies a binary operation to two arguments.

     \item $x_i pred x_2$ applies a binary predicate to two arguments.

     \end{itemize}

     Note that there are no primitive that would allow to write ${\bf
     let} x = y$, where $y$ is a variable; thus there cannot be two
     variables that directly share the same value. *)
  and primitive =
  | Value of value
  | Projection of int * occur
  | Integer_binary_operation of Constant.Ibop.t * occur * occur
  | Integer_binary_predicate of Constant.Ibpred.t * occur * occur

  (* Values are completely evaluated objects. They are separated from
     primitives only because they can be evaluated statically and put
     in a read-only data section.

     \begin{itemize}

     \item [Constant] is self-explinatory;

     \item $t = (x_0,...,x_{n-1})$ corresponds to a contiguous zone of
     memory; each field $i$ is accessible using $t[i]$ ([Proj]). The
     reason why we prefer [Tuple] over using chains of [Pair], is
     [Pair] cannot be compiled to a contiguous zone of memory (without
     indirection), without prior transformation to [Tuple].

     \item $inj_{i/j}(x)$ creates a variant value, with tag number $i$
     (starting from 0) over $j$ possible tags.

     \item $\{ k \to (x_0,...,x_{n-1}) \to body\}$ build a function.
     Functions can have several arguments, because this simplifies
     closure conversion, and allows to express some transformations
     such as arity-raising.

     \item [external( name)] denotes a value defined in another
     compilation unit.

     \end{itemize}
     Note that contrary to the source language, we can have Tuple
     objets with 1 element; they correspond to pointers to an object.
     [x] is different from [Tuple( [x])]. *)
  and value =
  | Constant of Constant.t
  | Tuple of occur list
  | Injection of int * int * occur
  | Lambda of function_type * cont_var * var list *  term
  | External of string


  (* We distinguish closures, that may contain free variables which
     constitutes an environment that need to be stored, from
     functions, who may not. We distinguish them only as an argument
     to [Apply] and [Lambda], because most of the code that access
     them does not depend on this distinction. Having functions at
     the top level is useful, for instance, to interact with C.  *)
  and function_type =
  | Closure
  | No_environment


  (*s We now present the logical structure of toplevel definitions. A
    definition is like a let-binding, except that the variable is
    bound globally (and are thus accessible to all the following
    definitions), instead of just in the body the let-binding. *)
  type definitions = definition list

  (*s A [definition] binds a (global) variable to a value, with some
    [visibility]. [Public] global variables may be used by other
    modules. [Private] ones can be used by other definitions of the
    module. [Unused] ones cannot be used by any code, they are used to
    represent computations used only for their side effects.

    Note that [Unused] cannot be replaced by a check that the variable
    has no occurrences when compiling incrementally: indeed when the
    definition is compiled, the variable may not yet have any
    occurrence. *)
  and definition = Definition of visibility * definition_type
  and visibility = Public of var | Private of var | Unused

  (* Variables can be bound either to statically computed and
     allocated values and functions (put in .data/.rodata/.text
     sections), or dynamic values, computed at initialization time and
     compiled into constructors calls (.ctors and .bss). *)
  (*i TODO: A "staticalization" phase will transform dynamic values into
      static ones. i*)
  and definition_type =
  | Static_value of value
  | Dynamic_value of term

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
