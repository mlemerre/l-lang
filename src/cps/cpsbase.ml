(* Copyright 2012 Matthieu Lemerre.  *)

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
*)

(*s We define variables and continuation variables a unique, to avoid
  any need for alpha conversion. *)
module UniqueCPSVarId = Unique.Make(struct end);;
module UniqueCPSContVarId = Unique.Make(struct end);;

type var = Var of UniqueCPSVarId.t
type contvar = ContVar of UniqueCPSContVarId.t;;

(* Many algorithms use sets and maps of variables and continuation
   variables. *)
module VarMap = Map.Make(struct
  type t = var
  let compare = compare
end)

module VarSet = Set.Make(struct
  type t = var
  let compare = compare
end)

module ContVarMap = Map.Make(struct
  type t = contvar
  let compare = compare
end)

module ContVarSet = Set.Make(struct
  type t =  contvar
  let compare = compare
end)

(*s Values are primitive objects, held in continuation variables.  *) 
type value = 
  | Void 
  | Constant of Constant.t
  (*i XXX: Is tuple a primitive object? Or should it be part of let_primitive?, with other primitive operations?  i*)
| Tuple of var list 
  | Lambda of contvar * var *  term 

(*s The representation of CPS terms separates continuations from usual
  functions. The various terms are:

  \begin{itemize}
  \item ${\bf let} x = value; body$ creates a binding to a primitive
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
and term = 
  (*i XXX: value should probably be one of the primitive operations.
    But the differents values should be put together as "values", to
    allow constant propagation. Maybe that already defined functions
    should go there too.
    i*)
| Let_value of var * value *  term
  | Let_primop of var * primitive_operation *  term
  | Let_cont of contvar * var * term * term
  | Apply_cont of contvar * var
  | Apply of var * contvar * var
  | Halt of var


(*s Primitive operations return a value. The various operations do not
  take values as parameters (even constants such as int), only
  variables: the representation forces all values to be bound in a
  variable. This allows a uniform treatment that helps transformation
  passes.

  The various operations are:

  \begin{itemize}

  \item $x[i]$ get the $i$th element out of $x$. $x$ is a variable bound to
  a tuple.

  \item $x_1 op x_2$ applies binary op to two arguments.

  \end{itemize}

  Note that there are no primitive that would allow to write ${\bf
  let} x = y$, where $y$ is a variable; thus there cannot be two
  variables that directly share the same value.
*)
and primitive_operation = 
  | Projection of var * int
  | Integer_binary_op of Constant.integer_binary_op * var * var

