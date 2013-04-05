(* Copyright 2012-2013 Matthieu Lemerre. *)

(*s This module translates CPS representation to the LLVM IR. CPS
  terms must observe that

  \begin{itemize}
  \item Functions  do not have free (unbound) variables or continuation
  variables (use closure conversion to get rid of free variables in
  functions)

  \item Constants functions (such as $+,-$) have been $\eta$-expanded,
  and translated to the use of CPS primitive operations.
  \end{itemize}
*)

val build_definition: Cpsbase.definition -> Llvm.llvalue Cpsbase.Var.Var.Map.t -> (Llvm.llvalue * Llvm.llvalue Cpsbase.Var.Var.Map.t)
