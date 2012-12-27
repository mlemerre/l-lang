(* Copyright 2012 Matthieu Lemerre. *)

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



(*s All  translations are done using [Llvm.global_context()], and in a
   single Llvm module named [the_module]. *)
val the_module : Llvm.llmodule

(*s [build_nodef name expr] builds an [expr], an expression in CPS form
   that is not part of a function, (for instance if it was typed in the
   interactive prompt). It is translated to a Llvm function that take
   no argument, named [name]. *)
val build_nodef : Cps.term -> Llvm.llvalue Cps.Var_Map.t -> Llvm.llvalue

val build_toplevel: Cps.toplevel -> Llvm.llvalue Cps.Var_Map.t -> (Llvm.llvalue * Llvm.llvalue Cps.Var_Map.t)

(*i val build_function :
  string ->
  Cps.ContVarMap.key ->
  Cps.VarMap.key -> (int, int) Cps.term -> Llvm.llvalue i*)

