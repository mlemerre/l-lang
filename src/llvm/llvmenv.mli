(* Copyright 2013 Matthieu Lemerre *)

(*s All LLVM translation are done using [Llvm.global_context()], and
  in a single Llvm module named [the_module]. *)
val the_module: Llvm.llmodule;;

(* Returns a llvalue according to a their name. *)
val lookup_function: string -> Llvm.llvalue;;
val lookup_global: string -> Llvm.llvalue;;
