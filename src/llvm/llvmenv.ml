(* Copyright 2013 Matthieu Lemerre *)

(* This file allows the compiled module to refer to functions and
   global variables defined in the L library (written in C), for
   compilation as well as for JIT execution.

   TODO: in the future, [env_files] will be given as a parameter to
   the L compiler. *)

(* Everything must be put in a single bytecode file; use llvm-link to
   merge bytecode files. This is because for JIT, Llvm does not allow
   reference to symbols in other modules, so we need to create a
   single module and then extend it. *)
let env_file = "src/libl/libl.bc";;

let file_to_module file =
  let context = Llvm.global_context() in
  try
    let mb = Llvm.MemoryBuffer.of_file file in
    let modu = Llvm_bitreader.parse_bitcode context mb in
    modu
  with _ -> failwith ("File " ^ file ^ " not present")
;;

let the_module = file_to_module env_file;;

let lookup fn name =
  match fn name the_module with
  | Some(x) -> x
  | None -> failwith (name ^ " was not found in environment");;

(* Note: what we would really like is a "lookup_value" function, that
   would work for functions and globals. Unfortunately the LLVM
   bindings do not provide this function. *)
let lookup_function = lookup Llvm.lookup_function;;
let lookup_global = lookup Llvm.lookup_global;;
