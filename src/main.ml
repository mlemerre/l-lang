(* Copyright 2013 Matthieu Lemerre *)

(* Main.ml: Entry point to the compiler and command-line processing. *)

if true
then 
  begin
    (* Fixed entries. Useful for debugging with camldebug. *)
    Compilation_passes.process_file "../tests/test_current.l";
    List.iter Compilation_passes.process_file Asttest.files
  end
else
  let file = Sys.argv.(1) in
  Compilation_passes.process_file file
;;


