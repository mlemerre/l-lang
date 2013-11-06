(* Copyright 2013 Matthieu Lemerre *)

(* Main.ml: Entry point to the compiler and command-line processing. *)

if Sys.argv.(1) = "--fixed"
then 
  begin
    (* Fixed entries. Useful for debugging with camldebug. *)
    Compilation_passes.process_file "../tests/test_current.l";
    List.iter Compilation_passes.process_file Asttest.files
  end
else if Sys.argv.(1) = "--pretty"
  then Pretty_print.print_html Sys.argv.(2)
else
  let file = Sys.argv.(1) in
  Compilation_passes.process_file file
;;


