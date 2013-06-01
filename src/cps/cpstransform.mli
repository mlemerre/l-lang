(* Copyright 2013 Matthieu Lemerre *)

(* CPS transformation transforms definitions in the AST language to
   definitions in the CPS language. *)
val from_stream: Astdef.Definition.t Stream.t -> Cpsbase.definition Stream.t
