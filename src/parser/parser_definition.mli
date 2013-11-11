(* Copyright 2013 Matthieu Lemerre *)

(* The parser transforms the stream of tokens coming from the lexing
   pass to a stream of parse trees representing the sequence of
   definitions in the L file. *)
val definition_stream: Token.Stream.t -> Parsetree.term Stream.t
