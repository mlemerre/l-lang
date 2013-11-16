(* Copyright 2013 Matthieu Lemerre *)

(* The result of parsing is a parsetree, formatted using (grounded
   first-order) [term]s. The main advantage of this representation is
   its simplicity; thanks to it, the parser code focus only on
   parsing. 

   Moreover, the locations in the terms define a hierarchical and
   orderred partitionning of the source code (the location of a node
   is the sum of the locations of its siblings, and the locations of
   the siblings do not overlap and are ordered). This property allows
   the term structure to be used for bidirectional correspondance
   between the abstract syntax and the source code. *)
type term = {
  func: func;
  arguments: term list;
  location: Src_location.t;
}

(* The function symbol of a term. [Token] is generally prefered (for
   infix or prefix symbols); [TokenPair] apply to delimited lists;
   [Custom] apply only when there is no suitable token. *)
and func =
| Token of Token.With_info.t
| TokenPair of Token.With_info.t * Token.With_info.t
| Custom of string
;;

(* Print a term. *)
val print: Format.formatter -> term -> unit

(* The parser: from a stream of tokens, produce a stream of terms
   containing the successive definitions of the file. *)
val definition_stream: Token.Stream.t -> term Stream.t
