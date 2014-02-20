(* Copyright 2013 Matthieu Lemerre *)

(* Parse a module with its path.  *)
val parse_path_allow_type_constr:
  Token.Stream.t -> Parsetree.term

(* An alias for the above, used when we use the module name to access
   the underlying type. *)
val parse_type: Token.Stream.t -> Parsetree.term

(* [parse_path_to parser stream] will parse a path, then call
   [parser]. The language corresponding to [parser] must not contain
   words beginning by the "upper\_id/" or "upper_id<". *)
val parse_path_to:
  (Token.Stream.t -> Parsetree.term) -> Token.Stream.t -> Parsetree.term
