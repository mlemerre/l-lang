(* Copyright 2013 Matthieu Lemerre *)

(* Additional helper pretty-printing functions. *)
val print_list :
  ?empty:string ->
  ?open_paren:string ->
  ?close_paren:string ->
  ?sep:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

