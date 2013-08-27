(* Copyright 2013 Matthieu Lemerre. *)

(* This module allows to "intern", i.e. "hashcons", i.e. giving to
   each string a unique memory representation. This unique
   representation, created with [intern], is suitable for use as a
   key. *)


module Make(N:sig end):sig
  (* A unique representative of a string, created with intern. *)
  type t

  (* Returns the unique representative for the string. *)
  val intern : string -> t

  (* Returns the string associated to that representative. *)
  val to_string : t -> string

  include Key.S with type t := t
end
;;
