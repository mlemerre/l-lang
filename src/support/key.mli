(* Copyright 2013 Matthieu Lemerre. *)

(* Module type for elements used as keys in maps or hash tables. *)
module type S = sig
  type t

  (* Same spec than [Pervasives.compare]. *)
  val compare : t -> t -> int

  (* Same spec than [Hashtbl.hash]. *)
  val hash: t -> int

  (* Often-used helper modules.  *)
  module Map : Extensions.Map.S with type key = t
  module Set : Set.S with type elt = t
end
