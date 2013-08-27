(* Copyright 2011-2013 Matthieu Lemerre. *)

(* The [Unique] module provides globally unique identifiers.

   It is a functor parametrized by the empty [Nothing] module. This
   allows to have different [Unique] identifiers, that have different
   types. *)

module type Nothing = sig end
module Make(S : Nothing):
sig
  type t

  (* Create a fresh, globally unique value. *)
  val fresh : unit -> t

  (* String representation of the identifier. It is also unique. *)
  val to_string : t -> string

  include Key.S with type t := t

  (* Comparison between unique ids. Fast. *)
  val equal: t -> t -> bool
end
