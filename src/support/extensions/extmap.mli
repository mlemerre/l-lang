(* Copyright 2013 Matthieu Lemerre.  *)

module type S = sig
  include Map.S;;

  (* Get the list of keys of the map (sorted according to [Ord.compare]).  *)
  val keys: 'a t -> key list

  (* Get the list of keys of the map (the order is such that the
     corresponding keys are sorted according to [Ord.compare]). *)
  val values: 'a t -> 'a list

  (* [add_unless_mem key value m] adds the binding [(key,value)] to
     [m], unless [key] is already a member of [m]. *)
  val add_unless_mem: key -> 'a -> 'a t -> 'a t

  (* Get the number of bindings in the map. *)
  val length: 'a t -> int
end

module Make(Ord: Map.OrderedType): S with type key = Ord.t;;
