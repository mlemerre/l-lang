(* Copyright 2013 Matthieu Lemerre.  *)

module type S = sig
  include Map.S;;
  val keys: 'a t -> key list
  val values: 'a t -> 'a list
  val add_unless_mem: key -> 'a -> 'a t -> 'a t
  val length: 'a t -> int
end

module Make(Ord: Map.OrderedType): S with type key = Ord.t = struct
  module Orig = Map.Make(Ord);;
  include Orig;;

  let keys t = List.map fst (Orig.bindings t)
  let values t = List.map snd (Orig.bindings t)


  let add_unless_mem key value map =
    if Orig.mem key map
    then Orig.add key value map
    else map

  let length m = Orig.fold (fun _ _ x -> x+1) m 0
end;;

