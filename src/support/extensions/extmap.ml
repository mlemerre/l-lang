(* Copyright 2013 Matthieu Lemerre.  *)

module type S = sig
  include Map.S;;
  val keys: 'a t -> key list
  val values: 'a t -> 'a list
  val add_unless_mem: key -> 'a -> 'a t -> 'a t
  val foldk: ('a -> key -> 'b -> ('a -> 'c) -> 'c) -> 'a -> 'b t -> ('a -> 'c) -> 'c
  val add_map: 'a t -> 'a t -> 'a t
end

module Make(Ord: Map.OrderedType): S with type key = Ord.t = struct
  module Orig = Map.Make(Ord);;
  include Orig;;

  let keys t = List.map fst (Orig.bindings t)
  let values t = List.map snd (Orig.bindings t)

  let add_unless_mem key value map =
    if Orig.mem key map
    then map
    else Orig.add key value map

  let foldk f init map k =
    let bindings = Orig.bindings map in
    let f_for_list accu (key,value) g = f accu key value g in
    Extlist.foldk f_for_list init bindings k

  let add_map m1 m2 = Orig.fold Orig.add m1 m2

end;;
