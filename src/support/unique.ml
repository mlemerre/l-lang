(* Copyright 2011-2012 Matthieu Lemerre. *)
module type Nothing = sig end;;

module Make(S:Nothing) =
struct
  type t = int;;
  type t_ = t;;

  module Ordered = struct
    type t = t_
    let compare = compare
  end

  module Map = Extensions.Map.Make(Ordered);;
  module Set = Set.Make(Ordered);;

  let to_string i = string_of_int i;;

  let compare = compare;;
  let hash = Hashtbl.hash;;
  let equal = (==);;

  module Hashtbl = Hashtbl.Make(struct
    type t = t_
    let equal = equal
    let hash = hash
  end)


  (*c Create a fresh identifier. If this overflows, switch to bigger
    int such as [Int64]. *)
  let fresh =
    let x = ref 0 in
    fun () ->
      if !x == max_int then failwith "Overflow in Unique module"
      else (incr x; !x)
end
