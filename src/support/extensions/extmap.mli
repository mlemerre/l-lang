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

(* [foldk] performs a continuation-passing-style folding on maps: it
   passes an accumulator downwards, and then propagates the result
   upwards. In other words, [foldk f x0 map k] computes
   [f x0 key1 val1 (x1 ->
     f x1 key2 val2 (x2 ->
       ...
      f xn keyn valn ( xn+1 ->
        k xn+1)))]. *)
  val foldk: ('a -> key -> 'b -> ('a -> 'c) -> 'c) -> 'a -> 'b t -> ('a -> 'c) -> 'c

  (* [add_map m1 m2] adds m1 into m2 (bindings of m1 replace those of m2). *)
  val add_map: 'a t -> 'a t -> 'a t
end

module Make(Ord: Map.OrderedType): S with type key = Ord.t;;
