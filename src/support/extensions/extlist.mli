(* Copyright 2013 Matthieu Lemerre.  *)

(* The [*_with_index] functions are like their counterparts without
   the [_with_index] suffix, except that the function argument takes
   an extra [index] argument, that starts from [start_index] (default
   to 0) and is incremented at each call to [f]. *)
val fold_left_with_index:
  ('a -> 'b -> int -> 'a)  -> 'a -> ?start_index:int -> 'b list -> 'a
val iter_with_index:
  ('a -> int -> unit) -> ?start_index:int -> 'a list -> unit
val map_with_index:
  ('a -> int -> 'b) -> ?start_index:int -> 'a list -> 'b list

(* [foldk] performs a continuation-passing-style folding on lists: it
   passes an accumulator downwards, and then propagates the result
   upwards. In other words, [foldk f init [b1;...;bn] k] computes
   [f init b1 (x1 -> f x2 b2 (x3 -> ... f xn bn ( xn+1 -> k xn+1)))]. *)
val foldk : ('a -> 'b -> ('a -> 'c) ->'c) -> 'a -> 'b list -> ('a -> 'c) -> 'c

(* [first l] returns the last element in l. l must be non-empty. *)
val last: 'a list -> 'a

(* Map on a list, but preserve sharing as much as possible (if the
   provided function returns the same argument for a suffix of the
   list, reuse that suffix. *)
val subst: ('a -> 'a) -> 'a list -> 'a list

(* Perform a map and filter at the same time. *)
val map_filter: ('a -> 'b option) -> 'a list  -> 'b list;;
