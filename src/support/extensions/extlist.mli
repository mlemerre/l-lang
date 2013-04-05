(* Copyright 2013 Matthieu Lemerre.  *)

(* [List.fold_left_with_index f init l] does like [List.fold_left f
   init l], except that [f] takes an extra [index] argument, that
   starts from [start_index] (default to 0) and is incremented at each
   call. Tail-recursive. *)
val fold_left_with_index: ('a -> 'b -> int -> 'a)
  -> 'a -> ?start_index:int -> 'b list -> 'a

(* [List.iter_with_index f l] does like [List.iter f init l], except
   that [f] takes an extra [index] argument, that starts from
   [start_index] (default to 0) and is incremented at each call.
   Tail-recursive. *)
val iter_with_index: ('a -> int -> unit) -> ?start_index:int -> 'a list -> unit
