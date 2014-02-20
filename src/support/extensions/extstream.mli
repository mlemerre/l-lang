(* [transformer f initial_state input_stream] returns an output stream,
   whose elements are computed using the [f] function. [f] takes
   elements of the input stream one at a time, returns 0 or several
   elements, and updates its state. *)
val transformer : ('c -> 'a -> 'b list * 'c) -> 'c -> 'a Stream.t -> 'b Stream.t

(* Same as transformer, but they do not keep any state (this can be useful
   for parallelization). *)
val stateless_transformer : ('a -> 'b list) -> 'a Stream.t -> 'b Stream.t

(* [iter_and_copy f input_stream] returns an output stream with the
   same elements than in [input_stream], but also calls [f] each time
   an element in [input_stream] is computed. [f] may update the
   elements destructively, so one should not retain references on
   elements in [input_stream]. *)
val iter_and_copy : ('a -> unit) -> 'a Stream.t -> 'a Stream.t

val fold: ('a -> 'b -> 'a) -> 'a -> 'b Stream.t -> 'a
val to_list: 'a Stream.t -> 'a list

val sink: 'a Stream.t -> unit
