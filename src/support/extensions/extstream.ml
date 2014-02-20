(* Copyright 2013 Matthieu Lemerre *)

let transformer f initial_state input_stream =

  (* The interface of [Stream.from] does not allow explicit states; we
     circumvant this problem using a reference. *)
  let cur_state = ref initial_state in

  (* A buffer of already-produced output elements. When a new element
     is required from the input stream, it is taken from [cur_stack];
     if [cur_stack], then it is filled by calling [f]. *)
  let cur_stack = ref [] in

  (* The function used to create stream elements. It is recursive,
     because [f] can return an empty list. *)
  let rec stream_fun i =
    match !cur_stack with
    | a::b -> cur_stack:=b; Some(a)
    | [] -> match Stream.peek (input_stream) with
      | None -> None
      | Some(input) ->
	Stream.junk input_stream;
	let (defs,new_state) = f !cur_state input in
	cur_state := new_state;
	cur_stack := defs;
        stream_fun i

  in Stream.from stream_fun ;;

let stateless_transformer f stream =
  transformer (fun () input -> (f input, ())) () stream;;

let iter_and_copy f stream =
  Stream.from (fun _ -> match Stream.peek stream with
  | None -> None
  | Some(a) as p -> f a; Stream.junk stream; p)

let rec fold f acc stream =
  match Stream.peek stream with
  | None -> acc
  | Some x -> Stream.junk stream; fold f (f acc x) stream
;;

let to_list stream = List.rev (fold (fun a b -> b::a) [] stream);;

let sink stream = Stream.iter (fun _ -> ()) stream;;
