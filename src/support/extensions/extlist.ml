(* Copyright 2013 Matthieu Lemerre.  *)

let rec fold_left_with_index f accu ?(start_index=0) l =
  match l with
    | [] -> accu
    | a::l -> let new_accu = (f accu a start_index) in
	      fold_left_with_index f new_accu ~start_index:(start_index + 1) l;;

let rec iter_with_index f ?(start_index=0) = function
  | [] -> ()
  | a::l -> f a start_index; iter_with_index f ~start_index:(start_index+1) l

let rec map_with_index f ?(start_index=0) = function
  | [] -> []
  | a::l -> (f a start_index)::(map_with_index f ~start_index:(start_index+1) l)

let rec foldk f accu list k = match list with
  | [] -> k accu
  | a::b -> f accu a (fun x -> foldk f x b k)


let rec last = function
  | [] -> failwith "List.last called on empty list"
  | [x] -> x
  | a::b -> last b

let rec subst f = function
  | [] as l -> l (* Note: reusing [l] is not necessary here
                    because [] is uniquely represented. *)
  | a::b as l ->
    let new_b = subst f b in
    let new_a = f a in
    if a == new_a && b == new_b then l
    else new_a::new_b

let rec map_filter f = function
  | [] -> []
  | a::b ->
    let mapb = map_filter f b in
    (match f a with
    | None -> mapb
    | Some(x) -> x::mapb)
;;
