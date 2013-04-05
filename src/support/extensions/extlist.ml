(* Copyright 2013 Matthieu Lemerre.  *)

let rec fold_left_with_index f accu ?(start_index=0) l =
  match l with
    | [] -> accu
    | a::l -> let new_accu = (f accu a start_index) in
	      fold_left_with_index f new_accu ~start_index:(start_index + 1) l;;

let rec iter_with_index f ?(start_index=0) = function
  | [] -> ()
  | a::l -> f a start_index; iter_with_index f ~start_index:(start_index+1) l
