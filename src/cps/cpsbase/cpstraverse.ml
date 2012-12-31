(* Copyright 2012 Matthieu Lemerre *)

open Cpsdef;;

(* Note: in the implementation of this function, it is important that
   we call [f] on the way back of the traversal of the tree. This
   allows to save the terms on the stack before iterating on them, and
   thus iterating on the original terms, even if [f] changes the
   terms. *)
let iter_on_terms ~enter_lambdas t f = 
  if enter_lambdas then
    let rec loop t = (match Term.get t with
      | Let_cont(_,_,term,body) -> loop term; loop body
      | Let_prim(_,Value(Lambda(_,_,_,body_lambda)),body) -> loop body_lambda; loop body
      | Let_prim(_,_,body) -> loop body
      | _ -> ()); f t
    in loop t
  else
    let rec loop t = ((match Term.get t with
      | Let_cont(_,_,term,body) -> loop term; loop body
      | Let_prim(_,_,body) -> loop body
      | _ -> ()); f t)
    in loop t
;;

(* Traverse the tree of CPS term; call a function on the binding
   variable (i.e. before any occurrence of the variable is found),
   then on each occurrence, then on the binding variable again (i.e.
   once all occurrences have been found); and do that for both the
   variable and the continuation variable. *)
let fold_on_variables_and_occurrences t init 
    ~before_var ~occ ~after_var ~before_cont_var ~cont_occ ~after_cont_var =
  let rec loop acc t = 
    match Term.get t with
    | Let_prim(x,p,body) -> 
      let acc = before_var acc x in
      let acc = 
        (match p with
        | Projection(o,_) -> occ acc o
        | Integer_binary_op(_,a,b) -> occ (occ acc a) b
        | Value v -> (match v with
          | Void | Constant(_) -> acc
          | Tuple(l) -> List.fold_left occ acc l
          | Lambda(_,k,xl,body) ->
            let acc = before_cont_var acc k in
            let acc = List.fold_left before_var acc xl in
            let acc = loop acc body in
            let acc = List.fold_left after_var acc xl in
            let acc = after_cont_var acc k in
            acc
        )) in
      let acc = loop acc body in
      let acc = after_var acc x in
      acc
    | Let_cont(k,x,term,body) ->
      let acc = before_cont_var acc k in
      let acc = before_var acc x in 
      let acc = loop acc term in
      let acc = loop acc body in
      let acc = after_var acc x in 
      let acc = after_cont_var acc k in
      acc
    | Apply(_,f,k,xl) ->
      let acc = occ acc f in
      let acc = cont_occ acc k in
      let acc = List.fold_left occ acc xl in
      acc
    | Apply_cont(k,x) -> 
      let acc = cont_occ acc k in
      let acc = occ acc x in
      acc
    | Halt(x) -> occ acc x
  in loop init t
;;

let fold_on_occurrences t init ~occ ~cont_occ =
  let fold_id = (fun acc x -> acc) in
  fold_on_variables_and_occurrences t init 
    ~before_var:fold_id ~occ ~after_var:fold_id
    ~before_cont_var:fold_id ~cont_occ ~after_cont_var:fold_id
;;

let iter_on_occurrences t ~occ ~cont_occ = 
  fold_on_occurrences t ()
    ~occ:(fun () o -> occ o)
    ~cont_occ:(fun () o -> cont_occ o) 
