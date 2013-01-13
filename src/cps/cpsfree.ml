(* Copyright 2012 Matthieu Lemerre *)

(* This module computes a map associating each lambda with the set of free
   variables used in the body of this lambda. *)

open Base;;

module VarSet = Var.Var.Set;;
module VarMap = Var.Var.Map;;

(* A doubleton is a set with two elements. *)
let doubleton x y = VarSet.add x (VarSet.singleton y);;

(* Merge maps, while checking that keys of [m1] and [m2] are
   disjoint. *)
let merge_map m1 m2 = VarMap.fold (fun key value map ->
  assert( not( VarMap.mem key map));
  VarMap.add key value map) m1 m2;;

let var occ = Var.Occur.binding_variable occ;;

(* [term t] returns a pair:

   \begin{itemize}
   \item The first element is set of free variables for [t]
   \item The second is a map from variables that bind a [Lambda]
   to the free variables in the lambda.
   \end{itemize}

   The term is traverse using a depth-first traversal (that enter into
   [Lambda]s), and the set and the map are built bottom-up.*)
let rec term t = match Term.get t with

  (* Note: we do a special case for let lambda to handle correctly
     recursive functions, and to associate x to the set of free
     variables. In case of a recursive function, x is not considered
     free. *)
  | Let_prim(x,Value(Lambda(_,_,vl,bodylambda)),body) ->
    let (set,map) = (term bodylambda) in
    let set = List.fold_left (fun set v -> VarSet.remove v set) set vl in
    let set = VarSet.remove x set in
    (set, VarMap.add x set map)

  | Let_prim(x,p,body) ->
    let (set_prim, map_prim) = prim p in
    let (set_body, map_body) = term body in
    let set = VarSet.remove x (VarSet.union set_prim set_body) in
    let map = merge_map map_prim map_body in
    (set,map)

  | Let_cont(_,x,t,body) ->
    let (set_t, map_t) = term t in
    let (set_body, map_body) = term body in
    let set = (VarSet.union set_body (VarSet.remove x set_t)) in
    let map = merge_map map_body map_t in
    (set,map)

  | Apply_cont(_,x) -> VarSet.singleton (var x), VarMap.empty
  | Case(o,_,default) ->
    let set = VarSet.singleton (var o) in
    (match default with
    | None -> set, VarMap.empty
    | Some(t) -> let (set2,map) = term t in
                 VarSet.union set set2, map)
  | Apply(_,f,_,xl) ->
    let set = List.fold_left
      (fun set x -> VarSet.add (var x) set) VarSet.empty xl in
    VarSet.add (var f) set, VarMap.empty
  | Halt(x) -> VarSet.singleton (var x), VarMap.empty

and prim = function
  | Integer_binary_op(_, a,b) -> doubleton (var a) (var b), VarMap.empty
  | Integer_comparison(_, a,b) -> doubleton (var a) (var b), VarMap.empty
  | Projection(_,x) -> VarSet.singleton (var x), VarMap.empty
  | Value v -> value v

and value = function
  | Constant(_) -> VarSet.empty, VarMap.empty
  | Tuple(l) -> (List.fold_left
                   (fun set occ -> VarSet.add (var occ) set)
                   VarSet.empty l ), VarMap.empty
  | Lambda(_,_,vl,body) -> assert false (* This case is already handled. *)
  | Injection(_,_,x) ->  VarSet.singleton (var x), VarMap.empty
