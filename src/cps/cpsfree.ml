(* Copyright 2012 Matthieu Lemerre *)

(* The [Cpsfree] module computes a map associating each lambda with the set
   of free variables used in the body of this lambda. *)

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

(*s [term t] returns a pair:

  \begin{itemize}
  \item The first element is set of free variables for [t]
  \item The second is a map from variables that bind a [Lambda]
  to the free variables in the lambda.
  \end{itemize}

  The term is traverse using a depth-first traversal (that enter into
  [Lambda]s), and the set and the map are built bottom-up.

  In the case of a recursive or mutually recursive values (lambdas,
  injections, tuples)ambda, the recursive bindings are considered to
  be free in the value (and especially in the lambda). *)
let rec term t = match Term.get t with

  | Let_prim(x,Value(Lambda(_,_,vl,bodylambda)),body) ->
    let (set,map) = (term bodylambda) in
    (* Remove arguments from the free variables of [bodylambda]. *)
    let set = List.fold_left (fun set v -> VarSet.remove v set) set vl in
    Log.Free_variables.output "Free variables for %s: [%a] \n"
      (Var.Var.to_string x)
      Log.sep_list_string (", ", Var.Var.to_string, VarSet.elements set);
    (* [x] is still considered free in [bodylambda]: it is removed from
       [set], but after being added to [map]. *)
    let map = VarMap.add x set map in
    let (set_body, map_body) = term body in
    let set = VarSet.remove x (VarSet.union set set_body) in
    let map = merge_map map_body map in
    (set,map)

  | Let_prim(x,p,body) ->
    let set_prim = prim p in
    let (set_body, map_body) = term body in
    let set = VarSet.remove x (VarSet.union set_prim set_body) in
    let map = map_body in
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
    | Some(t) -> (assert false; (* Case with default not yet supported. *)
                  let (set2,map) = term t in
                  VarSet.union set set2, map))
  | Apply(_,f,_,xl) ->
    let set = List.fold_left
      (fun set x -> VarSet.add (var x) set) VarSet.empty xl in
    VarSet.add (var f) set, VarMap.empty
  | Halt(x) -> VarSet.singleton (var x), VarMap.empty

(*s Prim and value return only a set (they would always return an
  empty map, because the [Lambda] case is already handled above). *)
and prim = function
  | Integer_binary_operation(_, a,b)
  | Integer_binary_predicate(_, a,b) -> doubleton (var a) (var b)
  | Projection(_,x) -> VarSet.singleton (var x)
  | Value v -> value v

and value = function
  | Constant(_) -> VarSet.empty
  | Tuple(l) -> (List.fold_left
                   (fun set occ -> VarSet.add (var occ) set)
                   VarSet.empty l )
  | Lambda(_,_,vl,bodylambda) -> assert false (* This case is already handled. *)
  | Injection(_,_,x) ->  VarSet.singleton (var x)
