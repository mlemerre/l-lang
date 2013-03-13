(* Copyright 2013 Matthieu Lemerre. *)

(* This file regroups "constants" handled by the L compiler.
   "constants" should be understood with a broad sense; it ranges from
   integers and strings, to all primitive operations (i.e. operations
   that are compiled specially). *)

(* Integer binary operations are operations of type [(t,t) -> t], with
   [t] an integer type. *)
module Ibop = struct
  type t =
  | Add | Sub | Mul | Div
end;;

(* Integer binary predicates are operations of type [(t,t) -> bool],
   with [t] an integer type. The U and S prefix distinguish the signed
   and unsigned comparisons.*)
module Ibpred = struct
  type t =
  | Eq  | Ne
  | Ugt | Uge | Ult | Ule
  | Sgt | Sge | Slt | Sle
end;;

(* The list of constants handled by the system. *)
type t =
| Integer of int  | Float of float | String of string
| Integer_binary_operation of Ibop.t
| Integer_binary_predicate of Ibpred.t
| True | False
| Not | If | And | Or
(* TODO: Factorize these with a generic "throw" *)
| Assert | Match_failure | Failwith of string
(* TODO: Rec should not be a constant *)
| Rec

let integer_binary_operation_to_string = function
  | Ibop.Add -> "Add"
  | Ibop.Sub -> "Sub"
  | Ibop.Mul -> "Mul"
  | Ibop.Div -> "Div"
;;

let integer_binary_predicate_to_string = function
  | Ibpred.Eq -> "Eq"
  | Ibpred.Ne -> "Ne"
  | Ibpred.Ugt -> "Ugt"
  | Ibpred.Uge -> "Uge"
  | Ibpred.Ult -> "Ult"
  | Ibpred.Ule -> "Ule"
  | Ibpred.Sgt -> "Sgt"
  | Ibpred.Sge -> "Sge"
  | Ibpred.Slt -> "Slt"
  | Ibpred.Sle -> "Sle"
;;


let to_string = function
  | Integer(i) -> string_of_int i
  | Float(f) -> string_of_float f
  | String(s) -> "\"" ^ s ^ "\""
  | Integer_binary_operation( op) -> integer_binary_operation_to_string op
  | Integer_binary_predicate( pred) -> integer_binary_predicate_to_string pred
  | True -> "true" | False -> "false"
  | If -> "If" | Not -> "Not" | And -> "And" | Or -> "Or"
  | Assert -> "Assert"  | Match_failure -> "Match_failure"
  | Failwith(s) -> "Failwith( \"" ^ s ^ "\")"
  | Rec -> "Rec"
