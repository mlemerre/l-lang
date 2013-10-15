(* Copyright 2013 Matthieu Lemerre *)

(* Keywords are hash-consed (or interned), and represented by unique
   identifiers. *)
module Keyword_id = Unique.Make(struct end)
type keyword = Keyword_id.t

module Ident = struct 
  include Intern.Make(struct end);;
  let of_string = intern
end
type ident = Ident.t;;


type token =
| Ident of ident
| String of string
| Int of int
| Float of float
| Keyword of keyword
| End
;;


module Keyword = struct
  (* A mapping between a string and its identifier, in both
     directions. The reverse direction is used only by the [to_string]
     function. *)
  let keyword_table:(string,keyword) Hashtbl.t = Hashtbl.create 31;;
  let inverse_keyword_table:(keyword,string) Hashtbl.t = Hashtbl.create 31;;
  let add string =
    if Hashtbl.mem keyword_table string
    then Log.Lexer.raise_compiler_error "Keyword `%s' already added" string
    else let num = Keyword_id.fresh() in
         Hashtbl.add keyword_table string num;
         Hashtbl.add inverse_keyword_table num string;
         Keyword num

  let find string = Keyword( Hashtbl.find keyword_table string)

  (* Not exported; the user can use just Token.to_string. *)
  let to_string keyword = Hashtbl.find inverse_keyword_table keyword
end

module Separation = struct
  type t =
  | Stuck
  | Normal
  | Strong
  | Explicit
  ;;

  (* Converting from and to ints make comparisons easier. *)
  let int_of_separation = function
    | Stuck -> 0
    | Normal -> 1
    | Strong -> 2
    | Explicit -> 3
  ;;

  let separation_of_int = function
    | 0 -> Stuck
    | 1 -> Normal
    | 2 -> Strong
    | 3 -> Explicit
    | _ -> assert false
  ;;

  let max sep1 sep2 =
    separation_of_int (max (int_of_separation sep1) (int_of_separation sep2))
  ;;
end

module With_info = struct
  type t = {
    token: token;
    separation_before: Separation.t;
    separation_after: Separation.t;
    location: Src_location.t
  }
end

let to_string = function
  | Ident(id) -> Ident.to_string id
  | String(string) -> "\"" ^ string ^ "\""
  | Int(n) -> string_of_int n
  | Float(f) -> string_of_float f
  | End -> "<end-of-input>"
  | Keyword(k) -> Keyword.to_string k


let compare = Pervasives.compare
let hash = Hashtbl.hash
let equal = (=)

module Ordered = struct
  type t = token
  let compare = compare
end

module Set = Set.Make(Ordered);;
module Map = Extensions.Map.Make(Ordered);;
module Hashtbl = Hashtbl.Make(struct
  type t = token
  let hash = Hashtbl.hash
  let equal = (=)
end);;
