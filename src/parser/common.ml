(* Copyright 2013 Matthieu Lemerre *)
(* Helper functions common to all parser modules.  *)
 

(* Common shortcuts. *)
module P = Parsetree;;
module Sep = Token.Separation;;
module Kwd = Token.Keywords;;
open Token.With_info;;


(****************************************************************)
(* Checks and error reporting. *)

(* Checks that a parsed token is as expected. *)
let expect (ti:Token.With_info.t) ?before_min ?before_max ?after_min ?after_max (t:Token.token) = 
  if ti.token <> t 
  then Log.Parser.raise_user_error ~loc:ti.location "expected %s"
    (Token.to_string t);

  (* Check separations. *)
  let separation_level_order = function
    | Sep.Stuck -> 0
    | Sep.Normal -> 1
    | Sep.Strong -> 2
    | Sep.Explicit -> 3
  in
  let (>>) a b = separation_level_order a > separation_level_order b in

  let min_separation_error required before_after = 
    let sep_explaination = match required with 
      | Sep.Stuck -> assert false (* Cannot be smaller than stuck *)
      | Sep.Normal -> "at least one space character"
      | Sep.Strong -> "at least one newline character"
      | Sep.Explicit -> "at least one `;' character"
    in
    Log.Parser.raise_user_error ~loc:ti.location 
        "insufficient separation %s token %s:\
         should be separated by %s " 
      before_after (Token.to_string t) (sep_explaination)

  and max_separation_error required before_after = 
    let sep_explaination = match required with 
      | Sep.Stuck -> 
        "be stuck (i.e. must not contain any character)"
      | Sep.Normal -> 
        "not contain any `;` or newline characters (it can contain only spaces)"
      | Sep.Strong -> 
        "not contain any `;' (it can contain only space and newline characters)"
      | Sep.Explicit -> assert false    (* cannot be larger than ; *)
    in
    Log.Parser.raise_user_error ~loc:ti.location 
        "too large separation %s token `%s': \
         it must %s "
      before_after (Token.to_string t) (sep_explaination)
  in
  (* Note: these checks could be desactivated with a configuration
     option; this would be useful e.g. during development. Their main
     use is to make the syntax more consistent; and are not necessary
     for parsing (since they are given to [expect], meaning that the
     token is already parsed). *)
  (match before_min with
  | Some(x) when x >> ti.separation_before ->
    min_separation_error x "before"
  | _ -> ());
  (match before_max with
  | Some(x) when ti.separation_before >> x -> 
    max_separation_error x "before"
  | _ -> ());
  (match after_min with
  | Some(x) when x >> ti.separation_after -> 
    min_separation_error x "after"
  | _ -> ());
  (match after_max with
  | Some(x) when ti.separation_after >> x -> 
    max_separation_error x "after"
  | _ -> ());
  ()
;;

let expect_id (ti:Token.With_info.t) =
  match ti.token with
  | Token.Ident(_) -> ()
  | _ -> Log.Parser.raise_user_error ~loc:ti.location "expected id"
;;

let expect_strong_separation stream = 
  match (Token.Stream.peek stream).separation_before with
  | Token.Separation.Strong | Token.Separation.Explicit -> ()
  | _ -> failwith "strong separation expected"
;;

let expect_stuck_separation stream =
  match (Token.Stream.peek stream).separation_before with
  | Token.Separation.Stuck -> ()
  | _ -> failwith "The token should be stuck"
;;


(* The difference between assert and check is that checks could be
   removed (it detects a compiler error, not a user one). *)
let check (ti:Token.With_info.t) (t:Token.token) = 
  if ti.token <> t 
  then Log.Parser.raise_compiler_error ~loc:ti.location "expected %s"
    (Token.to_string t)
;;

(****************************************************************)
(* Helper parsing functions. *)

(* Parse a comma-separated tuple, parsing each element with parse_fun.
   Returns the list of elements that have been parsed. The list may be
   empty. *)
let parse_tuple_generic stream parse_fun =
  let lparen = Token.Stream.next stream in 
  expect lparen Kwd.lparen ~after_max:Sep.Strong;
  if ((Token.Stream.peek stream).token = Kwd.rparen)
  then (let rparen = Token.Stream.next stream in
        expect rparen Kwd.rparen ~before_max:Sep.Strong; 
        (lparen,rparen, []))
  else let exp = parse_fun stream in
       let rec after_comma () = 
	 if ((Token.Stream.peek stream).token = Kwd.rparen)
	 then []
	 else (expect (Token.Stream.next stream) Kwd.comma
                 ~before_max:Sep.Normal ~after_max:Sep.Strong;
	       let exp = parse_fun stream
	       in exp::after_comma())
       in let additional_args = after_comma() in
          let rparen = Token.Stream.next stream in
          expect rparen Kwd.rparen ~before_max:Sep.Strong; 
	  (lparen,rparen, exp::additional_args)
;;

(* Parse a comma separated list. There must be at least one element. *)
let rec parse_comma_separated_list stream parse_element_fun = 
  let elt = parse_element_fun stream in
  let next = Token.Stream.peek stream in
  if next.token = Kwd.comma
  then (expect next Kwd.comma ~before_max:Sep.Normal ~after_max:Sep.Strong;
        Token.Stream.junk stream;
	elt::(parse_comma_separated_list stream parse_element_fun))
  else [elt];;

(* These functions provide different means to handle separation. *)

(* Require the infix token to be stuck to the previous token. *)
let infix_when_stuck x t = match t.separation_before with
  | Sep.Stuck -> x
  | Sep.Normal -> 
    Log.Parser.raise_compiler_error ~loc:t.Token.With_info.location
      "invalid separation for token %s" (Token.to_string t.Token.With_info.token)
  | Sep.Strong | Sep.Explicit -> -1

(* Normal infix: does not take into account  *)
let infix_when_normal f t = match t.separation_before with
  | Sep.Stuck | Sep.Normal -> f
  | Sep.Strong | Sep.Explicit -> -1
;;

(* The infix token can be used even after newlines, but must be stuck
   to the following token. This is useful for the `.' operator, for
   chaining on multiple lines. To avoid confusion, this should be used
   only when the token cannot be used in prefix position; e.g. no
   expression may begin by `.'. *)
let infix_when_stuck_after x t = match t.separation_before with
  | Sep.Explicit -> -1
  | Sep.Stuck | Sep.Normal | Sep.Strong -> 
    (match t.separation_after with
    | Sep.Stuck -> x
    | _ -> Log.Parser.raise_user_error 
      "Token `%s' should be stuck to the following token" 
      (Token.to_string t.token));;


