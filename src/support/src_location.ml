(* Copyright 2013 Matthieu Lemerre *)

(* Precise location of elements (tokens, expressions, etc.) in the
   source code. *)

(* A location is an interval between two positions. *)
type t = (Lexing.position * Lexing.position);;

let coordinates pos =
  let open Lexing in
  (pos.pos_fname,
   pos.pos_lnum,
   pos.pos_cnum - pos.pos_bol);;

(* The style is that of OCaml and Pythons tracebacks; it is understood
   by Emacs compilation mode, it is precise (allowing ranges of lines
   and characters), and can be read without knowing the format (thanks
   to the "lines" and "characters" text. *)
let printf fmt (pos1,pos2) =
  let (file1,line1,col1) = coordinates pos1 in
  let (file2,line2,col2) = coordinates pos2 in
  assert (file1 = file2);
  let line() =
    if line1 == line2
    then Format.fprintf fmt "line %d" line1
    else Format.fprintf fmt "lines %d-%d" line1 line2 in
  let col() =
    if col1 == col2
    then Format.fprintf fmt "character %d" col1
    else Format.fprintf fmt "characters %d-%d" col1 col2 in
  Format.fprintf fmt "File %s, %t, %t:\n"
