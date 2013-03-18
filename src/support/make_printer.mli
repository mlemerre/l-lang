(* Copyright 2013 Matthieu Lemerre *)

(* This module helps producing user-defined printers, called with
   ["%a"] in the format string. All the functions presented here have
   type ['a -> Format.formatter -> 'b -> unit], with ['b] being the
   type of the argument being output, and ['a] parameters to the
   user-defined printer producing function.
   
   They can be used with

   [Format.printf "...%a..." (user_defined_printer_producing_function
   args) element_being_displayed]

   or any other function behaving like [Format.printf]. *)

(* Print a list, from the function displaying each element. *)
val list :
  ?empty:string ->
  ?open_paren:string ->
  ?close_paren:string ->
  ?sep:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(* Print an element from the function computing a string representation of this element.  *)
val from_string: ('a -> string) -> Format.formatter -> 'a -> unit
