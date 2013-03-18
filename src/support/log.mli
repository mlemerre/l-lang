(* Copyright 2012 Matthieu Lemerre. *)

(* Output functions, by category that can be selectively made silent.
   Each output function has the same type than [Format.printf]. *)

(* This allows to log something quickly; useful for temporary
   debugging. *)
val tmp: ('a, Format.formatter, unit) format -> 'a

(* The [Log] module defines a set of debug options. If the option is
   set, then [Option.is_output] will return true; and [Option.output]
   will output something. Usage of [Option.is_output] is not
   necessary; it is provided to avoid a computation when nothing is
   printed.  [Option.is_output] is not necessary if the %a spec is
   used.  [Option.is_output] would not be necessary in a lazy
   language. *)
module type CATEGORY = sig
  val is_output: bool
  val output: ('a, Format.formatter, unit) format -> 'a
end

module Free_variables:CATEGORY;;
module Llvm_output:CATEGORY;;
