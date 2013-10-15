(* Copyright 2012 Matthieu Lemerre. *)

(* Log and error reporting functions, by category. Logging functions
   have the same type than [Format.printf]. *)

(* This allows to log something quickly; useful for temporary
   debugging. *)
val tmp: ('a, Format.formatter, unit) format -> 'a

(* For each category one can choose the severity- (or log-) level, of the
   error. The levels are:

   \begin{itemize}
   \item [Debug]: outputs information most useful for the devopper of a
   module of the compiler. There is no error.
   \item [Info]: outputs information useful for the user,
   like opportunities for optimizations. This is not an error, but the
   user may take action.
   \item [Warning]: indicates a possible error in the program, but
   the compiler can still proceed. The user should take action.
   \item [Error] an error that hinders production of the code of the
   module.  The user must take action.
   \item [Compiler_error] indicates that an internal invariant in the
   compiler was broken; the compiler must be fixed.
   \end{itemize}

   The last two levels stops the current processing and throw
   [User_error] and [Compiler_error] exceptions. They can be caught to
   try to execute the compiler further despite the error. *)
type level =
| Debug
| Info
| Warning
| Error
| Compiler_error;;

(* The [Log] module defines a set of debug options. If the option is
   set, then [Option.is_output] will return true; and [Option.output]
   will output something. Usage of [Option.is_output] is not
   necessary; it is provided to avoid a computation when nothing is
   printed.  [Option.is_output] is not necessary if the %a spec is
   used.  [Option.is_output] would not be necessary in a lazy
   language. *)
module type CATEGORY = sig

  (* Is a message output at this level. Useful to avoid extra
     processing that help display more precise error messages. *)
  val is_output: level -> bool

  (* Output a message at a given level. Does NOT raise any error. *)
  val output: level -> ('a, Format.formatter, unit) format -> 'a

  (* [Format.printf]-like functions for each log level. In addition
     the last two levels do not return, and throw an error. *)
  val debug:
    ?loc:Src_location.t -> ('a, Format.formatter, unit) format -> 'a
  val info:
    ?loc:Src_location.t -> ('a, Format.formatter, unit) format -> 'a
  val warning:
    ?loc:Src_location.t -> ('a, Format.formatter, unit) format -> 'a
  val raise_user_error:
    ?loc:Src_location.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  val raise_compiler_error:
    ?loc:Src_location.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  exception User_error;;
  exception Compiler_error;;
end

(* TODO: Should we declare the modules here, but in their respective
   locations? The advantage of declaring it here is that it is a
   central location to customize the logging. But logging levels
   should be chosen by the command line/environment anyway. *)
(* TODO:  We should have hierarchical categories. *)
module Free_variables:CATEGORY;;
module Llvm_output:CATEGORY;;
module Lexer:CATEGORY;;
module Parser:CATEGORY;;
module Expansion:CATEGORY;;
module Pattern_matching:CATEGORY;;
module Compilation_passes:CATEGORY;;
module Ast_elaboration:CATEGORY;;
module Cps_transformation:CATEGORY;;
module Closure_conversion:CATEGORY;;
