(* Copyright 2013 Matthieu Lemerre *)

(* Note: unlike C, writed to stderr are not buffered by default; this
   module handles all the flushing. *)

let koutput_ ?loc k (bool,name) x =
  if bool
  then
    begin
      let maybe_print_loc fmt = match loc with
        | Some(loc) -> Src_location.fprintf fmt loc
        | None -> () in
      Format.eprintf "@.%t#LOG (%s): @? " maybe_print_loc name;
      (* We use kfprintf to preserve the type of output_ *)
      Format.kfprintf (fun fmt -> Format.eprintf "@."; k()) Format.err_formatter x
    end
  else Format.ikfprintf (fun _ -> k()) Format.err_formatter x;;

let output_ (bool,name) x = koutput_ (fun () -> ()) (bool,name) x;;

let tmp x = output_ (true, "###tmp###") x;;

type level =
| Debug
| Info
| Warning
| Error
| Compiler_error;;

(* Comparison between levels. *)
let ( >== ) output reference =
  let to_int = function
    | Debug -> 0 | Info -> 1 | Warning -> 2 | Error -> 3 | Compiler_error -> 4 in
  (to_int output) >= (to_int reference)

module type CATEGORY = sig
  val is_output: level -> bool
  val output: level -> ('a, Format.formatter, unit) format -> 'a

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

module type S = sig
  val min_output_level: level
  val printed_name: string
end

module Make(Param:S):CATEGORY = struct
  let is_output l = l >== Param.min_output_level;;
  let output level x = output_ (is_output level,Param.printed_name) x;;
  let koutput ?loc k level x = koutput_ ?loc k (is_output level,Param.printed_name) x;;

  let debug ?loc x = output Debug x
  let info ?loc x = output Info x
  let warning ?loc x = output Warning x

  exception User_error;;
  exception Compiler_error;;

  let raise_user_error ?loc x = koutput ?loc (fun () -> raise User_error) Error x
  let raise_compiler_error ?loc x = koutput ?loc (fun () -> raise Compiler_error) Error x
end

(* For now, we set manually the min output level. *)
module Free_variables = Make(struct
  let min_output_level = Info;;
  let printed_name = "free variables"
end)

module Llvm_output = Make(struct
  let min_output_level = Info;;
  let printed_name = "llvm output"
end)

module Lexer = Make(struct
  let min_output_level = Info;;
  let printed_name = "lexer"
end)

module Parser = Make(struct
  let min_output_level = Info;;
  let printed_name = "parser"
end)

module Expansion = Make(struct
  let min_output_level = Info;;
  let printed_name = "expansion"
end)

module Pattern_matching = Make(struct
  let min_output_level = Info;;
  let printed_name = "pattern matching"
end)

module Compilation_passes = Make(struct
  let min_output_level = Info;;
  let printed_name = "compilation_passes"
end)

module Ast_elaboration = Make(struct
  let min_output_level = Info;;
  let printed_name = "AST elaboration"
end)

module Cps_transformation = Make(struct
  let min_output_level = Info;;
  let printed_name = "CPS transformation"
end)

module Closure_conversion = Make(struct
  let min_output_level = Info;;
  let printed_name = "CPS closure conversion"
end)




