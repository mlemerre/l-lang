(* Copyright 2013 Matthieu Lemerre *)

let output_ (bool,name) x =
  if bool
  then
    begin
      Format.printf "#LOG (%s): %! " name;
      (* We use kfprintf to preserve the type of output_ *)
      Format.kfprintf (fun fmt -> Format.printf "@.") Format.std_formatter x
    end
  else Format.ifprintf Format.std_formatter x;;

let tmp x = output_ (true, "###tmp###") x;;

module type CATEGORY = sig
  val is_output: bool
  val output: ('a, Format.formatter, unit) format -> 'a
end

module type S = sig
  val is_output: bool
  val printed_name: string
end

module Make(Param:S):CATEGORY = struct
  include Param;;
  let output x = output_ (Param.is_output,Param.printed_name) x;;
end

(* For now, set to true/false the first field according to whether you
   want to see some output. *)

module Free_variables = Make(struct
  let is_output = true;;
  let printed_name = "free variables"
end)

module Llvm_output = Make(struct
  let is_output = false;;
  let printed_name = "llvm output"
end)

