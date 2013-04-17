(* Copyright 2012 Matthieu Lemerre *)

(* The [Print] module outputs textual representation of terms. *)

(* Output an expression, definition, or list of definitions on formatter. *)
val expression:Format.formatter -> Cpsdef.expression -> unit
val definition:Format.formatter -> Cpsdef.definition -> unit
val definitions:Format.formatter -> Cpsdef.definition list -> unit



(* Output an expression on stderr (between dashes).  *)
val debug_expression: Cpsdef.expression -> unit
