(* Copyright 2012 Matthieu Lemerre *)

(* The [Print] module outputs textual representation of terms. *)

(* Output a term, definition, or list of definitions on formatter. *)
val term:Format.formatter -> Cpsdef.term -> unit
val definition:Format.formatter -> Cpsdef.definition -> unit
val definitions:Format.formatter -> Cpsdef.definition list -> unit



(* Output a term on stderr (between dashes).  *)
val debug_term: Cpsdef.term -> unit
