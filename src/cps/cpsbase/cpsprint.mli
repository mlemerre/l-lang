(* Copyright 2012 Matthieu Lemerre *)

(* The [Print] module outputs textual representation of terms. *)

(* Output a term on formatter. *)
val term:Format.formatter -> Cpsdef.term -> unit

(* Output a term on stderr (between dashes).  *)
val debug_term: Cpsdef.term -> unit
