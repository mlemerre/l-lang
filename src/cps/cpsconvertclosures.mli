(* Copyright 2013 Matthieu Lemerre *)

(* This module performs \emph{closure conversion}, i.e. removes the
   \emph{free variables} from [Lambda] constructs. *)

(* Closure conversion is performed in-place, hence the type. We
   provide two versions: an expression-based one (for testing purposes
   only), and a definition-based one.

   TODO (easy): Remove the definition of [in_expression] from the
   signature. *)
val in_expression: Cpsbase.expression -> unit
val in_definition: Cpsbase.definition -> unit
