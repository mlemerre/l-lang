(* Copyright 2013 Matthieu Lemerre *)

(* This file describes the parsetree, which is the result of the
   parsing phase.

   The parsetree format has the following properties:
   \begin{itemize}

   \item First, it is really simple to produce, being the simplest
   abstraction of top of the stream of token; this makes parsing code
   simpler. This is important because parsing code (especially for
   hand-written parsers) tend to be hard to read.

   \item It is extensible, which allows to easily add new syntactic
   sugar, or more complex mechanisms like a Lisp-like macro system.

   \item It has a close correspondance to the source code, and
   locations in the source code:
   \begin{itemize}
   \item Each node in the parsetree corresponds to an interval between
   two locations in the source code.
   \item The interval of a term encloses the intervals of each
   subterm.
   \item The intervals of several siblings do not overlap, and appear
   in the order of the source code.
   \end{itemize}
   This property makes the parsetree useful for bidirectional
   correspondance between the abstract syntax and the source code.

   \end{itemize}

*)

(* Algebra of grounded first-order terms. *)
type term = {
  func: func;
  arguments: term list;
  location: Src_location.t;
}

(* The function symbol of a term. Tokens are generally prefered;
   Token pairs apply to delimited lists; Custom apply when there is no
   suitable token. *)
and func =
| Token of Token.With_info.t
| TokenPair of Token.With_info.t * Token.With_info.t
| Custom of string
;;

(* Aliases that give information about the intended return value. *)
type expression = term
type statement = term
type pattern = term
type case = term

val print: Format.formatter -> term -> unit

(****************************************************************)
(* Some functions that help building a term. Their main interest is
   that they build the location information automatically. *)

(* Single token, e.g. an integer or an id. *)
val single: Token.With_info.t -> term

(* Token between two terms. *)
val infix_binary_op: term -> Token.With_info.t -> term -> term

(* Token that start a list, token that ends a list,  *)
val delimited_list: Token.With_info.t -> term list -> Token.With_info.t -> term

(****************************************************************)
(* Some functions to compute the location information. Returns the
   location between a pair or token or terms. *)
val between_toks: Token.With_info.t -> Token.With_info.t -> Src_location.t
val between_tok_term: Token.With_info.t -> term -> Src_location.t
val between_terms: term -> term -> Src_location.t
