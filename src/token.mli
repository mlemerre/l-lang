(* Copyright 2013 Matthieu Lemerre *)

(* Tokens are the result of the lexing pass. Two special categories
   are defined. [keyword]s, which are either identifiers handled
   specially (e.g. [let] or [if]), or symbols (like [+] or [->]); the
   lexer is extensible and allows defining new keywords. [ident]s,
   which are hash-consed (interned) strings and provide a [Key]
   interface for convenient later use. *)
type keyword;;
type ident;;

type token =
| Ident of ident           (* An identifier (begins with a letter) *)
| String of string         (* A quoted string.  *)
| Int of int               (* An integer value. *)
| Float of float           (* A float value. *)
| Keyword of keyword       (* Any keyword or punctuation mark. *)
| End;;                    (* Indicates the end of a stream. *)

include Key.S with type t := token;;

module Ident:sig
  (* Note: as there is no location information in Ident, the
     token_with_info should probably be kept around? Or location is
     sufficient? *)
  val to_string: ident -> string
  val of_string: string -> ident
  include Key.S with type t := ident;;
end

module Keyword : sig
  (* [add string] adds a new keyword. [string] must not have been
     already added. *)
  val add: string -> token

  (* [find string] finds an existing keyword. Raises [Not_found] if
     [string] is not a keyword. *)
  val find: string -> token            
end

(* String representation of a token. *)
val to_string: token -> string

(* The L parser is sensitive to separation, i.e. spacing between
   tokens. For instance,

   [f( x)] (with a stuck [f] and [(]) applies [x] to [f];
   [f (x)] (with space between [f] and [(] is incorrect;

   [f; (x)] (with a [;] or a newline between [f] and [(x)] is
   interpreted as first executing [f], then executing [(x)].

   Separation allows to reuse symbols for different contexts and avoids
   the use of underscore to separate consecutive statements. As a
   bonus, it can also enforce some common coding style conventions. *)
module Separation : sig

  (* Separation levels, from the more stuck to the more separated. *)
  type t =
  | Stuck    (* Stuck to the previous element *)
  | Normal   (* Separated with a space *)
  | Strong   (* Separated with \n *)
  | Explicit (* Separated with ; *)

  val max: t -> t -> t
end

(* Lexed tokens come with information about them: their separation
   level, and their location in the source code. *)
module With_info : sig
  type t = {
    token: token;
    separation_before: Separation.t;
    separation_after: Separation.t;
    location: Src_location.t
  }
end

(* A stream of token is the result of the lexing pass.  *)
module Stream: sig
  type t

  (* Create a new stream from a file name. "-" indicates stdin. *)
  val make: string -> t;;

  (* Remove the first token from of the stream, and return it. *)
  val next: t -> With_info.t

  (* Reads the first token in the stream, leaving it in the stream. *)
  val peek: t -> With_info.t

  (* Reads the nth next token on the stream (starting from 0), leaving
     it in the stream. [peek stream] is equivalent to [peek_nth stream
     0]. *)
  val peek_nth: t -> int -> With_info.t

  (* Remove the first token from the stream, without returning it. *)
  val junk: t -> unit

(* The list of predefined keywords for the L language. *)
module Keywords: sig
val lparen:token;; val rparen:token;;
val lbrace:token;; val rbrace:token;;
val colon:token;; val doublecolon:token;; val coma:token;; val pipe:token;;
val equals:token;; val eq:token;; val ne:token;; 
val gt:token;; val lt:token;; val le:token;; val ge:token;;
val plus:token;; val minus:token;; val star:token;; val slash:token;;
val semicolon:token;;
val arrow:token;;
val assigns:token;;

(* Non-symbols keywords. Those that are also OCaml keyword are
   suffixed by _. *)
val cast:token;;
val if_:token;; val else_:token;;
val match_:token;;
val let_:token;; val and_:token;;
val def:token;; val type_:token;; val declare:token;; val module_:token;; val data:token
end


