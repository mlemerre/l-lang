(* Copyright 2013 Matthieu Lemerre *)

type term = {
  func: func;
  arguments: term list;
  location: Src_location.t;
}

and func =
| Token of Token.With_info.t
| TokenPair of Token.With_info.t * Token.With_info.t
| Custom of string
;;

type expression = term
type statement = term
type pattern = term
type case = term

let rec print fmt term = begin
  (match term.func with
  | Token(tok) -> Format.fprintf fmt "%s"
    (Token.to_string tok.Token.With_info.token)
  | TokenPair(tok1,tok2) -> Format.fprintf fmt "%s%s"
    (Token.to_string tok1.Token.With_info.token)
    (Token.to_string tok2.Token.With_info.token)
  | Custom(s) -> Format.fprintf fmt "%s" s);
  (match term.arguments with
  | [] -> ()
  | args ->
    Format.fprintf fmt "( @[<hov>%a@])"
      (Make_printer.list ~sep:",@ " print) args)
end
;;

let between_toks tok1 tok2 =
  (fst tok1.Token.With_info.location,
   snd tok2.Token.With_info.location)

let between_tok_term tok1 term =
  (fst tok1.Token.With_info.location,
   snd term.location)

let between_terms term1 term2 =
  (fst term1.location, snd term2.location)

let single token =
  { func = Token token; arguments = [];
    location = token.Token.With_info.location }
;;

let infix_binary_op t1 token t2 =
  { func = Token token; arguments = [t1;t2];
    location = between_terms t1 t2 }

let delimited_list token1 args token2 =
  { func = TokenPair( token1, token2); arguments = args;
    location = (fst token1.Token.With_info.location,
                snd token2.Token.With_info.location) }
;;
