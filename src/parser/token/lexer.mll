(* Copyright 2013 Matthieu Lemerre. *)

(* The lexer for the L language.  *)
{

module Token = Base;;

}

(* We separate ids into several categories:
   \begin{enumerate}
   \item lower case ids, used for variable names; names are separated
   by underscores.

   \item capitalized ids, used for constants and constructors. Their
   first letter is uppercase, the rest lowercase, and separated by
   underscores.

   \item camlcased upper case ids (used for types, modules and
   classes). These must not use underscores.

   \item '_' is a special id, indicating that the name is unspecified
   (e.g. unnamed function argument, unnamed catchall pattern, unnamed
   type).

   \end{enumerate}

   In any case (except '_'), variables must begin by a character.

   The motivation for these categories is greater readability: it is
   easy for the reader to differentiate the three categories
   (categories 1 and 3 being opposite, and category 2 being "the
   middle ground"). There is still an ambiguity between category 2 and
   3 for single-word ids, but it is generally clear in the context. By
   fixing a convention, we avoid people having source code with
   different coding conventions.

   Because of this ambiguity, the distinction between syntactical
   categories of id is made in later parsing phases.

   Additionally, ids may end by a '?' or a '!'. '?' is used for
   predicates, or functions that may fail (and then return an option
   type). ids ending with '!' are reserved for future use. *)
let id = ('_' | ['a'-'z' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* ['?']? )
let digit = ['0'-'9']

(* We want to allow usage of a variety of symbols, but at the same
   some consecutive characters must be parsed as different symbols;
   for instance "((" in ((3+4)*2), "()", "(-" in 3 * (-4 + 5). (This
   problem happens in many situation; however two infix symbols are
   separated by expressions which always contain a number or an
   identifier, and cannot be used in succession).

   To attempt avoid this problem we have defined symbol categories
   that attempts to avoid overlapping (especially overlapping of
   prefix symbols). Even with this, some classic overlapping remain,
   e.g in <-, <<, >>, --, etc. For clarity, it is better to clearly
   separate the use of custom symbols using space. *)
let tex_symbol = '\\' [^'\t' ' ' '\n' ';']+
let custom_symbol = ['=' '+' '-' '|' '*' '.' '!' '?' '<' '>' ':' '/']+
let arrow_end = ( "<"+ | ">"+ | "()" | "[]" | "|" | "+" | "*")
let arrow_symbol = arrow_end? ['-' '=' '|' '+' '~']+ arrow_end?
let open_symbol = ['(' '{' '<' '['] ['|']?
let close_symbol =  ['|']?[']' '>' '}' ')']

(* Symbols that begin with an open delimiter, contains only symbols,
   and end with a close delimiter. Exemples: (+), {---}, ... *)
let round_symbol = '(' custom_symbol ')'
let square_symbol = '[' custom_symbol ']'
let brace_symbol = '{' custom_symbol '}'

let symbol = tex_symbol | custom_symbol | arrow_symbol | ","
  | open_symbol | close_symbol
  | round_symbol | square_symbol | brace_symbol

(* We defined 2 rules, which are not mutually recursive, and must be
   called alternatively. One analyze the tokens, the other the
   separation (i.e. space) between tokens. *)
rule token = parse
  | digit+ as inum
      { let num = int_of_string inum
        (* TODO: Check for truncation. *)
	in Token.Int( num) }

  | digit+ '.' digit* as fnum
      { failwith "Float not yet implemented";
        (* TODO: Ensure that floats are carried around without losing precision. *)
        let num = float_of_string fnum in
  	Token.Float( num) }

  (* Ids may be normal ids, or keywords if they belong to the hashtable. *)
  | id as word
      { try Token.Keyword.find word
	with Not_found -> Token.Ident(word) }

  | symbol as op
      { try Token.Keyword.find op
        with Not_found -> Log.Lexer.raise_user_error "Unknown symbol `%s'" op }

  | _ as c
      { Log.Lexer.raise_user_error "Unrecognized character: %c" c }

  | eof
      { Token.End }

(* The [cur_sep] argument is the current level of separation; it is
   updated according to the nature of spacing characters between
   tokens. *)
and sep cur_sep = parse
  | ("//" [^ '\n']* | [' ' '\t']+)+     (* Whitespaces and comments. *)
      { let new_sep = Token.Separation.max cur_sep Token.Separation.Normal in
        sep new_sep lexbuf }

  | ['\n']
      { let new_sep = Token.Separation.max cur_sep Token.Separation.Strong in
	Lexing.new_line lexbuf;
	sep new_sep lexbuf }

  | [';']
      { let new_sep = Token.Separation.max cur_sep Token.Separation.Explicit in
        sep new_sep lexbuf }

  | eof { Token.Separation.max cur_sep Token.Separation.Strong }

  | "" { cur_sep }

{
  type t  = {
    lexbuf: Lexing.lexbuf;

    (* Remembering the parsed token is necessary to implement [peek]. *)
    mutable cache: Token.With_info.t option;

    (* Remembering the previous separation is used to provide
       [separation_before]. *)
    mutable previous_separation: Token.Separation.t
  }

  (* Returns the location [(start_pos,end_pos)] of token that was just
     parsed. *)
  let current_location lexbuf =
    (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)


  (* Retrieve the next token. [t.lexbuf] must point to a token (i.e.
     there must have been a previous call to [sep] on [t.lexbuf]).
     This function should be called only if the cache is empty (if
     full, empty the cache first). *)
  let get_next t =
    assert (t.cache == None);
    let separation_before = t.previous_separation in
    let token = token t.lexbuf in
    let location = current_location t.lexbuf in
    (* After each token, the separation level is reset to [Stuck]. *)
    let separation_after = sep Token.Separation.Stuck t.lexbuf in
    t.previous_separation <- separation_after;
    let open Token.With_info in
    { token; location; separation_before; separation_after}
  ;;

  (* Note that if [next] is called repeteadly, the tokens do not enter
     the cache.  *)
  let next t =
    match t.cache with
    | None -> get_next t
    | Some(x) -> t.cache <- None; x;;

  let peek t =
    match t.cache with
    | None ->
      let tokeni = get_next t in
      t.cache <- Some tokeni;
      tokeni
    | Some(x) -> x
  ;;

  let junk t = ignore (next t);;

  let make filename =
    let lexbuf =
      if filename = "-"
      then
        Lexing.from_channel Pervasives.stdin
      else
        let channel = open_in filename in
        let lexbuf = Lexing.from_channel channel in
        lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p
                                      with Lexing.pos_fname = filename };
        lexbuf in

    (* The first token in filename is considered as being after a newline.  *)
    let previous_separation = sep Token.Separation.Strong lexbuf in

    {cache = None; previous_separation; lexbuf }
  ;;
}
