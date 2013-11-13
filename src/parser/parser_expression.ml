(* Copyright 2013 Matthieu Lemerre *)

(* This module implements the TDOP-based parser of L expressions. *)

(*p \include{../../../doc/grammardefs} *)

open Parser_base;;
open Token.With_info;;
module List = Extensions.List;;

module ExpSemanticActions = struct
  type t = P.expression
  let int_handler n stream = P.single (Token.Stream.next stream);;

  (* TODO: Differentiate upper and lower ids at the token level *)
  let ident_handler id stream =
    Parser_path.parse_path_to
      (fun stream -> P.single (Token.Stream.next stream))
      stream

end;;

(*\begin{grammar}
  \item $\call{exp} ::=\\
  \alt \call{int}\\
  \alt \call{dir}\call{id}$
  \end{grammar} *)
module ExpTdop = Tdop.Make(ExpSemanticActions);;

let parse_expression stream = ExpTdop.parse stream 0;;

(* Operator relative precedence is inspired by the C grammar. *)
let binary_infix tok ~left ~right = P.infix_binary_op left tok right
;;

(*\begin{grammar}
  \item $\addinfix{exp}{==}{90}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{==}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{!=}{90}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{!=}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{<}{a0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{<}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{<=}{a0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{<=}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{>=}{a0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{>=}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{>}{a0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{>}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{+}{c0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{+}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{-}{c0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{-}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{*}{d0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{*}^{\backslash{}n}\  \call{exp}}$
  \item $\addinfix{exp}{/}{d0}{leftassoc}{
  \ {{}^{\textrm{\textvisiblespace}}}\tok{/}^{\backslash{}n}\  \call{exp}}$
  \end{grammar} *)
ExpTdop.define_infix_left_associative Kwd.eq    (infix_when_normal 0x9000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.ne    (infix_when_normal 0x9000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.lt    (infix_when_normal 0xa000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.le    (infix_when_normal 0xa000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.ge    (infix_when_normal 0xa000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.gt    (infix_when_normal 0xa000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.plus  (infix_when_normal 0xc000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.minus (infix_when_normal 0xc000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.star  (infix_when_normal 0xd000) binary_infix;;
ExpTdop.define_infix_left_associative Kwd.slash (infix_when_normal 0xd000) binary_infix;;

(* \begin{grammar}
   \item $\addprefix{exp}{-}{\tok{-}^\textrm\textvisiblespace\ \call{exp}}$
   \end{grammar} *)
ExpTdop.define_prefix Kwd.minus (fun stream ->
  let minus = Token.Stream.next stream in
  expect minus Kwd.minux ~after_max:Kwd.Normal;
  let exp = ExpTdop.parse stream 0xf000 in
  { P.func = P.Token minus;
    P.arguments = [exp];
    P.location = P.between_tok_term minus exp })
;;

(* \begin{grammar}
   \item $\call{tuple\_exp} ::=\\
   \alt \tok{(}^{\backslash{}n}{}^{\backslash{}n}\tok{)}\\
   \alt \tok{(}^{\backslash{}n}\ \call{exp}\ ({}^\textrm\textvisiblespace\tok{,}^{\backslash{}n}\ \call{exp})* )\ {}^{\backslash{}n}\tok{)}$
   \item $\addprefix{exp}{(}{\call{tuple\_exp}}$
   \item $\addinfix{exp}{(}{f0}{leftassoc}{\call{tuple\_exp}}$
   \end{grammar}

   Note that function calls bind very strongly, more than other
   operators. This allows to write f() + g(), instead of (f()) +
   (g()); this is more useful than having + that bind more strongly
   than function call because e.g. (1 + g)() has no meaning. *)
let parse_tuple stream =
  let lpar,rpar,list_exp = parse_tuple_generic stream parse_expression in
  P.delimited_list lpar list_exp rpar
in ExpTdop.define_prefix Kwd.lparen parse_tuple;
   ExpTdop.define_prefix Kwd.lparen parse_tuple;
   let infix_fun stream caller =
     let args = parse_tuple stream in
     { P.func = P.Custom "apply"; P.arguments = [caller;args];
       P.location = P.between_terms caller args }
   in ExpTdop.define_infix Kwd.lparen (infix_when_stuck 0xf000) infix_fun;;

(* \begin{grammar}
   \item $\addprefix{exp}{if}{\tok{if} \tok{(}^{\backslash{}n} \call{exp} {}^{\backslash{}n}\tok{)}^{\backslash{}n}
   \ \call{exp}\ {}^{\backslash{}n}\tok{else}^{\backslash{}n}\ \call{exp}}$
   \end{grammar}

   Note: for now we require parens around the condition of the if. We
   can remove this condition, but this requires that it is always ok
   to parse consecutive expressions. Currently, f { x } or f ( x ) is
   flagged as an error, making impossible to write things like if f {
   x + 1}. The parser is a bit conservative here to help catch these
   mistakes, but we will probably relax it later. *)
let parse_if stream =
  let iftok = Token.Stream.next stream in
  expect (Token.Stream.next stream) Kwd.lparen
    ~before_max:Sep.Stuck ~after_max:Sep.Strong;
  let cond = parse_expression stream in
  expect (Token.Stream.next stream) Kwd.rparen
    ~before_max:Sep.Strong ~after_max:Sep.Strong;
  let then_ = parse_expression stream in
  expect (Token.Stream.next stream) Kwd.else_ ~before_max:Sep.Strong ~after_max:Sep.Strong ;
  let else_ = parse_expression stream in
  { P.func = P.Token iftok; P.arguments = [cond;then_;else_];
    P.location = P.between_tok_term iftok else_ }
in ExpTdop.define_prefix Kwd.if_ parse_if

(* The pattern language is actually a sub-language of the language of
   expressions. Parsing patterns as expressions allows to parse e.g.
   function definitions without backtracking. This requires to return
   the same thing for expressions and patterns, i.e. prevent having a
   complete AST in a single pass. This is one of the reason why we use
   [Parsetree] as an intermediate format. *)
let parse_pattern = parse_expression;;


(* \begin{grammar}
   \item $\call{let\_binding} ::= \call{pattern}\
   {}^\textrm\textvisiblespace\tok{=}^{\backslash{}n}\ \call{expression}$
   \end{grammar}  *)
let parse_let_binding stream =
  let pattern = parse_pattern stream in
  let eq_tok = Token.Stream.next stream in
  expect eq_tok Kwd.equals ~before_max:Sep.Normal ~after_max:Sep.Strong;
  let exp = parse_expression stream in
  (* TODO: For and: return
  and(=(patt1,exp1),and(=(patt2,exp2),=(patt3,exp3)))  *)
  (* TODO: Should be in parse_let_bindings? *)
  (if (Token.Stream.peek stream).token = Kwd.and_
   then failwith "parser: and_ not implemented");
  (* P.infix_binary_op pattern eq_tok exp  *)
  (pattern,exp)
;;

(* \begin{grammar}
   \item $\call{statement} ::=\\
   \alt \tok{let}_{\textrm{\textvisiblespace}}^{\textrm{\textvisiblespace}}\ \call{let\_binding}\\
   \alt \call{exp}$
   \item $\call{statements} ::= \call{statement}\ ({}_{\backslash{}n}\call{statement})*$
   \item $\call{pattern\_matching} ::= (\call{pattern}\
   {}^\textrm\textvisiblespace\tok{->}^{\backslash{}n}\ \call{statements})+$

   \item $\call{lambda} ::= \tok{\{}^{\backslash{}n}\ \call{pattern\_matching}\ {}^{\backslash{}n}\tok{\}}$
   \item $\call{statements\_block} ::= \tok{\{}^{\backslash{}n}\ \call{statements}\ {}^{\backslash{}n}\tok{\}}$
   \item $\call{block} ::=\\
   \alt \call{lambda}\\
   \alt \call{statements\_block} $

   \end{grammar}

   This part is the trickiest to parse without using backtracking, and
   relies on the fact that parsing expressions and patterns use the
   same function. The idea is to call [parse_expression]; if it is
   followed by a ["->"], then it was a pattern; else it is an
   expression. A third case may arise where we parse a statement which
   is not an expression (currently, a [let]), but these are quickly
   detected because they use a special keyword as prefix.

   This scheme could be easily extended to allow multiple arrows, as
   in { x -> y -> x + y }, if this syntax extension is considered
   useful. *)

(* This function parses all the statements, up to (and including) the
   following pattern in the match list, or up to the \} if there is no
   following pattern. *)
let rec parse_statements_and_maybe_next_pattern stream =
  let continue_with stmt =
    expect_strong_separation stream;
    let (stmts, maybe_patt) = parse_statements_and_maybe_next_pattern stream in
    (stmt::stmts, maybe_patt)
  in
  if (Token.Stream.peek stream).token = Kwd.let_
  then begin
    let let_tok = Token.Stream.next stream in
    expect let_tok Kwd.let_ ~after_min:Sep.Normal ~after_max:Sep.Normal;
    let (patt,exp) = parse_let_binding stream in
    let stmt =
      { P.func = P.Token(let_tok);
        P.arguments = [patt;exp];
        P.location = P.between_tok_term let_tok exp }
    in
    continue_with stmt
  end
  else
    let pattern_or_expression = parse_expression stream in
    let after = Token.Stream.peek stream in
    if after.token = Kwd.rbrace
    then (* Last statement. *)
      ([pattern_or_expression], None)
    else if after.token = Kwd.arrow
    then (expect after Kwd.arrow ~before_max:Sep.Normal ~after_max:Sep.Strong;
          Token.Stream.junk stream;
          ([], Some (pattern_or_expression,after)))
    else continue_with pattern_or_expression
;;

(* Parse a pattern matching once we know it is a pattern matching. *)
let parse_rest_pattern_matching stream first_patt first_arrow =
  let rec loop patt arrow =
    let (stmts, maybe_patt) = parse_statements_and_maybe_next_pattern stream in
    let stmts =
      { P.func = P.Custom("statements");
        P.arguments = stmts;
        P.location = P.between_terms (List.hd stmts) (List.last stmts)
      }
    in
    let patt_stmts = P.infix_binary_op patt arrow stmts in
    match maybe_patt with
    | None -> [patt_stmts]
    | Some(patt,arrow) -> patt_stmts::(loop patt arrow)
  in
  let patt_stmts_list = loop first_patt first_arrow in
  patt_stmts_list
;;

(* Parse a lambda, between its \{ \}. In this case, we know in advance that
   we expect a match list; this is use in the parse rule of
   [match]. *)
let parse_lambda stream =
  let lbra = Token.Stream.next stream in
  expect lbra Kwd.lbrace ~after_max:Sep.Strong;
  let first_pattern = parse_pattern stream in
  let first_arrow = Token.Stream.next stream in
  expect first_arrow Kwd.arrow;
  let pattern_matching =
    parse_rest_pattern_matching stream first_pattern first_arrow in
  let rbra = Token.Stream.next stream in
  expect rbra Kwd.rbrace ~before_max:Sep.Strong;
  P.delimited_list lbra pattern_matching rbra
;;

(* When we encounter a \{, we do not know whether it only introduces a
   new statement block, or if it is a lambda. This function parses in both
   cases. *)
let parse_block stream =
  let lbra = Token.Stream.next stream in
  expect lbra Kwd.lbrace;
  let stmts, maybe_patt = parse_statements_and_maybe_next_pattern stream in
  match stmts, maybe_patt with
  (* Pattern matching. *)
  | [], Some(patt,arrow) ->
    let pattern_matching = parse_rest_pattern_matching stream patt arrow in
    let rbra = Token.Stream.next stream in
    expect rbra Kwd.rbrace;
    P.delimited_list lbra pattern_matching rbra
  (* Statements.  *)
  | stmts, None ->
    let rbra = Token.Stream.next stream in
    expect rbra Kwd.rbrace;
    { P.func = P.Custom "statements";
      P.arguments = stmts;
      P.location = P.between_terms (List.hd stmts) (List.last stmts) }
  (* Empty block: {} *)
  | [], None ->
    Log.Parser.raise_compiler_error ~loc:lbra.location
      "Error: nothing between { and }"
  | _ -> Log.Parser.raise_compiler_error ~loc:lbra.location
    "Error: a pattern matching must begin by \"<pattern> ->\" "
;;

(* \begin{grammar}
   \item $\addprefix{exp}{\{}{\call{block}}$
   \item $\addinfix{exp}{\{}{f0}{leftassoc}{\call{block}}$
   \end{grammar} *)
ExpTdop.define_prefix Kwd.lbrace parse_block;;
let infix_fun stream left =
  let right = parse_block stream in
  { P.func = P.Custom "apply";
    P.arguments = [left;right];
    P.location = P.between_terms left right }
in ExpTdop.define_infix Kwd.lbrace (infix_when_stuck 0xf000) infix_fun
;;

(* \begin{grammar}
   \item $\addprefix{exp}{match}{\tok{match} \tok{(}^{\backslash{}n} \call{exp} {}^{\backslash{}n}\tok{)} \call{lambda}}$
   \end{grammar}

   As for if, we could remove the () around the expression beeing
   matched. *)
let parse_match stream =
  let match_tok = Token.Stream.next stream in
  expect (Token.Stream.next stream) Kwd.lparen
    ~before_max:Sep.Stuck ~after_max:Sep.Strong;
  let cond = parse_expression stream in
  expect (Token.Stream.next stream) Kwd.rparen ~before_max:Sep.Strong;
  let pattern_matching_block = parse_lambda stream in
  { P.func = P.Token match_tok;
    P.arguments = [ cond; pattern_matching_block ];
    P.location = P.between_tok_term match_tok pattern_matching_block }
in ExpTdop.define_prefix Kwd.match_ parse_match;;

(* \begin{grammar}
   \item $\addprefix{exp}{cast}{\tok{cast}\tok{(}^{\backslash{}n} \call{exp}
   {}^\textrm\textvisiblespace\tok{,}^{\backslash{}n}\ \call{type} {}^{\backslash{}n}\tok{)}}$
   \end{grammar}

   This construction, and its syntax, are still alpha. *)
let parse_cast stream =
  let cast_tok = Token.Stream.next stream in
  check cast_tok Kwd.cast;
  expect (Token.Stream.next stream) Kwd.lparen
    ~before_max:Sep.Stuck ~after_max:Sep.Strong;
  let exp = parse_expression stream in
  expect (Token.Stream.next stream) Kwd.comma ~after_max:Sep.Strong;
  let t = Parser_path.parse_type stream in
  let rparen = (Token.Stream.next stream) in
  expect rparen Kwd.rparen ~before_max:Sep.Strong;
  { P.func = P.Token cast_tok;
    P.arguments = [ exp; t];
    P.location = P.between_toks cast_tok rparen }
in ExpTdop.define_prefix Kwd.cast parse_cast;;

(* \begin{grammar}
   \item $\addinfix{exp}{\{}{e0}{noassoc}{\tok{::}\call{type}}$
   \end{grammar}

   This construction, and its syntax, are still alpha. *)
let parse_annotation stream left =
  let dcolon = Token.Stream.next stream in
  expect dcolon Kwd.doublecolon ~before_max:Sep.Stuck ~after_max:Sep.Stuck;
  let typ = Parser_path.parse_type stream in
  P.infix_binary_op left dcolon typ
in ExpTdop.define_infix Kwd.doublecolon (function
| {Token.With_info.separation_before = Sep.Stuck} -> 0xe000
| _ -> failwith "invalid use of annotation") parse_annotation
;;
