(* Copyright 2013 Matthieu Lemerre *)

(* This module implements the TDOP-based parser of L paths and module
   names. Module names are identifiers that allow to name a module. In
   particular, in L a module can contain at most 1 type; and we also
   use module names to represent the type that they represent.

   Paths allow to refer to components inside of a module. They are
   composed of a sequence of module names separated with "/", with an
   additional syntactic restriction: module names in a path may only
   be of the form [Module] or [Module<Module_name,...Module_name>]. In
   other words, module names of the form
   [(Module_name,...,Module_name)] or [Module_name -> Module_name] may
   appear in a path only as arguments of a Module. This restriction
   makes the code more readable, and allows parsing of paths to remain
   in LL(2). *)

(*p 
  \newenvironment{grammar}{\begin{itemize}}{\end{itemize}}
  \newcommand{\tok}[1]{"{\bf #1}"}
  \newcommand{\call}[1]{<#1>}
*)

open Parser_base;;
open Token.With_info;;

(****************************************************************)
module Base_module_name = struct
  type t = P.term
  let int_handler n _ =
    Log.Parser.raise_compiler_error "Found an int while parsing a path"
  let ident_handler id stream =
    let tok = Token.Stream.next stream in
    P.single tok
end;;

module Module_name_tdop = Tdop.Make(Base_module_name);;

let parse_module_name stream = Module_name_tdop.parse stream 0;;

(* \begin{grammar}
   \item $module\_name\_with\_path ::= \call{module\_name} (\tok{/} \call{module\_name})*$
   \end{grammar} 
   Note: we cannot just define [parse_module_name_with_path =
   parse_with_path parse_module_name]: this would parse 
   [module_name]s greedily, and then fail because a trailing "/" 
   would be required. 

   Note: "/" can be seen as a low-priority path operator.
*)
let rec parse_module_name_with_path stream = 
  let mn = parse_module_name stream in
  let tok = Token.Stream.peek stream in
  if tok.Token.With_info.token = Kwd.slash 
  && tok.separation_before = Token.Separation.Stuck
  then (Token.Stream.junk stream;
        P.binary_op tok mn (parse_module_name_with_path stream))
  else mn
;;

(* In L, there is no need to have a special grammar for types: because
   modules can hold at most one type, we can use them to represent the
   type they contain. We use this alias when module names are used
   this way. *)
let parse_type = parse_module_name_with_path;;


(* \begin{grammar}
   \item $tuple\_type ::= 
   \tok{(} ( \epsilon{} | \call{type} (\tok{,} \call{type})* )
   \tok{)}$
   \item prefix for $\call{type}: \tok{(} \to \call{tuple\_type}$
   \end{grammar} *)
let parse_tuple_path_and_remove_singleton stream =
  let tok = Token.Stream.peek stream in
  let (lpar,rpar,list_paths) =
    parse_tuple_generic_return_parens stream parse_module_name_with_path in
  match list_paths with
    | [t] -> t
    | _ -> P.delimited_list lpar rpar list_paths
in Module_name_tdop.define_prefix Kwd.lparen parse_tuple_path_and_remove_singleton;

(* \begin{grammar}
   \item modapply ::= $\tok{<} \call{type}
   (\tok{,} \call{type})* \tok{>}$
   \item infix 0xf0 for $\call{type}: \call{type}\tok{<} \to \call{modapply}$
   \end{grammar}  *) 
let parse_tuple_type_infix stream left =
  let lt = Token.Stream.next stream in
  expect lt Kwd.lt;
  let type_list = 
    parse_list_with_sep stream parse_module_name_with_path Kwd.coma in
  let gt = Token.Stream.next stream in
  expect gt Kwd.gt;
  let right = P.delimited_list lt gt type_list in 
  { P.func = P.Custom "modapply"; P.arguments = [ left; right ];
    P.location = P.between_terms left right }
in Module_name_tdop.define_infix Kwd.lt (infix_when_stuck 0xf000) parse_tuple_type_infix;;

(*\begin{grammar}
  \item infix rightassoc 0x80 for $\call{type}: \call{type} {}^{\backslash{}n}
  \tok{->}$
\end{grammar}*)
Module_name_tdop.define_infix_right_associative
  Kwd.arrow (infix_when_normal 0x8000)
  (fun tok ~left ~right -> P.binary_op tok left right)
;;

(* Things like (Int,Int), or Int -> Int, are correct module paths. So
   it should be possible to writing things like
   (Int,Int)/to_string(p). But we constrain paths
   to be of the form:
   path = | epsilon
          | ModuleID/path
          | ModuleID<path module,...,path module>/path

   This allows to write List<Int -> Int>/to_string, but not 
   (Int -> Int)/to_string.

   There are several reasons for this limitation. The first is
   technical: the first form requires mixing the path and expression
   parsers, which would delay the detection of parse errors. The
   second is readability, because the (Int -> Int)/to_string form
   would be highly uncommon and would look particularly obscure.

   If one wants to use tuples and arrows in path, they can just use a
   named version, as in the following:

   module Pair<M1,M2> = (M1,M2)
   module Arrow<M1,M2> = M1 -> M2
   Pair<Arrow<Int,Int>,Int>/to_string 

   With this restriction, to know if the rest of the stream contains a
   path, one has to peek the two next tokens, and check that the
   second is a stuck / or <. This requires that the grammar of the
   element following the path cannot use stuck / or < after
   (upper-case) identifiers. *)

(* \begin{grammar}
   \item $path ::= \epsilon \\ \qquad
   | \call{upper\_id}\tok{/}\call{path} \\ \qquad
   | \call{upper\_id}\tok{<} \call{type} (\tok{,} \call{type})* \tok{>}\tok{/}\call{path}$
   \end{grammar} *)
let rec parse_with_path parsefun stream = 
  let module_id = Token.Stream.peek stream in
  let following = (Token.Stream.peek_nth stream 1) in
  let is_upper id =   
    let id = Token.Ident.to_string id in
    let c = String.get id 0 in 
    let code = Char.code c in
    (Char.code 'A') <= code && code <= (Char.code 'Z')
  in
  let is_upper_id = function
    | Token.Ident(id) when is_upper id -> true
    | _ -> false
  in
  if is_upper_id module_id.token
    && (following.token = Kwd.slash || following.token = Kwd.lt)
    && following.separation_before = Token.Separation.Stuck
  then (let dir = parse_module_name stream in
        let slash = Token.Stream.next stream in
        expect slash Kwd.slash;
        let rest = parse_with_path parsefun stream in
        P.binary_op slash dir rest)
  else parsefun stream
;;
