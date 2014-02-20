(* Copyright 2013 Matthieu Lemerre *)

(*p \include{../../../doc/grammardefs} *)

(* This module implements the TDOP-based parser of L paths and module
   names.

   \paragraph{Module names}

   Module names are identifiers that allow to name a module. In
   particular, in L a module can contain at most 1 type; and we also
   use module names to represent the type that they represent. Modules
   that contain a type are called classes.

   Functors are functions that take modules as arguments, and return a
   new module. Functors in L are applicative, meaning that a functor
   given the same arguments return the same module. Applicative
   functors are necessary if we want to use module names to represent
   types. The notation for functor application is like a function
   call, with [(...)] replaced by [<...>]. As functors are applicative, it
   makes sense to use functor application to name a module (because
   applying a functor always refers to the same module). So one can
   see the BNF of module names as the following one:

   \begin{itemize}
   \item $\call{module\_name} ::= \\
   \alt \call{id}\\ \alt \call{id}\tok{<}
   \call{module\_name} (\tok{,} \call{module\_name})* \tok{>}$
   \end{itemize}

   \paragraph{Paths}

   Modules can contain components, and it is thus necessary to be able
   to refer to components inside of a module. As modules can contain
   other modules, there can be a complete \emph{path} describing how
   to get down into modules to retrieve the components. Paths are
   composed of a sequence of module names, each one followed by a "/";
   then by the name of the component. By analogy with filesystem
   paths, we name the first part the "directory" part.

   Adding paths changes the BNF as in the following:

   \begin{itemize}
   \item $\call{module\_name} ::= \\
   \alt \call{id}\\
   \alt \call{id}\tok{<} \call{path\_to\_module\_name}
   (\tok{,} \call{path\_to\_module\_name})* \tok{>}$
   \item $\call{dir} ::= (\call{module\_name}\tok{/})*$
   \item $\call{path\_to\_module\_name} ::= \call{dir}\call{module\_name}$
   \end{itemize}

   \paragraph{Type constructors}

   Two families of functors have a special syntax: [->], and [(...)].
   [->] corresponds to the function type constructor ([->] in OCaml),
   and (...) to the tuple type constructor ([*] in OCaml). They are
   actually functors, that construct a class that contains
   the constructed type obtained by taking the type part of the
   arguments (that must be classes). They are used when module names
   are used to denote the type they contain.

   Even if they are normal functors, there is a syntactical
   restriction: they can appear in paths only as arguments to a
   functor. Paths like [(Int -> Int,Float)/to_string] are thus
   forbidden.

   This makes the BNF look like the following one:
   \begin{itemize}
   \item $\call{module\_name} ::= \\
   \alt \call{id}\\
   \alt \call{id}\tok{<} \call{path\_to\_module\_name\_allow\_type\_constr}
     (\tok{,} \call{path\_to\_module\_name\_allow\_type\_constr})* \tok{>}$
   \item $\call{dir} ::= (\call{module\_name}\tok{/})*$
   \item $\call{path\_to\_module\_name} ::= \call{dir}\call{module\_name}$
   \item $\call{path\_to\_module\_name\_allow\_type\_constr} ::= \\
     \alt \call{path\_to\_module\_name} \\
     \alt \call{path\_to\_module\_name\_allow\_type\_constr}
            \tok{->} \call{path\_to\_module\_name\_allow\_type\_constr} \\
     \alt \tok{(} \call{path\_to\_module\_name\_allow\_type\_constr}
            (\tok{,} \call{path\_to\_module\_name\_allow\_type\_constr})* \tok{)}$
   \end{itemize}

   There are several reasons for this limitation. The first is
   technical: the first form would require mixing the path and
   expression parsers, which would delay the detection of parse
   errors. The limitation keeps the path and expression grammars
   separated, and allows parsing of paths to remain in LL(2): it
   suffices to peek at the next to token, and check that the second
   token is a stuck < or / (this requires that the grammar of the
   element following the path cannot use stuck / or < after
   (upper-case) identifiers).

   The second reason is readability, because the [(Int ->
   Int)/to_string] form would be highly uncommon and would look
   particularly obscure.

   If one wants to use tuples and arrows in path, they can just use a
   named version, as in the following:

   [module Pair<M1,M2> = (M1,M2)]
   [module Arrow<M1,M2> = M1 -> M2]
   [Pair<Arrow<Int,Int>,Int>/to_string ]

   \paragraph{Types}

   In L, there is no need to have a special grammar for types: because
   modules can hold at most one type, we can use them to represent the
   type they contain. So we have

   \begin{itemize}
   \item $\call{type} = \call{path\_to\_module\_name\_allow\_type\_constr}$
   \end{itemize}

   As $\call{path\_to\_module\_name\_allow\_type\_constr}$ is a
   lengthy name, we use $\call{type}$ everywhere and define
   $\call{path\_to\_module\_name\_allow\_type\_constr}$ to be an alias
   for it. So note that in some contexts, $\call{type}$ can denote a
   path to a module name that may not contain any type. *)

open Common;;
open Token.With_info;;

(****************************************************************)
(* Module names and paths. *)

(* Difference with path: -> and () are allowed. *)
let r_parse_type = ref (fun _ -> assert false);;

(* \begin{grammar}
   \item $\call{module\_args} ::=
   \tok{<}^{\backslash{}n} \call{type} ({}^{\textrm{\textvisiblespace}}\tok{,}^{\backslash{}n} \call{type})* {}^{\backslash{}n}\tok{>}$
   \end{grammar}

   Module args is when $\call{type}$ can appear in paths. *)
let parse_module_args stream left =
  let lt = Token.Stream.next stream in
  expect lt Kwd.lt ~after_max:Sep.Strong;
  let type_list =
    parse_comma_separated_list stream !r_parse_type in
  let gt = Token.Stream.next stream in
  expect gt Kwd.gt ~before_max:Sep.Strong;
  let right = P.delimited_list lt type_list gt in
  { P.func = P.Custom "modapply"; P.arguments = [ left; right ];
    P.location = P.between_terms left right }
;;

(* \begin{grammar}
   \item $\call{path} ::= \\
   \alt \call{module\_id} \\
   \alt \call{path}\tok{/}^\nleftrightarrow\call{module\_id} \\
   \alt \call{path}\call{module\_args} $
   \end{grammar} *)

(* Note: [parse_path] is parsed quite differently from
   [parse_path_to]:

   \begin{itemize}
   \item In [parse_path_to], we need to detect whether we are on a path;
     there is a trailing "/" introducing the following element (which
     will be an id)

   \item In [parse_path], we know that we are on a path, and there is no
     trailing "/".
   \end{itemize}

   Thus we have two different functions for parsing these two, even if
   the grammar is quite similar.

   Extending [Type_tdop] (below) to handle [parse_path] does not work,
   because we want to forbid things like A/(B,C) and (A,B)/C ( (A,B)
   is a valid type, but not a valid path). Note that we could have
   detected there erroneous constructs at a later phase. *)
let parse_path stream =
  let rec loop leftb =
    let module_id = Token.Stream.next stream in
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
    if not (is_upper_id module_id.token)
    then Log.Parser.raise_user_error ~loc:module_id.location "expected upper id";
    let left = leftb (P.single module_id) in
    let left =
      let rec loop_on_calls left =
        let following = Token.Stream.peek stream in
        if following.separation_before = Sep.Stuck && (following.token = Kwd.lt)
        then parse_module_args stream left
        else left  in
      loop_on_calls left in
    let following = Token.Stream.peek stream in
    if following.token = Kwd.slash then
      (expect_stuck_separation stream;
       Token.Stream.junk stream;
       let leftb x = P.infix_binary_op (leftb left) following x in
       loop leftb)
    else left in
  loop (fun x -> x)
;;



let parse_path_to parsefun stream =
  let rec loop leftb =
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
    if not (is_upper_id module_id.token
            && following.separation_before = Sep.Stuck
              && (following.token = Kwd.slash || following.token = Kwd.lt))
    then leftb (parsefun stream)
    else
      (* We got a path! *)
      (  Token.Stream.junk stream;
         let left = P.single module_id in
         if following.token = Kwd.slash then
           (Token.Stream.junk stream;
            let leftb x = P.infix_binary_op (leftb left) following x in
            loop leftb)
         else
           (assert (following.token = Kwd.lt);
            let left = leftb left in
            let left = parse_module_args stream left in
            let slash = (Token.Stream.next stream) in
            (* TODO: Parse args in a loop; we should allow P<X><X> *)
            expect slash  Kwd.slash ~before_max:Sep.Stuck ~after_max:Sep.Stuck;
            loop (fun x -> P.infix_binary_op left slash x)))
  in loop (fun x -> x)
;;

(****************************************************************)
(* Types are paths with two added constructs, [(...)] and [->]. *)

(*\begin{grammar}
  \item $\call{type} ::= \call{path}$
  \end{grammar} *)
module Base_type = struct
  type t = P.term
  let int_handler n _ =
    Log.Parser.raise_compiler_error "Found an int while parsing a type"
  let ident_handler _id stream = parse_path stream
end;;

module Type_tdop =
  Tdop.Make(Base_type);;
let parse_type stream = Type_tdop.parse stream 0;;
r_parse_type := parse_type;;

(* \begin{grammar}
   \item $\call{tuple\_type} ::= \\
    \alt \tok{(}^{\backslash{}n}{}^{\backslash{}n}\tok{)}\\
    \alt \tok{(}^{\backslash{}n} \call{type} ({}^{\textrm{\textvisiblespace}}\tok{,}^{\backslash{}n} \call{type})* {}^{\backslash{}n}\tok{)}$
   \item $\addprefix{type}{(}{\call{tuple\_type}}$
   \end{grammar} *)
let parse_tuple_type stream =
  let (lpar,rpar,list_paths) =
    parse_tuple_generic stream parse_type in
  P.delimited_list lpar list_paths rpar
in Type_tdop.define_prefix Kwd.lparen parse_tuple_type;;

(*\begin{grammar}
  \item $\addinfix{type}{->}{80}{rightassoc}{
  \ {}^{\textrm{\textvisiblespace}}\tok{->}^{\backslash{}n}\ \call{type}}$
\end{grammar}*)
Type_tdop.define_infix_right_associative
  Kwd.arrow (infix_when_normal 0x8000)
  (fun tok ~left ~right -> P.infix_binary_op left tok right)
;;

(*\begin{grammar}
  \item $\call{path\_allow\_type\_constr} ::= \call{type}$
  \end{grammar}

  $\call{type}$ should be used when what is parsed really denotes a
  type; when a module is needed (which may not contain a type), the
  $\call{path\_to\_module\_name\_allow\_type\_constr}$ is more suited. *)
let parse_path_allow_type_constr stream =
  Type_tdop.parse stream 0;;
