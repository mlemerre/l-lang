open Base;;

let lparen = Keyword.add "(";;
let rparen = Keyword.add ")";;
let lbrace = Keyword.add "{";;
let rbrace = Keyword.add "}";;
let colon = Keyword.add ":";;
let coma = Keyword.add ",";;
let pipe = Keyword.add "|";;
let gt = Keyword.add ">";;
let arrow = Keyword.add "->";;
let assigns = Keyword.add ":=";; 
let equals = Keyword.add "=";;
let eq = Keyword.add "==";;
let ne = Keyword.add "!=";;
let lt = Keyword.add "<";;
let le = Keyword.add "<=";;
let ge = Keyword.add ">=";;
let plus = Keyword.add "+";;
let minus = Keyword.add "-";;
let star = Keyword.add "*";;
let slash = Keyword.add "/";;
let semicolon = Keyword.add ";";;

(* Non-symbols keywords. Those that are also OCaml keyword are
   suffixed by _. *)
let forall = Keyword.add "forall";;
let if_ = Keyword.add "if";;
let else_ = Keyword.add "else";;
let match_ = Keyword.add "match";;
let cast = Keyword.add "cast";;
let let_ = Keyword.add "let";;
let and_ = Keyword.add "and";;
let def = Keyword.add "def";;
let type_ = Keyword.add "type";;
let declare = Keyword.add "declare";;
