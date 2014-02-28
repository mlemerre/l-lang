(* Copyright 2014 Matthieu Lemerre. *)

(* This file defines the AST produced after parsing. While it is much
   more abstract than parsetrees produced by the parser, all
   identifiers are still named textually by strings, and some of the
   most complex syntactic sugar (like handling "include") still has to
   be performed before typing. *)

(* Identifiers for base objects.  *)
module Id = struct
  type var = Token.ident
  type constructor = Token.ident
  type modul = Token.ident
end

(* Paths to a module. *)
module Path = struct

  (* Note: contrary to the next level, the paths here are all to a
     base module identifier. *)
  type path = 
  | Relative_to of Id.modul             (* module *)
  | In of path * Id.modul               (* path/module *)
  | Apply of path * path                (* path< path > *)
  | Basic of basic_path                 

  (* These special paths are made special by the parsing; others (like
     Int,Bool...) are identified when analyzing module names. It would
     be clumsy to pass these as tokens, especially Tuple, which is a
     family of constructor. *)
  and basic_path = 
  | Tuple of int
  | Arrow

  (* Also called qualified identifiers. *)
  type 'a with_path = path option * 'a

  (* This alias is used when a path is used to denote the "main" type
     it contains. *)
  type typ = path

end

(* Patterns and Expressions. Note: these could be factorized with the
   Patterns and Expressions of "uniquified" pass, but it does not seem
   that this would be a gain. *)
module Pattern = struct
  type t = 
  | Variable of Id.var
  | Constant of Constant.t
  | Constructor of Id.constructor Path.with_path * t list 
  | Tuple of t list 
end

(* Expressions. Note that there is a small redundancy in the
   constructors here, which could be removed; however simplifying the
   Ast might make reporting of accurate error messages harder, so we
   kept them currently. *)
module Expression = struct
  type t = 
  | Let of Pattern.t * t * t
  | Let_rec of (Id.var * t) list * t
  | Apply of t * t
  | Constant of Constant.t
  | Variable of Id.var Path.with_path
  | Tuple of t list 
  | Function of (Pattern.t * t) list
  | Cast of t * Path.typ
  | Annot of t * Path.typ

  (* Could be simplified to let _ = ... in ..., but this would prevent
     us from issuing a warning when ignoring a result. *)
  | Sequence of t * t 

  (* Could be reduced to apply + func. *)
  | Match of t * (Pattern.t * t) list

  (* Could be reduced to a pattern match. *)
  | If of t * t * t
end

module Module = struct

  (* Exemple: Cons(T,tail:List<T>) *)
  type variant = Id.constructor * (Id.var option * Path.typ) list
  type datatype = variant list

  type definition = 
  | Def_module of Id.modul * module_expr
  | Def_value of Id.var * Expression.t
  | Expression of Expression.t
  | Declare of Id.var * Path.typ

  (* [Def_module] defines a new module, as in [module X = { def y = 3 }]. 
     Note that [module X = ...] is a shorthand for [def module X = ... ]

     [Def_value] defines a new value, as in [def y = 3]. This is a
     shorthand for [def value y = 3].

     [Expression] defines a L expression to be executed "immediately".

     [Declare] allows to declare external variables (defined in C).
     This may change in the future. *)

  and module_statement = 
  | Open of Id.modul
  | Include of Id.modul
  | Def of definition list

  (* [open M] makes the module accessible to current scope. For instance, 

     [module M = { def x = 3 }; open M; def y = x + 1] is equivalent
     to:
     [module M = { def x = 3 }; def y = M.x + 1].

     [include M] makes an alias for every definition of M. For instance,

     [module M = { def x = 3; def Z = {} }; include M] is equivalent
     to:
     [module M = { def x = 3; def Z = {} }; def x = M.x; module Z = M.Z].
     
     [Def] creates new (recursive) definitions. Note that it is
     possible to define e.g. a value which is mutually recursive with
     a module, as in
     
     def module X = {
     def g(x) = f(x)+1
     }
     and f = { 12 -> 0; n -> X.g(n) + 1 } *)

  and module_expr =
  | Defined of module_statement list
  | Alias of Path.path              
  | Functor of Id.modul list * module_expr 
  | Data of datatype

(* [Defined] creates a new module. For instance,

   [module M = { }] creates a new empty module M , while 
   [module M = { module Z = {}; def y = 3 }] creates a new module containing modules Z and y.

   [module M = P] createse a new [Alias]. Alias are just new names for an
   existing module, they do not create a new module. The main
   difference lies in functor application: if F is a functor, then
   [F<M>] and [F<P>] are also equal (and the possible corresponding
   types are equal). To create a new module M with the same contents
   than the one of P, use [module M = { include P }].

   [Functor] creates a new functor (parametrized module). Currently,
   functors are defined by the following syntax:

   [module M<Arg> = ...]

   [Data] creates a new module containing constructors and views for a
   datatype. For instance,

   [module Point = data { Point(x:Int,y:Int) }] creates a new record,
   while

   [module Expr = data { Num(Int); Plus(Expr,Expr); Times(Expr,Expr) }
   defines a sum type.

   TODO: For records types, there will be a shortcut:
   record Point(x:Int,y:Int) *)
end




