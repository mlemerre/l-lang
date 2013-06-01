(* Copyright 2013 Matthieu Lemerre *)

module Ast = Astdef;;
module Build = Cpsbase.Build;;

module type EXPRESSION = sig
  val transform :
    ?v:Cpsbase.Var.var ->
    Ast.Expression.t ->
    Cpsbase.Var.Occur.maker Ast.Variable.Map.t ->
    (Cpsbase.Var.Occur.maker -> Build.fresh) -> Build.fresh
end

module type RULES = sig
  val transform:
    (Ast.Pattern.t * Ast.Expression.t) list ->
    Cpsbase.Var.occur_maker ->
    Cpsbase.Var.occur_maker Ast.Variable.Map.t ->
    (Cpsbase.Var.Occur.maker -> Build.fresh) -> 
    Build.fresh
end
