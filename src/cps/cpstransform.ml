(* Copyright 2013 Matthieu Lemerre *)

module AstVarMap = Ast.Def.Variable.Map;;
let definition = Cpstransformpack.Definition.transform;;
let initial_env:Cpsbase.Var.Occur.maker AstVarMap.t = AstVarMap.empty;;

let from_stream ast_stream = 
  Extensions.Stream.transformer definition initial_env ast_stream 
