(* Copyright 2013 Matthieu Lemerre. *)

(* This module performs CPS transformation of definitions; it is
   similar to the transformation proposed in "Compiling with
   continuations, continued" by A. Kennedy.

   CPS transformation names all intermediary results, and fixes the
   order of evaluation; so it is in this module that we define that
   arguments to functions are evaluated from left to right, that a
   function is evaluated before its arguments, how pattern matching is
   compiled...

*)

module Build = Cpsbase.Build;;
module Ast = Ast.Def;;
module AstVarMap = Ast.Variable.Map;;

let expression = Expression.Expression.transform;;

(* Transform an expression (typically the expr in Ast.Def(x,expr) or
   Ast.Nodef(expr)) to a CPS definition *)

(* Transform a definition. Returns a list of cps definitions, and the
   new environment. *)
let transform env def =
  let halt_ctx = (fun x -> Build.halt x) in
  match def with

  (* Definitions whose result is not used. *)
  | Ast.Definition.Expression(exp) 
  | Ast.Definition.Value([Ast.Pattern.Wildcard, exp]) ->
    [Cpsbase.Definition( Cpsbase.Unused,
                         Cpsbase.Dynamic_value (expression exp env halt_ctx))],
    env

  (* Non-recursive definitions. *)
  | Ast.Definition.Value([Ast.Pattern.Variable var,exp]) ->
    let e = expression exp env halt_ctx in
    let definition = Build.with_var_in_def (fun v ->
      Cpsbase.Definition(Cpsbase.Public v, Cpsbase.Dynamic_value e)) in
    let Cpsbase.Definition(Cpsbase.Public v,_) = definition in
    ([definition], AstVarMap.add var (Cpsbase.Var.Occur.maker v) env)

  | Ast.Definition.Value(_) -> 
    failwith "Definitions with general patterns are not yet supported"

  (* Definitions are not directly recursive; i.e. [def id = ...id...]
     is transformed into [def id = {let id = ... id ...; id}]. This is
     because LLVM output is (currently) not able to deal with
     recursive definitions with dynamic values, because of the
     additional indirection. *)
  | Ast.Definition.Value_rec([var,exp]) ->
    let def = Build.with_var_in_def (fun defv ->
      let e = Build.with_var_in_expression (fun v ->
        let rec_v = Cpsbase.Var.Occur.rec_maker v in
        let rec_map = AstVarMap.add var rec_v env in
        expression ~v exp rec_map (fun v ->
          halt_ctx v)) in
      Cpsbase.Definition(Cpsbase.Public defv, Cpsbase.Dynamic_value e)) in
    let Cpsbase.Definition(Cpsbase.Public defv,_) = def in    
    ([def], AstVarMap.add var (Cpsbase.Var.Occur.maker defv) env)

  | Ast.Definition.Type_name (_,_,_) -> ([],env)

(* TODO:

   - We can extend [Halt] to handle several variables, with a
   construct like [Halt of def_vars * expr_vars]. This would be useful
   for both [Value_rec] with several variables, and for more general
   patterns (that can bind several variables at once).

   - (Maybe). We could have a [value] function to build values, and
   only those would have the ~v argument (or a list of ~v). 

   - Allow mutual recursions, mainly for functions but also for
   objects, perhaps through forward declarations. This is related to
   making the allocations explicit, and to typing (to know the size of
   the allocations). A forward declaration corresponds to allocation
   of object, which can be separated from its initialization.

   - Link each CPS variable to its corresponding subterm. This would
   allow to display proper error messages if there is an error after
   the CPS transformation, and could help put debugging information
   in the compiler. In addition, when the ADT is typed, type
   information should be transformed to CPS.

*)
