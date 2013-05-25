(* Copyright 2013 Matthieu Lemerre *)

open Cpstransform_base;;

module List = Extensions.List;;
module AstVarMap = Ast.Variable.Map;;

module rec Rules : RULES = Cpstransform_rules.Make(Expression)
and Expression: EXPRESSION = struct

  (* [transform ?v exp map kcontext] returns a CPS expression
     obtained from the AST expression [exp]. [map] contains a map
     from AST variables to CPS variables (several AST variables can
     correspond to the same CPS variables). The CPS expression is
     "returned" as a CPS (normal) variable [var], such that if the CPS
     expression and the AST expression were both evaluated, [var]
     would contain the result of the evaluation of [exp].

     CPS transformation is written itself in CPS style, so it takes a
     continuation argument [kcontext] to which [var] is returned.
     [kcontext] represents the context that will use the result of the
     transformation.

     When a [v] argument is given, then this [v] is the [var] bound to
     the result of the CPS expression and is returned. This is used
     when compiling recursive occurrences. *)
  let rec transform ?v exp map kcontext =
    let (exp_,_) = exp in

    (* Only lambda is handled when given an explicit [v] for now. We could
       also handle data structures with a pointer indirection, such as
       injections. *)
    assert( v == None || match exp_ with
    | Ast.Expression.Lambda _ -> true
    | _ -> false);

    match exp_ with

    (*s Base cases. Note that the implementation takes advantage of
      the higher-order abstract syntax (CPS-style friendly) interface
      of the [Cpsbuild] module: often it suffices to pass [kcontext]
      as the continuation argument of the [Cpsbuild] functions. *)

    | Ast.Expression.Variable(var) -> 
      let v = try AstVarMap.find var map
        (* Should not happen: any unbound variable error should have
           been already caught by the type checker. *)
        with Not_found -> Log.Cps_transformation.raise_compiler_error
          "Unbound variable %s" (Ast.Variable.to_string var) in
      kcontext v

    | Ast.Expression.Constant(c) -> Build.let_constant c kcontext
    | Ast.Expression.External(name, _) -> Build.let_external name kcontext
    | Ast.Expression.Injection(j,n,x) ->
      transform x map (fun injectedv ->
        Build.let_inj j n injectedv kcontext)

    (* Casts are just forgotten for now; if the transformation was
       typed, we would have to change the type too. *)
    | Ast.Expression.Cast(_,x) -> transform x map kcontext

    (* Fold on all elements in the tuple, CPS-style. *)
    | Ast.Expression.Tuple(l) ->
      let f accu exp k = transform exp map (fun expv -> k (expv::accu)) in
      List.foldk f [] l (fun vs ->
        Build.let_tuple (List.rev vs) kcontext)

    (*s Detection of primitive operations. *)
    (*i TODO: we do not allow using these primitives outside of an
      apply expression. If they were allowed, they should be
      transformed to a closure calling the primitive with its
      arguments, which is an eta-expansion of the primitive; these
      eta-expansions could be inlined in later phases. But I think
      that this expansion should be written manually, and L has a
      simple syntax to do that. So the AST should reflect this, and
      there should exist a [Apply_primitive] expression distinct from
      apply (this could include "externals"). This [Apply_primitive]
      (or [Call_primitive]) would take several arguments, and [Apply]
      would have only one. i*)
    | Ast.Expression.Apply(((Ast.Expression.Constant c),_),
                           [(Ast.Expression.Tuple([a;b]),_)]) ->
      transform a map (fun av ->
        transform b map (fun bv ->
          let buildf = match c with
            | Constant.Integer_binary_operation(op) ->
              Build.let_integer_binary_operation op
            | Constant.Integer_binary_predicate(pred) ->
              Build.let_integer_binary_predicate pred in
          buildf av bv kcontext))

    (*i TODO: handle functions with several arguments. *)
    | Ast.Expression.Apply(caller,[callee]) ->
      transform caller map (fun callerv ->
        transform callee map (fun calleev ->
	  Build.let_cont kcontext
	    (fun k -> Build.apply_closure callerv k [calleev])))

    | Ast.Expression.Apply(_,_) -> 
      failwith "Appling several arguments not yet implemented (write a test case first)"

    (*s Expressions handling patterns. *)
    (* Note that [Let] is now a special case of [Match], for
       compilation. We keep [Let] because it has a special meaning for
       compilation (it is where generalizations can happen), and
       because [Let] on an incomplete match should not yield a warning
       (at least when we can show that it is impossible to reach the
       other cases). *)
    | Ast.Expression.Let(patt, value, body) ->
      transform value map (fun valuev ->
        Rules.transform [(patt,body)] valuev map kcontext)
    | Ast.Expression.Match(cond, rules) ->
      transform cond map (fun condv ->
        Rules.transform rules condv map kcontext)
    | Ast.Expression.Lambda([patt], body) ->
      (* Note that here we reuse here [v] as the variable bound to the
         lambda expression. *)
      Build.let_lambda ?lambda_var:v
        (fun (k,x) ->
          let ctx_in_lambda v = Build.apply_cont k v in
          Rules.transform [(patt,body)] x map ctx_in_lambda) kcontext

    (* Recursive definitions. This uses the [~v] argument to
       [transform] to require that the value being built is bound to
       the  *)
    | Ast.Expression.Let_rec(var_and_values, body) -> 
      assert ((List.length var_and_values) == 1);
      (* Build [rec_map] and the list of [vs] to build the values. *)
      List.foldk (fun (rec_v_and_values,rec_map) (var,value) k ->
        Build.with_var_in_expression (fun v ->
          (* TODO: Add a CPS construct to allow forward declaration of
             recursive variable (and its type). Using decl-bound
             variables should not count as a free variable. The
             corresponding [Build] construct would replace
             [Build.with_var_in_expression]. *)
          let rec_v = Cpsbase.Var.Occur.rec_maker v in
          let rec_map = AstVarMap.add var rec_v rec_map in
          k ((var,v,value)::rec_v_and_values,rec_map)))
        ([],map) var_and_values (fun (rec_v_and_values,rec_map) ->
          let rec_v_and_values = List.rev rec_v_and_values in
          
          (* Build the values, and the non-recursive map. *)
          List.foldk (fun map (var,v,value) k ->
            transform ~v value rec_map (fun v ->
              let map = AstVarMap.add var v map in
              k map)) map rec_v_and_values (fun map ->
                
                (* Build the body. *)
                transform body map kcontext))
    (*i One-variable case. 
    | Ast.Expression.Let_rec([var,value], body) ->
      Build.with_var_in_expression (fun v ->
        let rec_v = Cpsbase.Var.Occur.rec_maker v in
        let rec_map = AstVarMap.add var rec_v map in
        transform ~v value rec_map (fun v ->
          let map = AstVarMap.add var v map in
          transform body map kcontext)) i*)

end 

let transform = Expression.transform;;

(*i TODO: 
   
   - Implement tail CPS transformation (see Compiling with
   continuations continued, by A. Kennedy); this should be done after
   implementing shrinking reductions (so as to test shrinking
   reductions). Ideally, the transformation should be one-pass, in the
   sense that it does not require further shrinking reductions by
   itself.

   - Mutual recursion is not yet implemented, and single recursion is
   implemented only for lambdas. The ?v optional argument seems a
   bit like a hack that will not work so well for
   mutually-recursive functions and values.

   Proper implementation would require either support for forward
   declarations in the CPS language (ideal), or support of
   mutually-recursive values, and maybe changes to Cpsbuild to allow
   building values separately from builting their "let" (this would
   also be useful for building definitions of static values, as needed
   by the "staticalisation" phase).

i*)


