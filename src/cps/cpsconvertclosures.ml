(* Copyright 2012 Matthieu Lemerre *)

(* This file performs \emph{closure conversion}, i.e. removes the
   \emph{free variables} from [Lambda] constructs. For a general
   introduction to closure conversion in CPS form, see the book
   "Compiling with continuations", by Andrew Appel.

   \paragraph{Closure conversion: removing free variables}

   The [Lambda] construct allows dynamic creation of nested functions
   with \emph{free} variables. A variable $v$ is \emph{free} in an
   expression if it is not \emph{bound} in that expression. A variable
   [x] is bound if it appears below a [Lambda], [Let_cont], or
   [Let_prim] construct that binds [x], or it is a global variable
   (i.e. it has been previously bound by [Def]).

   For instance if we considered only the term $e = $ [halt(z)],
   then [z] is free in $e$ (there is no operator that binds it in
   $e$). However if we consider an enclosing term $e'$: [let z =
   x + y in halt(z)], then [z] is bound in $e'$, while [x] and [y] are
   free. Thus, the fact that a variable is bound or free depends on
   the term we consider.

   An term is said to be closed if it contains no free variable.
   For instance, the term

   [ let f = { k (x,y) -> k(x) } in halt(f) ]

   is closed. The goal of closure conversion is to close all terms
   defining a [Lambda], because those terms cannot be compiled as is.
   It is achieved by transforming the code so that variables free in a
   [Lambda] variables are passed as arguments, in structures known as
   \emph{environments}. A \emph{closure} is the combination of a
   function and its environment.

   Closure conversion is an important pass towards machine-code
   compilation: machine code allows to define functions, but does not
   allow nested functions nor free variables. Closure conversion
   performs three kinds of transformations:

   \begin{itemize}

   \item {\bf Creating the environment}: Given a [Lambda] construct [l], make a
   tuple containing every variable needed by the [Lambda] (the
   \emph{environment}), together with the Lambda. I.e. transforms

   "[let f = { k -> x -> ... } in ... ]"\\  into
   
   "[let f = { k -> x -> ... } in]\\
   [ let env = (... free variables of f ...) in]\\
   [ let f' = (f,env) in]\\
   [ ... (code using f' instead of f)...]"

   Note that if the body of [l] also defines a [Lamdba] construct that
   refers to a variable [x], and [x] is free in [l], then the
   environment for [l] must include [x]; and [l] will need to pass the
   variables to the inner [Lambdas].

   \item {\bf Passing the environment}: As lambdas have been replaced by pairs,
   [Apply] is transformed to retrieve the function from the pair, and
   pass the environment as an extra argument. I.e. it transforms
   
   "[f'(k,x)]" into

   "[let (f,env) = f' in ]\\
   [ f(k,env,x)]"

   The choice to pass [env] as an extra argument, rather than
   replacing the [x] argument by a [(env,x)] pair (as was done in an
   earlier design), simplifies closure conversion. For instance if
   [body] contains another [Lambda] construct [l] which uses [x] (i.e.
   [x] is free in [l]), then when creating the environnement for [l],
   one should not use [x], but [#1(p)], where [p] is the new argument
   of [f] containing the [(env,x)] pair.

   \item {\bf Using the environment}: The last transformation retrieves the
   environment given as an argument (together with the actual
   argument), and uses variables of the environment instead of the
   free variables they represent. I.e. it transforms

   [ { k -> x -> ... } ] into

   [ { k -> (env,x) -> 
   ... use variables in env rather than free variables ... } ]

   \end{itemize} 

   \paragraph{Closure conversion and direct recursion}

   The algorithm above is sufficient to handle recursion through the
   use of a fixpoint combinator. However, some extra steps are
   necessary to handle direct recursive and mutually-recursive
   functions: recursive occurrences are handled differently than other
   free occurrences.

   When a function [f] is recursive, [f] is considered as a free
   variable in [f]. If we followed the algorithm above, then [f]
   should be passed with the environment; actually, so that [f] can be
   called in [f], we should pass "[f] + the environment of [f]" in the
   environment of [f]. It is possible, but requires handling of
   mutually-recursive values:

   [let f = { (env,arg) -> ]\\
   [          let f' = #i(env) in]\\
   [          ...use f' instead of f...} in]\\
   [let env = (..., f', ...)]\\
   [and f' = (f, env) in]\\
   [...use f' instead of f...]

   A simpler alternative is to recreate the closure for [f] inside
   [f], using the environment given as an argument:

   [let f = { (env,arg) -> ]\\
   [          let f' = (f,env) in]\\
   [          ...use f' instead of f...} in]\\
   [let env = (... without f' ...) in]\\
   [let f' = (f, env) in]\\
   [...use f' instead of f...]

   This is the approach we chose. It has several advantages; the
   environment is smaller, and the backend need only to support
   recursive closed functions; it does not need to support recursive
   values. Typing is simplified: there is no need for things like
   equirecursive types. Shrinking reductions of tuples can further
   optimize recursive calls. Finally, the approach extends simply to
   mutually recursive functions, as described below.

   (Note: mutual recursion is not yet implemented by the CPS AST, nor
   the backend).

   Mutually recursive functions will share the same environment:

   [let f = { (env,arg) -> ]\\
   [          let f' = (f,env) in]\\
   [          let g' = (g,env) in]\\
   [          ...use f' and g' instead of f and g...} in]\\
   [let g = { (env,arg) -> ]\\
   [          let f' = (f,env) in]\\
   [          let g' = (g,env) in]\\
   [          ...use f' and g' instead of f and g...} in]\\
   [let env = (... variables free in f or g ...) in]\\
   [let f' = (f, env) in]\\
   [let g' = (g, env) in]\\
   [... use f' and g' instead of f and g ...]

   \paragraph{Closure representation}

   \subparagraph{Closure-passing vs environment passing}

   We have chosen the environment-passing variant of closure
   conversion: the environment is a separate block, to which the
   closure points. Another alternative is the closure-passing variant,
   where the closure and the environment are in the same memory block
   (i.e. the same tuple definition).

   The advantage of the environment-passing is simplicity, in
   particular regarding environment sharing when there is mutual
   recursion, and regarding the types of the translation (e.g. there
   is no need for equirecursive types).

   The closure-passing variant is more efficient, but requires more
   machinery. The book "Compiling with continuations", by Andrew Appel
   describes a closure-passing variant where closures are shared using
   a simple addition to the CPS language.

   \subparagraph{Flat and linked closures}

   When lambdas are nested, there are two ways of arranging free variables
   in an environment:

   \begin{itemize}

   \item either the closure is \emph{flat}, i.e. all the free
   variables for the function are directly in the tuple environment;

   \item or the closure is \emph{linked}, i.e. free variables that are
   also free in the enclosing lambda are not put directly in the
   environment; instead the environment points to the environment of
   the enclosing lambda.

   \end{itemize}

   We chose to implement flat closures, because they are simpler, have
   more predictable performance, especially wrt. garbage collection.
   However, it could be useful to have some closures linked in some
   cases (for instance when all the free variables in the enclosing
   closure are used in the enclosed closure).

   \paragraph{Closure conversion and CPS}

   One of the advantage of performing closure conversion at the CPS
   level (rather than the AST level) is that some closures may have
   already been simplified (removed, inlined, or contified), some
   variables may have been simplified, so there are less closures to
   transform, with a smaller environment.

   The representation of CPS of Andrew Kennedy is well-suited to many
   transformations, but closure conversion is not one of them. The
   main advantage of this representation is that it allows, in
   constant time, to make all occurrences of one variable refer to
   another variable instead. If this is very useful for the "Creating
   the environment" and "passing the environment" transformations, it
   does not suit the "using the environment pass": this pass changes
   the occurrences in the body of a Lambda [l] of a variable [v] free
   in [l] into occurrences of a variable in the environment. But there
   may be occurrences of [v] outside of the body of [l], that must not
   be changed.

   The CPS representation still works, but we cannot avoid traversing
   the term to perform the replacements.

   \paragraph{Future improvements}

   \subparagraph{Constant handling}

   When a free variable is a bound to a constant, we should not need
   to pass it in the environment. We could re-create the binding in
   the lambda during closure conversion. But we could avoid doing that, by
   performing closure conversion after a "global constantization"
   pass, that would merge the constants and put them in global
   variables (to avoid duplication). Then there would be nothing
   special to do here to avoid passing constants in environment (as we
   handle global variables). *)

(*s Start of the algorithm.  *)

open Cpsbase;;

let top_convert t = 

  (* The algorithm uses two pass, and is thus linear on the size of
     the CPS AST. The first pass is only for analysis, and maps each lambda
     to the set of free variables it uses. Recursive occurrences of a
     function are considered free by this function. *)
  let (_,free_map) = Cpsfree.term t in

  (*s The main conversion function. The [f] argument tells which
    variables are to be used in place of the free variables; [t] is
    the term to convert. *)
  let rec convert f t = match Term.get t with
    | Apply(_,_,_,_) ->  pass_environment t
    | Let_prim(x,(Value(Lambda(Closure,k,[arg],lambda_body)) as prim),body) ->
      begin

        (* The set of free vars returned by [Cpsfree] contains
           variables recursively used, and variables that are not.
           Only the non-recursive variables are passed in the
           environment. *)
        let free_vars_set = Var.Var.Map.find x free_map in
        let free_vars_list = Var.Var.Set.elements free_vars_set in
        let currently_defined_bindings = [x] in
        let (rec_free_vars,nonrec_free_vars) =
          List.partition (fun elt -> List.memq elt currently_defined_bindings)
            free_vars_list in

        (* Note: the order of the variables in the environment is the
           one in [nonrec_free_vars], and is not important; what is
           important is that the same order is used when the
           environment is created than when it is used. *)
        create_environment x body nonrec_free_vars f;
        use_environment t rec_free_vars nonrec_free_vars
      end
    | _ -> ()

  (*s Transform [Apply] terms to retrieve the function and pass the
    environment. *)
  and pass_environment t =
    (* Retrieve the variables *)
    let Apply(_,focc,kocc,[xocc]) = Term.get t in
    let f = Var.Occur.binding_variable focc in
    let k = Cont_var.Occur.binding_variable kocc in
    let x = Var.Occur.binding_variable xocc in
    (* Delete apply and its occurrences, and return a dangling term *)
    let dangling_t = Change.delete_apply t in
    (* Insert "let (func,env) = f; apply func k env pair" *)
    ignore(
      Build.match_pair ~reconnect:dangling_t (Var.Occur.maker f) ( fun (func,env) ->
        Build.apply_function func (Cont_var.Occur.maker k) [env;Var.Occur.maker x]))

  (*s Transform [Lambda] into pairs of [Lambda] without free variable,
    and environment. *)
  and create_environment x body nonrec_free_vars f =
    let (replace_body,reuse_body) = Change.disconnect body in

    (* [nonrec_free_vars] contains the list of variables that should
       be put in the environment. If we detected that a variable [x]
       should be in the environment, but [x] is being replaced by
       [f(x)], then the environment should contain [f(x)] instead of
       [x]. *)
    let env_vars = List.map (fun free_var ->
      match f free_var with
      | None -> Var.Occur.maker free_var
      | Some(v) -> v ) nonrec_free_vars in

    ignore(
      Build.let_tuple ~reconnect:replace_body env_vars (fun env ->
        Build.with_var_in_term ( fun pair ->

          (* We create the [pair] variable, and make all non-recursive
             occurrences of [x] occurrences of [pair]. [pair] is bound
             by [let_pair] in a "let pair = (x,env); body" term. This
             operation creates a new occurrence of [x]; therefore the
             [replace_all_non_recursive_occurrences_of_with] operation
             must be done before calling [let_pair] (else the
             occurrence of [x] would become an occurrence of [pair],
             i.e. the term would become "let pair = (pair,env); body",
             which would be incorrect.) *)
          Change.replace_all_non_recursive_occurrences_of_with x pair;
          Build.let_pair ~var:pair (Var.Occur.maker x,env) (fun pair ->
            reuse_body))))


  (*s Replace the body of the lambda to use variables passed in the
    environments instead of the free variables. *)
  and use_environment term rec_free_vars nonrec_free_vars =
    let Let_prim(x,(Value(Lambda(Closure,k,[arg],lambda_body)) as prim),_)
        = Term.get term in

    (*c Add a new [env_arg] argument to the lambda. *)
    ignore( Build.with_var_in_term (fun env_arg ->
      Change.update_function_type_and_arguments term No_environment [env_arg; arg];

      (*c Retrieve parameters from [env_arg]. *)
      let (lambda_body_reconnect,lambda_body_reuse)
          = Change.disconnect lambda_body in
      let num_env_vars = List.length nonrec_free_vars in
      Build.match_tuple ~reconnect:lambda_body_reconnect
        num_env_vars (Var.Occur.maker env_arg) (fun vars ->

        (*c We map free variables to their replacement of the
          environment using a simple association list. We could use
          another structure, as [num_env_vars] may be large. *)
        let assoc_list = List.combine nonrec_free_vars vars in

        (*c For each function [x] being currently defined, we define a
          replacement for [x], using [env_arg] as the environment.
          This assumes that all recursive functions use the same
          environment. *)
        let g f =
          let rec loop accu = function
            | [] -> f (List.rev accu)
            | x::rest ->
              Build.let_pair (Var.Occur.rec_maker x, Var.Occur.maker env_arg) (fun p ->
                loop ((x,p)::accu) rest)
          in loop [] rec_free_vars
        in g (fun rec_assocs ->

          (*c Builds [f], the replacement function.  *)
          let assoc_list = rec_assocs@assoc_list in
          let f var =
            try Some (List.assq var assoc_list)
            with Not_found -> None in

          (*c We combine the replacement and the conversion in a
            single pass. *)
          let do_on_term t =
            Change.replace_some_occurrences_in_one_term t f;
            convert f t in
          Traverse.iter_on_terms ~enter_lambdas:false lambda_body_reuse do_on_term;

          lambda_body_reuse));
      term))
  in

  (* Note that for top-level definitions, there are no occurrences to
     convert; hence [f] always return [None], and we do not call the
     [replace_occurrence_in_one_term] function. *)
  let f = (fun _ -> None) in
  Traverse.iter_on_terms ~enter_lambdas:false t (convert f);;
