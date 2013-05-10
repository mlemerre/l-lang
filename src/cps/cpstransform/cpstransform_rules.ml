(* Copyright 2013 Matthieu Lemerre *)

(* This module handles the compilation of a set of \emph{rules}. A
   \emph{rule} is composed of a pattern, matched against a value; and
   an expression (the \emph{body} of the rule), to be executed if the
   pattern succeeds in matching the value.

   The [match(expression){rules}] expression matches an expression
   against a set of rules; its behaviour is to evaluate the expression
   once, and try to match the resulting value against the patterns in
   [rules] until one succeeds, in which case the corresponding body is
   executed.

   The order of the rules is thus important: a value can be matched by
   the patterns of two rules, e.g. [(5,6)] is matched by [(5,_)] and
   [(_,6)]; but only the body of the first rule is executed. This
   implies that two successive rules in a [match] can be reordered if,
   and only if, they are \emph{disjoint}, i.e. they cannot match the
   same value.

   But when allowed, reordering and factorizing the compilation of
   rules matching lead to more compact, faster code. Trying to produce
   the most efficient code for matching against a set of rules is a
   NP-complete problem (the complexity arise when compiling multiple
   independent patterns, for instance tuple patterns). Rather than
   attempting to solve this problem, L specifies how pattern matching
   is compiled, which allows the developper to visualize the costs of
   its pattern matching.

   \subsubsection*{Pattern matching rewrites}

   The compiler maintains a list of patterns that remain to be matched
   for each rule, and a list of values against which each rule is
   matched. The list of the first pattern to be matched in each rules
   is the first column, and is matched against the first value.
   Several cases can occur (see in the book "The implementation of
   functional programming languages" by Simon Peyton Jones, the
   chapter 5: "Efficient compilation of pattern matching", by Philip
   Wadler):

   \paragraph{There is no more column}

   When there are no remaining patterns to match, and no remaining
   values, it means that all the rules match. As pattern matching
   selects the first rule that matches, we execute the body of the
   first rule, and discard the other rules with a warning.

   For instance in

   [match(){]\\
   [  () -> body1]\\
   [  () -> body2]\\
   [}]

   [body1] matches and is executed; [body2] also matches, but is
   superseded by [body1], and is just discarded.

   \paragraph{The column contain only variables} In this case, in
   each rule the variable is bound to the value, and matching
   continues. For instance in

   [match(expr1,...){]\\
   [  (a,...) -> body1]\\
   [  (b,...) -> body2]\\
   [}]

   [a] is bound to [v1] in the first rule, and [b] in the second rule;
   where [v1] is a CPS variable representing the result of computing
   [expr1] in the condition of the match (computation in the condition
   is thus not repeated). Matching then proceeds, starting from the
   second column.

   This rule can be extended to incorporate wildcard ([_]) patterns
   (where nothing is bound), and all \emph{irrefutable} patterns.
   A pattern is irrefutable if it does not contain any \emph{variant}.

   For instance, consider

   [match((expr1,expr2),...){]\\
   [  (a,...) -> body1]\\
   [  (_,...) -> body2]\\
   [  ((c,d),...) -> body3]\\
   [}]

   The column contains only irrefutable patterns. Let [v1] be a
   CPS variable containing the evaluation of [expr1], and [v2]
   containing the evaluation of [expr2]. Then [a] is bound to
   [(v1,v2)], [c] to [v1], and [d] to [v2].

   \paragraph{The column contains only calls to constructors} A
   \emph{constructor} is a specific version of a variant type; for
   instance [3] is a constructor of [Int], [True] a constructor of
   [Bool], and [Cons(1,Nil)] a constructor of [List]<[Int]>.

   Note that if the column contain a variant, then all the
   constructors that it contains are of the same type: this is
   necessary for the pattern matching to typecheck.

   When two contiguous rules have different constructors at the same
   place, they cannot match the same value simultaneously: they are
   thus disjoint, and can be swapped. This allows to group the rules
   according to the constructor in their first column (the order of
   the rules within a group being preserved).

   For instance,

   [match(expr1, expr2){]\\
   [  (Cons(a,Cons(b,Nil)),Nil) -> body1]\\
   [  (Nil,Nil) -> body2]\\
   [  (Nil,c) -> body3]\\
   [  (Cons(d,e),_) -> body4 ]\\
   [}]

   can be grouped as (preserving the order between rules 1 and 4, and
   2 and 3) :

   [match(expr1, expr2){]\\
   [  (Cons(a,Cons(b,Nil)),Nil) -> body1]\\
   [  (Cons(d,e),_) -> body4]\\
   [  (Nil,Nil) -> body2]\\
   [  (Nil,c) -> body3]\\
   [}]

   Then, the matching of contiguous rules with the same constructor
   can be factorized out, as follow:

   [let v1 = expr1 and v2 = expr2 in]\\
   [match(v1){]\\
   [  Cons(hd,tl) -> match((hd,tl),v2){]\\
   \qquad[    ((a,Cons(b,Nil)), Nil) -> body1 ]\\
   \qquad[    ((d,e),_) -> body4 ]\\
   [    }]\\
   [  Nil -> match(v2){]\\
   \qquad[    Nil -> body2 ]\\
   \qquad[    c -> body3 ]\\
   [    }]\\
   [}]

   Note that the L compiler matches the values in the constructor
   before matching the other columns of the tuple, as was exemplified
   in the [Cons] rules.

   The construct of matching only the constructors of a single variant
   type can be transformed directly into the CPS [case] expression. It
   is generally compiled very efficiently into a jump table, and
   dispatching to any rule is done in constant time. (Note that the
   compiler may not generate a jump table if the list of constructors
   to check is sparse).

   \paragraph{The first column contains both refutable and irrefutable
   patterns}

   If the first column contains both kind of patterns, the list of
   rules is split into groups such that the ordering between rules is
   preserved, and either all the rules in the group have their first
   pattern that is refutable, or they are all irrefutable.

   For instance, the following match:

   [match((v1,v2),v3){]\\
   [  (_,1) -> 1]\\
   [  ((a,b),2) -> a+b+2]\\
   [  ((3,_),3) -> ...]\\
   [  ((4,_),_) -> ...]\\
   [  ((_,5),_) -> ...]\\
   [  ((a,b),c) -> a+b+c]\\
   [}]

   is split into three groups, with respectively rules 1-2 ([_] and
   [(a,b)] are both irrefutable patterns), rules 3-5, and rule 6. Then
   the groups are matched successively, the next group being matched
   only if no rule matched in the first one. This amount to performing
   the following transformation:

   [let c = ((v1,v2),v3) in]\\
   [match(c){]\\
   [  (_,1) -> 1]\\
   [  ((a,b),2) -> a+b+2]\\
   [  _ -> match(c){]\\
   \qquad[         ((3,_),3) -> ...]\\
   \qquad[         ((4,_),_) -> ...]\\
   \qquad[         ((_,5),_) -> ...]\\
   \qquad[         _ -> match(c){]\\
   \qquad\qquad[                ((a,b),c) -> a+b+c]\\
   \qquad\qquad[        }]\\
   \qquad[ }]\\
   [}]

   Note that rules 3,4,5 also need to be split and transformed further
   using this method.

   \subsubsection*{Compiling pattern matching}

   \paragraph{Compilation order}

   The order in which checking is made for a set of patterns is a
   choice, done by the compiler. L chooses to match the tuples from
   left to right, and the contents of the constructor as soon as they
   are matched; and to split rules according to the refutability of
   the pattern in their first remaining column. This choice may not be
   optimal in every case (but minimizing the number of matches is a
   NP-hard problem), but allows for a simple, visual analysis of the
   cost of pattern matching. The user is free to rearrange the set of
   patterns to improve performance (possibly guided by compiler
   hints).

   \paragraph{Element retrieval}

   At the beginning of a match, all the components, needed by at least
   one rule, that can be retrieved (i.e. components in a tuple etc.,
   but not those that are under a specific constructor) are retrieved.
   When a constructor is matched, all the components that can be
   retrieved that were under this constructor are retrieved. This
   behaviour produces the most compact code (avoid duplicating
   retrieval of elements in the compilation of several rules), but
   maybe not the most efficient (sometimes elements are retrieved that
   are not used). Optimizations, such as shrinking reductions, are
   allowed to move down or even duplicate code performing retrieval of
   elements into the case.

   \paragraph{CPS}

   Compilation of pattern matching is done during the CPS
   transformation, which transforms source code from the AST language
   to the CPS language. There are several reasons for that:

   \begin{itemize}

   \item The CPS transformation of expression fixes the order of their
   evaluation; compiling pattern matching fixes the order in which
   patterns are matched. So it makes sense to do both at the same
   time, to have a single pass that fixes all the order of evaluation.

   As a side note, it makes sense to keep pattern matching in the AST
   language, because patterns are easy to type, and any typing error
   can be more easily returned to the user.

   \item The CPS language provides continuations, which allows to
   express explicit join points in the control flow, something not
   possible in the AST language (without creating new functions).
   These joint points are necessary notably to factorize the
   compilation of pattern matching (this problem is similar to
   compilation of boolean expressions with the short-circuit operators
   \verb^&&^ and \verb^||^). For instance, compiling:

   [match(v){ (4,5) -> 1; _ -> 2 }]

   gives:

   [let k_not4_5() = { kreturn(2) }]\\
   [match(#0(v)){]\\
   [  4 -> match(#1(v)){]\\
   \quad[    5 -> kreturn(1)]\\
   \quad[    _ -> k_not4_5()]\\
   [  }]\\
   [  _ -> k_not4_5()]\\
   [}]

   Matching against [(4,5)] can fail at two different steps, and the
   action to perform in these two cases are the same, so they should
   be factorized using the same continuation.

   The L compiler does not yet allow it, but "or-patterns" (i.e. in
   [match(l){ Cons(Nil|Cons(_,Nil)) -> 0 _ -> 1 }]) also need join
   points. Finally, there is also a joint point (in
   [expr_env.context]) to which the value of the bodies in each rule
   is returned.
   \end{itemize}

   All the functions that involve building CPS code are themselves in
   CPS style; see the [Cps_transform_expression] module for an
   explanation.

   \paragraph{A complete example}

   Here is a (contrived) exemple of a complete pattern matching:

   \includegraphics{cpstransform_rules_example.png}

   This pattern is compiled as follows. We begin by creating a join
   continuation, which is where the result of the match is returned.
   This allows to factorize the following computation (the addition to
   17 in our case).

   [let kfinal(x) = { let x17 = x + 17 in halt(x17) }]

   Then, the condition of the match [e] is evaluated, and its result
   stored in a temporary value.

   [let v = ... eval e ...]

   Then, analysis of the patterns show that [v] contains a tuple.

   [let v.0 = #0(v)]\\
   [let v.1 = #1(v)]

   Analysis of the patterns also show that [v.0] contain a tuple.
   [v.1] is a variant type, so its elements cannot be retrieved yet.

   [let v.0.0 = #0(v.0)]\\
   [let v.0.1 = #1(v.0)]

   We begin by analysis the whole pattern (i.e. column $c_0$). All the
   rules are refutable, except the last one, so we split them into two
   contiguous blocks $b_i$ and $b_{ii}$; $b_{ii}$ is executed if
   matching against all the rules in $b_{i}$ fail.

   [decl kb_ii]

   All the rules in $b_i$ are tuples, so we inspect them from left to
   right (i.e. we begin by column $c_1$, then proceed with $c_4$).
   Analysis of column $c_1$ yields three contiguous blocks: the
   patterns in column $c_1$ are all irrefutable for block $b_{i.a}$,
   refutable for block $b_{i.b}$, and irrefutable again for block
   $b_{i.c}$.

   [decl kb_i.b, kb_i.c]

   As the patterns of column $c_{1}$ in rules in $b_{i.a}$ are all
   irrefutable, we just have to associate the variable [x] to [v.0.0]
   and [a] to [v.0.1] for the translation of the body of the rules.
   ([x] and [a] are unused in the rules of the example).

   We can then proceed with the analysis of column $c_2$ (still in
   block $b_{i.a}$). It is a variant, so we can regroup the rules
   according to the constructor, and perform a simple case analysis.

   [decl kcons]\\
   [match(v.1){]\\
   [  Nil -> { kfinal(2) }]\\
   [  Cons(x) -> { kcons(x) } ]\\
   [}]

   For the [Nil] constructor, we are already done. For [Cons], we have
   to discriminate against the patterns inside the [Cons]. But first,
   we analyze these patterns to retrieve all the elements that are
   needed:

   [let kcons(x) = {]\\
   [ let x.0 = #0(x)]\\
   [ let x.1 = #1(x)]\\
   [ let x.0.0 = #0(x.0)]\\
   [ let x.0.1 = #1(x.0)]

   There are two contiguous blocks: one with rule 1 and 3 (since rule
   2 has been regrouped with the [Nil]), and one with rule 4. We begin
   with the 1-3 block:

   [  decl knext]\\
   [  match(x.0.0){]\\
   \quad[ 1 -> { kfinal(1)}]\\
   \quad[ 3 -> { kfinal(3)}]\\
   \quad[ _ -> { knext()}]\\
   [  }]

   If matching against the 1-3 block fails, we match against rule 4.
   If this fails, then matching against all the rules in $b_{i.a}$
   failed, and we try to match against the rules in $b_{i.b}$.

   [  let knext() = {]\\
   \quad[  match(x.0.1){]\\
   \qquad[ 4 -> { kfinal(4)}]\\
   \qquad[ _ -> { kb_i.b()}]\\
   \quad[  }]\\
   [  }]\\
   [}]

   The rest of the matching is very similar. In $b_{i.b.1}$, the
   matching against rules 5 and 7 is factorized, because there is a
   common constructor. (Note that there is no factorization on [Cons]
   between rules 4 and 5, because they are in different blocks). Then
   blocks $b_{i.b.2}$, $b_{i.b.3}$, $b_{i.c}$ are tried successively.
   In $b_{i.c}$, the test of [Cons] is factorized, but not the test
   for [Nil], because testing [Nil] is done after testing [10].

   Finally, the pattern matching always succeeds since rule 12 is
   irrefutable, so there is no need to introduce code that perform
   [match_failure] in case nothing succeeds in matching.

   Note that the presentation would have been clearer if the patterns
   had been regrouped differently; in particular, grouping rules who
   share a constructor matched as the same time (e.g. exchanging rules
   1 and 2, and rule 5 and 6) would improve the presentation.
*)

module CaseMap = Cpsbase.CaseMap;;
module Build = Cpsbase.Build;;
module Ast = Astdef;;

(* This module is mutually recursive with the module performing
   transformation into CPS of expressions, from which we require
   the [transform] function. *)
open Cps_transform_common;;

(****************************************************************)
(*s Identifying the structure of patterns; retrieving needed
  components. *)

(* The [Structure] module allows to analyze a list of patterns, and
   build the code to retrieve all the components needed by at least
   one pattern. This ensures that (whenever possible) the components
   are retrieved only once, which makes the code more compact.

   This code also checks that the components are compatible (e.g. that
   the matched tuples have the same arity), in case that type checking
   failed to do it. *)
module Structure : sig

  (* The aim of this module is to produce a value of type [t], which
     is a CPS variable together with the CPS variables for each of its
     subcomponents. Note that structures do not go into variants,
     because we cannot: the contents in variants can be obtained only
     once they are matched. *)
  type 'a structure = 'a * 'a structure_
  and 'a structure_ =
  | Any
  | Tuple of int * 'a structure list
  | Fold of Ast.Type_name.t * 'a structure
  type t = Cpsbase.Var.Occur.maker structure

  (* [build v patts k] analyze [patts] to create a [structure]
     containing all the CPS variables needed for compiling the
     rules, and "returns" (in CPS style) by calling [k] with that
     structure. [v] is the CPS variable against which the [patts]
     are matched. *)
  val build:
    Cpsbase.Var.Occur.maker ->
    Ast.Pattern.t list ->
    (t -> Build.fresh) ->
    Build.fresh
end =
struct

  type 'a structure = 'a * 'a structure_
  and 'a structure_ =
  | Any
  | Tuple of int * 'a structure list
  | Fold of Ast.Type_name.t * 'a structure
  type t = Cpsbase.Var.Occur.maker structure

  (* The implementation is done in two steps:
     \begin{itemize}
     \item First we identify the structure (in normal style, by
     unification of the patterns);
     \item Then we build the code to retrieve the CPS variables (in
     CPS style).
     \end{itemize} *)

  let incompatible_patt patt str =
    Log.Pattern_matching.raise_compiler_error
      "Incompatible pattern %a, expected a %s"
      Astprint.pattern patt str
  ;;

  (* Identification works by first-order unification of all the
     patterns. The [unit structure] type is a special representation
     of a pattern, containing the results of the previous
     unifications. We do not enter variant types, because we will not
     retrieve their components until they are matched (and we redo an
     identification when this happens). *)
  let rec unify_patt (structure:unit structure) patt = match patt with
    | Ast.Pattern.Tuple l ->
      let n,structures = match snd structure with
        | Any -> List.length l, List.map (fun _ -> (), Any) l
        | Fold _ -> incompatible_patt patt "expected a tuple"
        | Tuple(n, l') ->
          if (List.length l != n)
          then incompatible_patt patt ("tuple of size " ^ (string_of_int n))
          else n, l' in
      (), Tuple (n, List.map2 unify_patt structures l)

    | Ast.Pattern.Fold(tn,patt') ->
      let subs = (match snd structure with
        | Any -> (),Any
        | Tuple _ -> incompatible_patt patt "fold"
        | Fold(tn', _) when tn' != tn ->
          incompatible_patt patt "fold with a different typename"
        | Fold(_,s) -> s) in
      (), Fold(tn, unify_patt subs patt')

    (* No new information about the structure, and we do not enter
       in variants. *)
    | Ast.Pattern.Wildcard | Ast.Pattern.Variable _
    | Ast.Pattern.Constant(Constant.Integer _ | Constant.Bool _)
    | Ast.Pattern.Injection _ -> structure
    | _ -> Log.Pattern_matching.raise_compiler_error "Pattern %a not handled"
      Astprint.pattern patt
  ;;

  let unify_patts patts =
    List.fold_left unify_patt ((),Any) patts

  (* Second step: build all the necessary CPS vars, in CPS style.
     Takes a CPS variable and its identified structure, and "return"
     (in CPS style, using the [k] argument) the structure completed
     with the CPS variable of its components. *)
  let rec build_structure v ((),s) k = match s with
    | Any -> k (v,Any)
    | Fold(tn, subs) ->
      build_structure v subs (fun s -> k (v, Fold(tn, s)))
    | Tuple(n,l) ->
      let array = Array.of_list l in
      let rec loop i accu =
        if i < n
        then Build.let_proj i v (fun v_i ->
          build_structure v_i array.(i) (fun subs ->
            loop (i+1) (subs::accu)))
        else k (v, Tuple( n, List.rev accu)) in
      loop 0 []

  (* The main algorithm: first identify the structure, and then fill
     it with the CPS variables. *)
  let build v patts k =
    let unit_s = unify_patts patts in
    build_structure v unit_s k
end;;

(****************************************************************)

module Make(Expression:EXPRESSION) = struct

  (* The current status of pattern matching compilation is represented
     by a list of [rule]s and one [pattern_env]. [rule] contains
     information per rule being matched, while [pattern_env] contains
     information common to all the rules. *)

  type rule = {
    (* Maps AST variables in the patterns compiled so far, to the CPS
       variables containing their value. *)
    scope_addition: Cpsbase.Var.occur_maker Ast.Variable.Map.t;

    (* The list of patterns that still have to be matched. Always
    have the same length than [env.remaining_vs]. *)
    remaining_patts: Ast.Pattern.t list;

    (* If all the elements in [env.remaining_vs] match
       [remaining_patts], then this [body] is executed. *)
    body: Ast.Expression.t;
  };;

  type pattern_env = {

    (* The continuation to call if nothing matches: this continuation
       can raise a [match_failure] exception, or match another list of
       rules. *)
    defaultk: Cpsbase.Cont_var.occur_maker;

    (* The value agains which the rules are matched (with their
       structures). *)
    remaining_vs: Structure.t list;

    (* We are compiling a set of patterns in an expression: this is
       the environment corresponding to that expression. [expr_env] is
       used only when compiling the [body] of a [rule]. *)
    expr_env: expr_env;

  };;

  (****************************************************************)
  (*s Helper functions for transform_rules: define refutability,
    irrefutability, rules splitting by contiguous blocks. *)

  (* A pattern is \emph{refutable} if contains a specific
     \emph{variant}. A \emph{variant} is currently an integer, a
     boolean, or an injection (i.e. user-defined variant).

     A pattern is \emph{irrefutable} if it never refers to a specific
     variant. *)
  let rec is_refutable = function
    | Ast.Pattern.Wildcard | Ast.Pattern.Variable _ -> false
    | Ast.Pattern.Fold(_,patt) -> is_refutable patt
    | Ast.Pattern.Tuple(patts) -> List.exists is_refutable patts
    | Ast.Pattern.Injection _ | Ast.Pattern.Constant _ -> true

  let is_irrefutable patt = not (is_refutable patt);;

  (* [split_firsts pred rules] returns two lists of rules,
     [(selected,other)], with [selected] the first elements of [rules]
     that satisfy the predicate [pred], and [other] the remaining
     elements (i.e. [other] is [[]] or so that [pred (List.hd other) =
     false]). *)
  let rec split_firsts pred = function
    | [] -> [],[]
    | a::b when pred a -> let (selected, other) = split_firsts pred b in
                          a::selected, other
    | l -> [], l
  ;;

  (* [split_on_first_pattern pred l] returs two lists of rules, split
     according to whether their first pattern is satisfied by
     [pred].*)
  let split_on_first_pattern pred rules =
    split_firsts (fun rule -> pred (List.hd rule.remaining_patts)) rules
  ;;

  (****************************************************************)
  (*s Entry point to the pattern compilation loop.  *)

  (* Requires that [env.remaining_vs] and all [rule.remaining_patts]
     have the same length; and that the set of rules is not empty. *)
  let rec transform_rules rules env =
    assert (rules != []);
    assert( let n = List.length env.remaining_vs in
            List.for_all (fun rule ->
              List.length (rule.remaining_patts) == n) rules);

    let first = List.hd rules in
    match first.remaining_patts with

    (* No remaining pattern: we managed to match everything. Just
       execute the body of [first], and discard the other rules. *)
    | [] ->
      assert (env.remaining_vs == []);
      if (List.tl rules != [])
      then Log.Pattern_matching.warning "There are unreachable rules";

      let new_scope =
        Ast.Variable.Map.add_map first.scope_addition env.expr_env.scope in
      Expression.transform first.body new_scope env.expr_env.context

    (* At least one remaining pattern. If the pattern is refutable,
       select the rules beginning by a refutable pattern, else select
       the rules beginning by an irrefutable pattern. Try to match
       against these rules; if that fails, execute the other rules. *)
    | patt::other_patts ->

      let split_rules pred f =
	let selected, other = split_on_first_pattern pred rules in
	(* If there are [other] rules, and matching against [selected]
	   fails, then matching continues against the rules in
	   [other]: so we create a new continuation to be used as
	   [defaultk], that will call the previous [defaultk] if they
	   fail to match too.

	   If there are no [other] rule, execution jumps to the
	   existing [defaultk] if the matching fails. *)
	let change_defaultk_if_other f =
	  if other == []
	  then f env.defaultk
	  else Build.let_cont
	    (fun _ -> transform_rules other env)
	    (fun k -> f k) in
	change_defaultk_if_other (fun defaultk ->
	  let v = List.hd env.remaining_vs in
	  let new_env =
	    { env with remaining_vs = List.tl env.remaining_vs; defaultk } in
	  f v selected other new_env) in

      if is_irrefutable patt
      then split_rules is_irrefutable (fun v selected other env ->
        (* The irrefutable patterns complete the matches, so [other]
           rules are useless. *)
        if other_patts == [] && other != []
        then Log.Pattern_matching.warning "There are unreachable rules";
        transform_rules_irrefutable v selected env)

      else split_rules is_refutable (fun v selected _ env ->
        match patt with

          (* Patterns refering to a specific variant, compiled into a case. *)
          | Ast.Pattern.Injection(_,n,_) ->
            transform_rules_injection n v selected env
          | Ast.Pattern.Constant(Constant.Integer _) ->
            transform_rules_integer v selected env
          | Ast.Pattern.Constant(Constant.Bool _) ->
            transform_rules_boolean v selected env
          | Ast.Pattern.Constant(_) -> Log.Pattern_matching.raise_compiler_error
            "arbitrary constants impossible"

          (* Structural patterns that increase the [remaining_patts]. *)
          | Ast.Pattern.Fold(tn,_) ->
            transform_rules_fold tn v selected env
          | Ast.Pattern.Tuple l ->
            transform_rules_tuple (List.length l) v selected env

          | _ -> Log.Pattern_matching.raise_compiler_error
            "refutable pattern expected" )

  (****************************************************************)
  (*s Irrefutable patterns. *)

  (* As all values of the components of the tuple have already been
     retrieved (and are in [structv]), the only responsibility of this
     function is to update the [scope_addition] of the [rules]. *)
  and transform_rules_irrefutable structv rules env =

    (* Jointly traverse a [pattern] (containing the AST variable
       names) and a [structure] (containing the CPS variables) to
       improve the [scope_addition] (containing a map from AST
       variables to CPS variables). *)
    let rec loop scope pattern (v,structure) = match pattern with
      (*i Note: We could remove Wildcard, and replace it by an unused
	variable.  The Cps variable is created anyway. This could
	simplify typing too. But what about unused results? i.e. let
	_ = ...? i*)
      | Ast.Pattern.Wildcard -> scope
      | Ast.Pattern.Variable var -> Ast.Variable.Map.add var v scope
      | Ast.Pattern.Tuple l ->
        let Structure.Tuple(_,structs) = structure in
        List.fold_left2 loop scope l structs
      | Ast.Pattern.Fold(_,patt) ->
        let Structure.Fold(_,structure) = structure in
        loop scope patt structure
      | _ -> assert false (* Not an irrefutable pattern. *) in

    let rules = List.map (fun rule ->
      let patt = List.hd rule.remaining_patts in
      { rule with scope_addition = loop rule.scope_addition patt structv;
	remaining_patts = List.tl rule.remaining_patts })
      rules in

    transform_rules rules env

  (****************************************************************)
  (*s Structural refutable patterns. *)

  (* Replace the head of [remaining_patts] and [remaining_vs], which
     is a Tuple, by the contents of this tuple. *)
  and transform_rules_tuple n v rules env =
    let rules = List.map (fun rule ->
      match rule.remaining_patts with
	| (Ast.Pattern.Tuple l)::matches ->
          assert (List.length l == n);
          { rule with remaining_patts = l @ matches }
	| _ -> assert false (* Already checked by Structure and type checking. *)
    ) rules in

    let (_,Structure.Tuple(nt,vs)) = v in
    assert (nt == n);
    transform_rules rules { env with remaining_vs = vs @ env.remaining_vs }


  (* Replace the head of [remaining_patts] and [remaining_vs], which
     is a Fold, by the contents of the fold. *)
  and transform_rules_fold tn v rules env =
    let rules = List.map (fun rule ->
      match rule.remaining_patts with
	| Ast.Pattern.Fold(t,patt)::matches ->
	  assert (tn == t);
          { rule with remaining_patts = patt::matches }
	| _ -> assert false (* Already checked by Structure and type checking. *)
    ) rules in

    let (_,Structure.Fold(tnf,v)) = v in
    assert (tnf == tn);
    transform_rules rules { env with remaining_vs = v::env.remaining_vs }

  (****************************************************************)
  (*s Specific variants, that are compiled into a case. *)

  (* [map_into_casemap] is similar to [List.map], except that rather
     than returning a linear list, it splits the list into a casemap
     according to the key returned by the [f] function. *)
  and map_into_casemap rules f =
    let casemap = List.fold_left
      (fun accu rule ->
        let (i,res) = f rule in
        let previous = try (CaseMap.find i accu) with Not_found -> [] in
        CaseMap.add i (res::previous) accu)  CaseMap.empty rules
    in CaseMap.map List.rev casemap


  (* [build_rules_casemap] compiles the code that matches [v] against
     the rules in [rules_casemap]. To each key in the casemap
     corresponds a list of rules, which is compiled into a
     continuation using [transform_rules]; when all continuations are
     obtained, the actual [case] instruction can be built.

     The [g] functional parameter allows to build code and change
     [env] before the list of rules are compiled. *)
  and build_rules_casemap (v,_) rules_casemap env g =
    (* Compiles each list of rules in the casemap into a continuation,
       anc accumulate them into [map]. *)
    let f map i rules_for_i nextf =
      Build.let_cont
        (fun x ->
	  g x rules_for_i env (fun env ->
	    transform_rules rules_for_i env))
        (fun k -> nextf (CaseMap.add i k map)) in
    (* Compiles the case instruction *)
    let finalf map = Build.case v ~default:env.defaultk map in
    CaseMap.foldk f CaseMap.empty finalf rules_casemap


  (* Compilation of integers and boolean is straightforward: we just
     have to extract the integer number corresponding to the case. *)
  and transform_rules_integer v rules env =
    let rules_casemap = map_into_casemap rules (fun rule ->
      match rule.remaining_patts with
	| Ast.Pattern.Constant(Constant.Integer i)::patts ->
          (i, { rule with remaining_patts = patts })
	| _ -> assert false) in
    build_rules_casemap v rules_casemap env (fun _ _ env k -> k env)

  and transform_rules_boolean v rules env =
    let rules_casemap = map_into_casemap rules (fun rule ->
      match rule.remaining_patts with
	| Ast.Pattern.Constant(Constant.Bool b)::patts ->
          let i = match b with false -> 0 | true -> 1 in
          (i, { rule with remaining_patts = patts })
	| _ -> assert false) in
    build_rules_casemap v rules_casemap env (fun _ _ env k -> k env)


  (* In addition to extracting the number for the case, injection also
     add the pattern they contain to the head of [remaining_patts]. At
     the beginning of the continuation [k] corresponding to a set of
     rules that match a variant, the structure needed to these rules
     are identified and obtained against the CPS var argument of [k],
     and added at the head of [remaining_vs]. *)
  and transform_rules_injection n v rules env =
    let (rules_casemap) = map_into_casemap rules (fun rule ->
      match rule.remaining_patts with
	| Ast.Pattern.Injection(i,j,patt)::patts ->
          (i, { rule with remaining_patts = patt::patts })
	| _ -> assert false) in

    build_rules_casemap v rules_casemap env (fun x rules env k ->
      let patts = List.map (fun { remaining_patts = patt::_ } -> patt ) rules in
      Structure.build x patts (fun v ->
        let env = { env with remaining_vs = v::env.remaining_vs } in
        k env))
  ;;

  (****************************************************************)

  (* Entry point of the module. *)
  let transform l v expr_env =

    (* Note: we could directly call [match_failure]. But should we
       allow this degenerate case? For now we fail if this happens. *)
    if l == []
    then failwith "Pattern matching with no rules is not implemented";

    (* If there is only one rule (e.g. compiling [let]), keep the
       context. Else, create a "join" continuation, to avoid code
       duplication. *)
    let maybe_change_context f =
      if List.tl l == [] then f expr_env
      else Build.let_cont
        (fun x -> expr_env.context x)
        (fun kjoin -> let context x = Build.apply_cont kjoin x in
                      f { expr_env with context }) in
    maybe_change_context (fun expr_env ->

      let patts = List.map fst l in

      (* If there is an irrefutable rule, then [defaultk] is never
         used. We avoid creating one by using [Obj.magic 0] to
         represent an undefined value. *)
      let irrefutable = List.exists is_irrefutable patts in
      let maybe_create_match_failure f =
        if irrefutable then f (Obj.magic 0) else Build.let_match_failure f in
      maybe_create_match_failure (fun defaultk ->

        (* Initialize pattern compilation.  *)
        let rules = List.map (fun (patt,body) ->
          { scope_addition = Ast.Variable.Map.empty;
            remaining_patts = [patt];
            body = body }) l in
        Structure.build v patts (fun structure ->
          let env = { remaining_vs = [structure]; defaultk; expr_env } in
          transform_rules rules env )))

end

(*i TODO: Improvements for pattern matching.

  - "Or" patterns: e.g. match(x){ 0|1 -> 1 2 -> 3 }
  - Possibly, range patterns for integers
  - "And" patterns:

  e.g. match(x){
  Nil -> 2
  Cons(a,b) and match(M.get(a)){ 0 -> 0; 1 -> 1}
  Cons(a,Nil) -> 3
  _ -> 4
  }

  The fallback if the second match fails, is to continue testing the
  remaining matches. It is a generalization of pattern guards, such as
  the [when] OCaml construct; that would also allow arbitrary
  computations inside patterns.

  - When compiling pattern matching, improve detection of missing and
  redundant rules. I think there exists a lattice of patterns, and
  performing the union of all patterns should allow to test whether
  some patterns are missing (and by taking the complement, to know
  which ones). To know if a rule is redundant is to test inclusion of
  a pattern by the union of the previous patterns. This lattice could
  maybe be used as the basis for building the "Structure".

  i*)
