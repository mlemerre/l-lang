(* Copyright 2012 Matthieu Lemerre *)

(*s The [CPS.Base] module is the entry point for modules that access
  or change terms in the CPS intermediate representation.

  It provides access [CpsAST], the abstract syntax of CPS terms; to
  the [Term] and [{Cont_}Var] modules, providing access to the
  various links between elements of the ast; and to modules that
  respectively allow printing, modifying, creating new, traversing,
  and checking invariants of, CPS terms. *)

(*s [CpsAST] provides the constructors for pattern matching of CPS
  terms. The [CpsAST] module is shared with [Cpsdef] (which provides
  low-level access to CPS terms for the code implementing [Cps.Base]);
  the [with] constraints allow the types of [Cps.Base] and [Cpsdef] to
  be compatible. Still, [Cpsdef] should not be used directly outide of
  the implementation of [Cps.Base]. *)
open Basepack

include Cpsast.S with type term = Cpsdef.term
                 and type term_ = Cpsdef.term_
                 and type primitive = Cpsdef.primitive
                 and type value = Cpsdef.value
                 and type toplevel = Cpsdef.toplevel
                 and type modul = Cpsdef.modul
                 and type visibility = Cpsdef.visibility
                 and type definition_type = Cpsdef.definition_type
                 and type definition = Cpsdef.definition
                 and type enclosing = Cpsdef.enclosing
                 and type var = Cpsdef.var
                 and type occur = Cpsdef.occur
                 and type cont_var = Cpsdef.cont_var
                 and type cont_occur = Cpsdef.cont_occur
;;

(*s In addition to the base abstract syntax tree, the representation
  of CPS terms provide additional "links" between entities: it is
  possible to know the term in which an occurrence appears, where a
  variable is bound, which term encloses another term... The [Term],
  [Var] and [Conv_var] modules provide helper functions to access
  these informations (in O(1) time). *)

module Term: sig
  (* Get the [term_] (i.e. actual tree element) inside a [term]
     structure. *)
  val get: term -> term_

  (* Get what is enclosing the term (it can be a term or
     definition). *)
  val enclosing: term -> enclosing
end;;

module type VAR = sig

  (* Types of variables and occurrences (different for standard
     variables/occurrences and for continuations
     variables/occurrences). *)
  type var
  type occur

  module Var: sig
    (* The following function differentiates, in O(1) time, between
       the case where a variable has 0 occurrence (the variable is not
       used), 1 occurrence case (a good candidate for inlining), and
       the case where it has more than one occurrence. *)
    type occurrence_number =
    | No_occurrence
    | One_occurrence of occur
    | Several_occurrences
    val occurrence_number: var -> occurrence_number

    (* This function allows iteration on occurrences. The order is
       arbitrary. *)
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a

    (* Get the term or definition that binds the variable. *)
    val binding_site: var -> enclosing

    (* Display a standard printed representation of variables. *)
    val to_string : var -> string

    (* Frequently-used helper modules of Maps and Sets of
       variables. *)
    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur: sig
    (* [binding_variable occur] returns the binding variable of
       [occur], i.e. the variable [var] such that [occur] is an
       occurrence of [var]. *)
    val binding_variable : occur -> var

    (* Display a standard printed representation of occurrences. *)
    val to_string : occur -> string

    (* Frequently-used helper modules of Maps and Sets of
       occurrences. *)
    module Map : Map.S with type key = occur
    module Set : Set.S with type elt = occur
  end
end

module Var:VAR with type var = var and type occur = occur;;
module Cont_var:VAR with type var = cont_var and type occur = cont_occur;;

(* These modules keep the same interface; see their respective
   interface files for their documentation. *)
module Print:module type of Cpsprint;;
module Change:module type of Cpschange with type fresh = term;;
module Build:module type of Cpsbuild with type fresh = term;;
module Traverse:module type of Cpstraverse;;
module Check:module type of Cpscheck;;
