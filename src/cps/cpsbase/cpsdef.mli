(* Copyright 2012 Matthieu Lemerre *)

(* This module provides low-level, read-write access to the abstract
   syntax tree of the CPS intermediate representation. *)

(* Abstract syntax for CPS terms. *)
module CaseMap:Extensions.Map.S with type key = int

include Cpsast.S with type 'a case_map = 'a CaseMap.t;;

(*s An [Empty] expression is an [expression] without a downlink to an
  [expression_]. *)
module Empty: sig

  (* Most operations on expressions do not work on empty expressions; for that
     reason we have created a separate type [Empty.t]. However, there
     can be expressions whose type is not [Empty.t] which are empty. For
     that reason the presence of empty expressions should always be
     temporary. *)
  type t

  (* If no care is taken, regular expressions can also be empty. *)
  val is_empty: expression -> bool

  (* Remove the downlink of an expression; the expression must be
     previously non-empty. *)
  val empty: expression -> t

  (* Set the downlink of an expression [t] (which was empty) to some
     [expression_], and return [t] (which is no longer empty) *)
  val fill: t -> expression_ -> expression
end;;

(*s Main functions to access (read and write) an expression. Note that
  the [set] and [set_enclosing] functions are dangerous, as they may
  break invariants on Cps expressions; for that reason, one should
  rather use the [Cpschange] module, that performs modifications while
  maintaining those invariants. *)
module Expression: sig

  (* Access to the [expression_] in an expression. *)
  val get: expression -> expression_
  val set: expression -> expression_ -> unit

  (* Access to the [enclosing] of an expression. *)
  val enclosing: expression -> enclosing
  val set_enclosing: expression -> enclosing -> unit

  (* If a [?reconnect] argument is given, [make] fills [reconnect]
     with the [expression_] argument, and return it. Else, it returns
     a new fresh expression, with downlink to a [expression_]. Thus
     depending on [reconnect] the argument, make will return a
     non-fresh or a fresh expression. *)
  val make: ?reconnect:Empty.t -> expression_ -> expression

  (* To use if an expression should not be used anymore. This helps to
     catch errors if accessing the discarded expression. *)
  val discard: expression -> unit
end;;


(*s A fresh expression is a [expression] without an uplink to an
  enclosing expression or definition. *)
module Fresh: sig
  (* Fresh expressions are very similar to expressions; only the "enclosing"
     operation is forbidden. For that reason, the type [t] is not made
     abstract, as it is too inconvenient to split the functions that
     apply on both. But ideally, [Fresh.t] would have been a supertype
     of type [expression]. *)
  (*i We could maybe use the private type abbreviation mechanism of Ocaml. i*)
  type t = expression

  (* Create a fresh expression containing a [expression_]. *)
  val make: expression_ -> t

  (* Get that [expression_]. *)
  val get: t -> expression_

  (* Checks if an expression is fresh. *)
  val is_fresh: expression -> bool

  (* [set_enclosing fresh encllosing] sets [enclosing] as the
     enclosing term for [fresh]. [fresh] must be fresh. *)
  val set_enclosing: t -> enclosing -> unit
end;;

(* Module type for variables and continuation variables. This is an
   expanded version of [Cps.Base.VAR], which includes the write
   operations. *)
type occur_type = Cpsvar.occur_type = Recursive | Non_recursive
module type VAR_RW = sig

  (* Note: the reason for these types is that they can easily changed
     using "[with]" declarations, contrary to the types inside the
     submodules [Var] and [Occur]. So they can be used as "gateways" to
     specify the types in these submodules. *)
  type var
  type occur_maker
  type occur

  module Var: sig

    (* As variables have uplinks to their enclosing sites, the CPS
       structure is cyclic. To break this recursivity, we build CPS
       structures in two steps: [make] creates an empty variable, to
       be used in a CPS expression; when this expression is made,
       [init] sets the uplink to it. This is all handled by
       [Cpsbuild]. *)
    val make : unit -> var
    val init: var -> enclosing -> unit

    type number_of_occurrences =
    | No_occurrence
    | One_occurrence of occur
    | Several_occurrences
    val number_of_occurrences: var -> number_of_occurrences
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a

    (* [replace_with v1 v2] makes all occurrences of [v1] become
       occurrences of [v2]. *)
    val replace_with: var -> var -> unit
    val replace_all_non_recursive_occurrences_of_with: var -> var -> unit

    val to_string : var -> string

    (* Access or change the binding site. For both functions,
       initialization must have been previously done.  *)
    val binding_site: var -> enclosing
    val set_binding_site: var -> enclosing -> unit

    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur: sig

    (* [make (var,occur_type)] creates a [Recursive] or
       [Non_recursive] occurrence of [var]. The pair (var,occur_type)
       thus allows to create a new occurrence, and is called an
       [maker]. [maker] and [rec_maker] are helper functions that
       return [maker]s. *)
    type maker = var * occur_type
    val maker : var -> maker
    val rec_maker: var -> maker
    val make : maker -> occur

    (* Delete the occurrence of a variable. Must be called when
       deleting some parts of a CPS tree (this is handled by
       [Cpschange]). *)
    val delete: occur -> unit

    val binding_variable : occur -> var
    val to_string : occur -> string
    module Map : Map.S with type key = occur
    module Set : Set.S with type elt = occur
  end
end


module Var:VAR_RW with type var = var
                  and type occur_maker = var * occur_type
                  and type occur = occur;;
module Cont_var:VAR_RW with type var = cont_var
                       and type occur_maker = cont_var * occur_type
                       and type occur = cont_occur;;

type occur_maker = Var.Occur.maker
type cont_occur_maker = Cont_var.Occur.maker
