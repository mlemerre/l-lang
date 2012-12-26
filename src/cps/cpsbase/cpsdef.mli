(* Copyright 2012 Matthieu Lemerre *)

(* This module provides low-level, read-write access to the abstract
   syntax tree of the CPS intermediate representation. *)

(* Abstract syntax for CPS terms; one should start here. *)
include Cpsast.S;;

(*s An empty term is a [term] without a downlink to a [term_].  *)
module Empty: sig

  (* Most operations on terms do not work on empty terms; for that
     reason we have created a separate type [Empty.t]. However, there
     can be terms whose type is not [Empty.t] which are empty. For
     that reason the presence of empty terms should always be
     temporary. *)
  type t

  (* If no care is taken, regular terms can also be empty. *)
  val is_empty: term -> bool

  (* Remove the downlink of a term; the term must be previously
     non-empty. *)
  val empty: term -> t

  (* Set the downlink of a term [t] (which was empty) to some [term_],
     and return [t] (which is no longer empty) *)
  val fill: t -> term_ -> term
end;;

(*s Main functions to access (read and write) a term. Note that the
  [set] and [set_enclosing] functions are dangerous, as they may break
  invariants on Cps terms; for that reason, one should rather use the
  [Cpschange] module, that performs modifications while maintaining
  those invariants. *)
module Term: sig

  (* Access to the [term_] in a term. *)
  val get: term -> term_
  val set: term -> term_ -> unit

  (* Access to the [enclosing] of a term. *)
  val enclosing: term -> enclosing
  val set_enclosing: term -> enclosing -> unit

  (* If a [?reconnect] argument is given, [make] fills [reconnect] with
     the [term_] argument, and return it. Else, it returns a new fresh
     term, with downlink to a [term_]. Thus depending on [reconnect] the
     argument, make will return a non-fresh or a fresh term. *)
  val make: ?reconnect:Empty.t -> term_ -> term

  (* To use if a term should not be used anymore. This helps to catch
     errors if accessing the discarded term. *)
  val discard: term -> unit
end;;


(*s A fresh term is a [term] without an uplink to an enclosing term or
  definition. *)
module Fresh: sig
  (* Fresh terms are very similar to terms; only the "enclosing"
     operation is forbidden. For that reason, the type [t] is not made
     abstract, as it is too inconvenient to split the functions that
     apply on both. But ideally, [Fresh.t] would have been a supertype
     of type [term]. *)
  (*i We could maybe use the private type abbreviation mechanism of Ocaml. i*)
  type t = term

  (* Create a fresh term containing a [term_]. *)
  val make: term_ -> t

  (* Get that [term_]. *)
  val get: t -> term_

  (* Checks if a term is fresh. *)
  val is_fresh: term -> bool

  (* [set_enclosing fresh term] sets [term] as the enclosing term for
     [fresh]. [fresh] must be fresh. *)
  val set_enclosing: t -> enclosing -> unit
end;;

(* Module type for variables and continuation variables. This is an
   expanded version of [Cps.Base.VAR], which includes the write
   operations. *)
module type VAR_RW = sig
  type var
  type occur
  module Var: sig

    (* As variables have uplinks to their enclosing sites, the CPS
       structure is cyclic. To break this recursivity, we build CPS
       structures in two steps: [make] creates an empty variable, to
       be used in a CPS term; when this term is made, [init] sets the
       uplink to it. This is all handled by [Cpsbuild]. *)
    val make : unit -> var
    val init: var -> enclosing -> unit

    type occurrence_number =
    | No_occurrence
    | One_occurrence of occur
    | Several_occurrences
    val occurrence_number: var -> occurrence_number
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a

    (* [replace_with v1 v2] makes all occurrences of [v1] become
       occurrences of [v2]. *)
    val replace_with: var -> var -> unit

    val to_string : var -> string

    (* Access or change the binding site. For both functions,
       initialization must have been previously done.  *)
    val binding_site: var -> enclosing
    val set_binding_site: var -> enclosing -> unit

    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur: sig

    (* Create a new occurrence for a variable.  *)
    val make : var -> occur

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

module Var:VAR_RW with type var = var and type occur = occur;;
module Cont_var:VAR_RW with type var = cont_var and type occur = cont_occur;;
