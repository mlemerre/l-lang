(* Copyright 2012 Matthieu Lemerre *)

(* There are two structures where information about variables is
   attached. [occurrence] stores data about the use of a variable
   (like the location of the use). [variable] stores data common to
   all the uses of the variable, like its type, or the location where
   the variable is bound. Each occurrence is linked to its binding
   variable, while a variable is linked to the set of its occurrences.
   There are actually two sets of occurrences for each variable: the
   recursive, and non-recursive occurrences. The distinction allows to
   detect, for instance, when there are no occurrences of a recursive
   function, which can then be eliminated. *)
type ('a, 'b) variable
type ('a, 'b) occurrence
type occur_type = Recursive | Non_recursive

(* The user of the module must configure the information it wants to
   attach to a [variable] or [occurrence] (using the DESC structure).
   This information is called a [description].

   The [make] functions, that create new variables and occurences, do
   not take an initial [description] argument. This allows to break
   circular dependencies, e.g. where a term points to a variable or
   occurrence, and a variable points to the term that bind or use it.

   Instead, for both the variable and its occurrences, the description
   is set with the [set_description] function, and obtained with the
   [description] function. It is illegal (and checked by assertions)
   to set a description more than once, or to call [description] on a
   variable when the description has not been set. 

   The [var_prefix] parameter is used as a prefix to the [to_string]
   function. For exemple if prefix is "x", [to_string] may return
   "x10" for a variable and "x10_2" for one of its occurrence. *)
module type DESCRIPTION = sig 
  type variable_description
  type occurrence_description
  val var_prefix:string
end

module type S = sig
  type var_desc
  type occur_desc
  type var = (var_desc, occur_desc) variable
  type occur = (var_desc, occur_desc) occurrence

  module Var :
  sig

    (* [make ()] creates a new variable. *)
    val make : unit -> var

    (* The following function differentiates, in O(1) time, between
       the 0-occurrence case (the variable is useless), the
       1-occurrence case (a good candidate for inlining), and the case
       with more than one occurrence. The count does not include
       recursive occurrences. *)
    type number_of_occurrences =
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences
    val number_of_occurrences: var -> number_of_occurrences

    (* These functions allows iteration on normal or recursive
       occurrences of a variable. The order is arbitrary. *)
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a
    val fold_on_recursive_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a
    (*i TODO: fold_on_all_occurrences? i*)

    (* This function "merges" variables; more specifically it makes
       all (recursive or non-recursive) occurrences of [old] become
       occurrence of [new]. After the call [old] is a variable with no
       occurrence. *)
    val replace_with: var -> var -> unit
    val replace_all_non_recursive_occurrences_of_with: var -> var -> unit
    (*i TODO: functions to replace_with only on
       recursive/non-recursive occurrences? i*)

    val description : var -> var_desc
    val set_description : var -> var_desc -> unit
    val to_string : var -> string

    (* include Key.S with type t := var *)
    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur :
  sig
    (* [make (var,occur_type)] creates a [Recursive] or
       [Non_recursive] occurrence of [var]. The pair (var,occur_type)
       thus allows to create a new occurrence, and is called a
       [maker]. [maker] and [rec_maker] are helper functions that
       return [maker]s. *)
    type maker = var * occur_type
    val maker: var -> maker
    val rec_maker: var -> maker
    val make : maker -> occur

    (* [delete occur] deletes an occurrence of a variable *)
    val delete: occur -> unit

    (* [binding_variable occur] returns the binding variable of
       [occur], i.e. the variable [var] such that [occur] is an
       occurrence of [var]. *)
    val binding_variable : occur -> var

    val description: occur -> occur_desc
    val set_description: occur -> occur_desc -> unit
    val to_string : occur -> string

    (* include Key.S with type t := occur *)
    module Map : Map.S with type key = occur
    module Set : Set.S with type elt = occur
  end
end

module Make (Desc : DESCRIPTION) : S
  with type var_desc = Desc.variable_description
  and type occur_desc =  Desc.occurrence_description;;
