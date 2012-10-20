(* Copyright 2012 Matthieu Lemerre *)

(* There are two structures where information about variables is
   attached. [occurrence] stores data about the use of a variable
   (like the location of the use). [variable] stores data common to
   all the uses of the variable, like its type, or the location where
   the variable is bound. *)
type ('a, 'b) variable
type ('a, 'b) occurrence

(* The user of the module must configure the information it wants to
   attach to a [variable] or [occurrence] (using the DESC structure).
   This information is called a [description].

   The [make] functions, that create new variables and occurence, do
   not take an initial [description] argument. This allows to break
   circular dependencies, e.g. where a term points to a variable or
   occurrence, and a variable points to the term that bind or use it.

   Instead, for both the variable and its occurrence, the description
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
module Make (Desc : DESCRIPTION) :
sig
  type var = (Desc.variable_description, Desc.occurrence_description) variable
  type occur = (Desc.variable_description, Desc.occurrence_description) occurrence

  module Var :
  sig

    (* [make ()] create a new variable. *)
    val make : unit -> var

    (* The following function differentiates, in O(1) time, between
       the 0-occurrence case (the variable is useless), the
       1-occurrence case (a good candidate for inlining), and the case
       with more than one occurrence. *)
    type occurrence_number = 
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences
    val occurrence_number: var -> occurrence_number

    (* This function allows iteration on occurrences. The order is
       arbitrary. *)
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a

    (* This function "merges" variables; more specifically it makes
       all occurrences of [old] become occurrence of [new]. After the
       call [old] is a variable with no occurrence. *)
    val replace_with: var -> var -> unit

    val description : var -> Desc.variable_description
    val set_description : var -> Desc.variable_description -> unit
    val to_string : var -> string

    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur :
  sig

    (* [make var] creates a new occurrence of [var]. *)
    val make : var -> occur

    (* [binding_variable occur] returns the binding variable of
       [occur], i.e. the variable [var] such that [occur] is an
       occurrence of [var]. *)
    val binding_variable : occur -> var

    val description: occur -> Desc.occurrence_description
    val set_description: occur -> Desc.occurrence_description -> unit
    val to_string : occur -> string
  end
end
