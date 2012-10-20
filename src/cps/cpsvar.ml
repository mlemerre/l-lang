(* Copyright 2012 Matthieu Lemerre *)

(*s This file implements links between variables, following the
  "Compiling with continuations, continued" paper by Andrew Kennedy,
  and "Shrinking lambda expressions in linear time", by Andrew Appel
  and Trevor Jim.

  Occurrences are linked to one another into a doubly-linked list,
  such that the doubly-linked list represents the set of all
  occurrences of a variable. A variable is linked to one of its
  occurrence (and thus to the set of all occurrences of the
  variable). This allows adding occurrence, removing occurence, and
  knowing if there is 0,1, or more occurrence to a variable, in O(1)
  time.

  The set of all occurrences of all variables is partitionned into
  same-binder equivalence classes using a union-find data structure,
  i.e. all the members of a partition are occurrences of the same
  variable. The description of the partition is the binding variable
  itself. This allows near-constant time access to the binding
  variable, and merging of occurrence sets. *)

module Union_find = Union_find.Safe;;

(* Note: a minor problem here is that the unique id is shared between
   all applications of Cpsvar.Make, but this is not a huge issue. *)
module UniqueId = Unique.Make(struct end);;

(*s Variables link to their description, and to one occurrence (if
  there is one). We also add a unique id to allow variable comparison.
  Also, unique variables remove any need for alpha conversion. *)
type ('a,'b) variable =
    { mutable var_desc: 'a initialized;
      mutable occurrences:('a, 'b) occurrence option;
      var_id: UniqueId.t;
      mutable occur_nb: int }

(* Occurrences need a description; a "union-find link" (to retrieve
   the binding variable, which is the description of the union-find
   partition); and the links of the doubly-linked lists. We also
   number the occurrences of a variable, to help in debugging and ease
   comparison between occurrences. *)
and ('a,'b) occurrence = 
    { mutable occur_desc: 'b initialized;
      mutable link_var: (('a, 'b) variable,('a, 'b) occurrence) Union_find.link;
      mutable next_occurrence:('a, 'b) occurrence;
      mutable previous_occurrence:('a, 'b) occurrence;
      occur_id: int }

(* We have to check that the user does not retrieve the description
   until the variable is initialized, or that it does not overwrite an
   already initialized description. *)
and 'a initialized = Undef | Initialized_to of 'a;;


(*s The CPSVar functor takes a DESCRIPTION argument as explained in the interface.  *)
module type DESCRIPTION = sig
  type variable_description
  type occurrence_description
  val var_prefix:string
end 

module Make(Desc:DESCRIPTION) = struct

  type var = (Desc.variable_description, Desc.occurrence_description) variable
  type occur = (Desc.variable_description, Desc.occurrence_description) occurrence

  (*s Occurrences contain a link variable, used by the union-find
    functor. The description of a partition of an occurrence [occur]
    is the variable [var] for which [occur] is an occurrence. *)
  module Var_union_find = Union_find.Make(struct
    type description = var
    type element = occur
    let set f v = f.link_var <- v
    let get f = f.link_var
  end)

  (* The Union-find module requires a "union-find data structure"
     (ufds) as its first argument. Ideally, we should have one such
     ufds per CPS term, as a variable and all its occurrences cannot
     be shared between several terms. However, using a single ufds for
     all terms in a global variable is more practical (no need to pass
     an additional argument). Moreover, confusion between partitions
     of several terms should be relatively easy to avoid, so checking
     that with several ufds is not absolutely necessary. *)
  let ufds = Var_union_find.create();;

  module Var = struct

    (*s The tryptic make/description/set_description allows building
      circular structures without simultaneous definitions.
      [var_counter] provides unique identifiers when displaying
      variables. *)

    let make() =
      { var_desc = Undef;
        occurrences = None;
        var_id = UniqueId.fresh();
        occur_nb = 0 } ;;

    let description b = match b.var_desc with
      | Initialized_to(d) -> d
      | Undef -> assert false (*r Trying to access an uninitialized description. *)

    let set_description b d = match b.var_desc with
      | Initialized_to(d) -> assert false   (*r Description already initialized. *) 
      | Undef -> b.var_desc <- Initialized_to(d) 

    (*s Occurrence number can return in O(1) whether a variable has
      no, one, or more than one occurrence. For that we must not
      return the list of occurrence if there are several; instead the
      user must then use the [fold_on_occurrences] function. *)
    type occurrence_number = 
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences

    let occurrence_number var = 
      match var.occurrences with
        | None -> No_occurrence
        | Some(occ) -> 
          if occ.previous_occurrence == occ
          then begin
            assert (occ.next_occurrence == occ);
            One_occurrence occ
          end
          else Several_occurrences;;

    let fold_on_occurrences var init f = 
      match var.occurrences with
        | None -> init
        | Some(occ) -> 
          let rec loop value cur = 
            if cur == occ
            then f value cur
            else loop (f value cur) cur.next_occurrence
          in loop init occ.next_occurrence;;

    (*s This function allows "merging" of variables in O(1) time:
      merging the doubly-linked lists is a O(1) operation, and so is
      performing union in the union-find algorithm. *)
    let replace_with old new_ =
      match old.occurrences with
        (* If there were already no occurrences of [old], there is
           nothing to do. *)
        | None -> ()
        | Some(occ_old) -> ((match new_.occurrences with

            (* If new_ had no occurrences, there is no need for full
               list merge and union-find: we can just update the
               partition description. *)
            | None ->
              new_.occurrences <- Some(occ_old);
              let part_old = Var_union_find.find ufds occ_old in
              Var_union_find.set_description ufds part_old new_

            (* The non-trivial case: merge the two circular
               doubly-linked lists into one big circular doubly-linked
               list. Also works on single-elements lists. *)
            | Some(occ_new) ->
              occ_old.next_occurrence.previous_occurrence <- occ_new.previous_occurrence;
              occ_new.previous_occurrence.next_occurrence <- occ_old.next_occurrence;
              occ_old.next_occurrence <- occ_new;
              occ_new.previous_occurrence <- occ_old;
              let (part_old,part_new) = (Var_union_find.find ufds occ_old,
                                         Var_union_find.find ufds occ_new) in
              ignore(Var_union_find.union ufds part_old part_new new_));
          old.occurrences <- None)
    ;;


    (*s Utility functions. Comparison is done using the unique ids;
      this allows faster comparisons, and avoid looping indefinitely
      when there are cyclic references between a [var] and its
      [description].*)
    let to_string bv = Desc.var_prefix ^ (UniqueId.to_string bv.var_id);;

    module Map = Map.Make(struct
      type t = var
      let compare a b = compare a.var_id b.var_id
    end)

    module Set = Set.Make(struct
      type t = var
      let compare a b = compare a.var_id b.var_id
    end)

  end;;

  module Occur = 
  struct

    (*s Create a new occurrence of VAR, i.e. add it to the
      doubly-linked list of all the occurrences of VAR, and to the
      union-find partition of the occurrences of VAR. *)
    let make var = 
      (* Create an initial self-linked occurrence of var.  *)
      let self_linked_occur var = 
        var.occur_nb <- var.occur_nb + 1;
        let rec occur = { occur_desc = Undef;
                          link_var = Union_find.empty_link;
                          next_occurrence = occur;
                          previous_occurrence = occur;
                          occur_id = var.occur_nb; } 
        in occur
      in
      match var.occurrences with
        (* If this is the first occurrence of var. *)
        | None -> 
          let new_occur = self_linked_occur var in
          Var_union_find.singleton ufds new_occur var;
          var.occurrences <- Some(new_occur);
          new_occur
        (* Else there are already some occurrences of var. *)
        | Some(existing_occur) -> 
          let new_occur = self_linked_occur var in
          (* Insert new between existing and existing.next. *)
          let following_occur = existing_occur.next_occurrence in
          let new_occur = { new_occur
                            with next_occurrence = following_occur;
                              previous_occurrence = existing_occur } in
          existing_occur.next_occurrence <- new_occur;
          following_occur.previous_occurrence <- new_occur;
          (* Merge new to the partition of occurrences of var. *)
          let new_part = Var_union_find.singleton ufds new_occur var in
          let existing_part = (Var_union_find.find ufds existing_occur) in
          Var_union_find.union ufds new_part existing_part var;
          new_occur;;

    (* Returns the variable of which [occur] is an occurrence. *)
    let binding_variable occur =
      let part = Var_union_find.find ufds occur in
      Var_union_find.description ufds part;;

    (* Similarly to [Var], description can be get and set only once.  *)
    let description b = match b.occur_desc with
      | Initialized_to(d) -> d
      | Undef -> assert false (*r Trying to access an uninitialized description. *)

    let set_description b d = match b.occur_desc with
      | Initialized_to(d) -> assert false   (*r Description already initialized. *)
      | Undef -> b.occur_desc <- Initialized_to(d) 

    (* We suffix the name of the variable by the occurrence number,
       which may be useful to follow code transformations steps. *)
    let to_string occur = 
      let bv = binding_variable occur in
      (Var.to_string bv) ^ "_" ^ (string_of_int occur.occur_id);;
  end

end
