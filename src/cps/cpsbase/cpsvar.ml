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
  variable, and merging of occurrence sets.

  We chose to keep normal occurrences, and recursive occurrences, in
  two separate equivalence classes. Not merging them:
  \begin{itemize}
  \item allows to replace the binding variable for only the normal
  occurrences, or only the recursive occurrences;
  \item simplifies the implementation of [replace_with];
  \item makes each equivalence class smaller (and thus slightly more
    efficient).
  \end{itemize} *)

module Union_find = Union_find.Safe;;

(* Note: a minor problem here is that the unique id is shared between
   all applications of Cpsvar.Make, but this is not a huge issue. *)
module UniqueId = Unique.Make(struct end);;

(*s Variables link to their description, to one occurrence and one
  recursive occurrence (if there is one). We also add a unique id to
  allow variable comparison. Also, unique variables remove any need
  for alpha conversion. *)
type ('a,'b) variable =
  { mutable var_desc: 'a initialized;
    mutable occurrences:('a, 'b) occurrence option;
    mutable recursive_occurrences: ('a, 'b) occurrence option;
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

(*s [occur_type] distinguish between normal and recursive occurrences.
  The following functions allows code factorisation. *)
type occur_type = Recursive | Non_recursive;;

let get_occurrences var (ot:occur_type) = match ot with
  | Recursive -> var.occurrences
  | Non_recursive -> var.recursive_occurrences;;

let set_occurrences var (ot:occur_type) value = match ot with
  | Recursive -> (var.occurrences <- value)
  | Non_recursive -> (var.recursive_occurrences <- value);;

(*s The CPSVar functor takes a DESCRIPTION argument as explained in the interface.  *)
module type DESCRIPTION = sig
  type variable_description
  type occurrence_description
  val var_prefix:string
end 

module Make(Desc:DESCRIPTION) = struct

  type var_desc = Desc.variable_description
  type occur_desc = Desc.occurrence_description
  type var = (var_desc, occur_desc) variable
  type occur = (var_desc, occur_desc) occurrence

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
        recursive_occurrences = None;
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
    type number_of_occurrences =
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences

    let number_of_occurrences_ ot var =
      match get_occurrences var ot with
        | None -> No_occurrence
        | Some(occ) -> 
          if occ.previous_occurrence == occ
          then begin
            assert (occ.next_occurrence == occ);
            One_occurrence occ
          end
          else Several_occurrences;;

    let number_of_occurrences = number_of_occurrences_ Non_recursive

    let fold_on_occurrences_ ot var init f =
      match get_occurrences var ot with
        | None -> init
        | Some(occ) -> 
          let rec loop value cur = 
            if cur == occ
            then f value cur
            else loop (f value cur) cur.next_occurrence
          in loop init occ.next_occurrence;;
    let fold_on_occurrences var init f = fold_on_occurrences_ Non_recursive var init f
    let fold_on_recursive_occurrences var init f = fold_on_occurrences_ Recursive var init f


    (*s This function allows "merging" of variables in O(1) time:
      merging the doubly-linked lists is a O(1) operation, and so is
      performing union in the union-find algorithm. *)
    let replace_with old new_ =
      let replace_with_ ot =
        match get_occurrences old ot with
        (* If there were already no occurrences of [old], there is
           nothing to do. *)
        | None -> ()
        | Some(occ_old) as some_occ_old ->
          ((match get_occurrences new_ ot with

          (* If new_ had no occurrences, there is no need for full
             list merge and union-find: we can just update the
             partition description. *)
          | None ->
            set_occurrences new_ ot some_occ_old;
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

           (* [old] can be reused if needed. *)
           set_occurrences old ot None) in

      replace_with_ Recursive;
      replace_with_ Non_recursive
    ;;


    (*s Utility functions. Comparison is done using the unique ids;
      this allows faster comparisons, and avoid looping indefinitely
      when there are cyclic references between a [var] and its
      [description].*)
    let to_string bv = Desc.var_prefix ^ (UniqueId.to_string bv.var_id);;

    module Ord = struct
      type t = var
      let compare a b = compare a.var_id b.var_id
    end

    module Map = Map.Make(Ord);;
    module Set = Set.Make(Ord);;

  end;;

  module Occur = 
  struct

    type maker = var * occur_type;;

    let maker v = (v,Non_recursive);;
    let rec_maker v = (v,Recursive);;

    (*s Create a new occurrence of VAR, i.e. add it to the
      doubly-linked list of all the occurrences of VAR, and to the
      union-find partition of the occurrences of VAR. *)
    let make (var,ot) =
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
      match get_occurrences var ot with
        (* If this is the first occurrence of var. *)
        | None -> 
          let new_occur = self_linked_occur var in
          Var_union_find.singleton ufds new_occur var;
          set_occurrences var ot (Some(new_occur));
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

    (* Delete [occur] from the doubly-linked list of occurrences.
       Following Andrew Kennedy, the union-find data structure is not
       changed. *)
    let delete occur =
      let next = occur.next_occurrence in
      let prev = occur.previous_occurrence in
      let var = binding_variable occur in
      if (occur == next)
      (* One-variable case. *)
      then begin
        assert (occur == prev);
        match (var.occurrences, var.recursive_occurrences) with
        | (Some(a),_) when a == occur -> var.occurrences <- None;
        | (_,Some(a)) when a == occur -> var.recursive_occurrences <- None;
        | _ -> assert false
      end
      else begin
        assert (occur != prev);
        prev.next_occurrence <- next;
        next.previous_occurrence <- prev;
        (match var.occurrences with
        | Some(a) when a == occur -> var.occurrences <- Some(prev)
        | _ ->
          (match var.recursive_occurrences with
          | Some(a) when a == occur -> var.recursive_occurrences <- Some(prev)
          | _ -> ()))

      end;;

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

     (* We compare occurrences by lexical order of (var_id, occur_id).
        We must not use the generic "compare" function, because
        Variables and Occurrences can be part of a circular chain of
        pointers, and "compare" could loop indefinitely  *)
     module Ord = struct
      type t = occur
      let compare a b =
        let unique_id o =
          let x = binding_variable o in
          (x.var_id, o.occur_id) in
        compare (unique_id a) (unique_id b);;
    end

    module Map = Map.Make(Ord);;
    module Set = Set.Make(Ord);;
  end

end

(*i*)
(* The module type has already been described in the interface. *)
module type S = sig
  type var_desc
  type occur_desc
  type var = (var_desc, occur_desc) variable
  type occur = (var_desc, occur_desc) occurrence

  module Var :
  sig
    val make : unit -> var
    type number_of_occurrences =
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences
    val number_of_occurrences: var -> number_of_occurrences
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a
    val fold_on_recursive_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a
    val replace_with: var -> var -> unit
    val description : var -> var_desc
    val set_description : var -> var_desc -> unit
    val to_string : var -> string
    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur :
  sig
    type maker = var * occur_type
    val maker : var -> maker
    val rec_maker : var -> maker
    val make : maker -> occur
    val delete: occur -> unit
    val binding_variable : occur -> var
    val description: occur -> occur_desc
    val set_description: occur -> occur_desc -> unit
    val to_string : occur -> string
    module Map : Map.S with type key = occur
    module Set : Set.S with type elt = occur
  end
end
(*i*)
