(* Copyright 2012 Matthieu Lemerre. *)

(*s A union-find data structure maintains a partition of elements into
  disjoint sets.

  It allows to add new elements in new partitions, perform the union
  of two partitions, and retrieve the partition in which is an
  element. Moreover it allows to attach a description to a partition,
  which is generally the point of using such a structure.

  This module has side effects: adding an element to a union-find
  data structure changes that element, and the union operation merges
  the partitions destructively. This make it easy to use this module
  incorrectly. To that end, a number of protections (using types and
  dynamic checks) are set that detect such incorrect uses of the
  module. 

  Note on the name: there are other data structures that maintain
  disjoint sets with other operations, such as partition refinement,
  so "union-find" is a more accurate name for this data structure
  than "disjoint set".  *)
module type S =
sig

  (* The t type represents the whole union-find data structure. A
     partition always belong to some t; elements belong to a t once
     there has been a "singleton" operation on them. 

     All the functions (except [create]) take a [t] argument; in their
     safe version this argument is used to checks that other
     [element] and [partition] arguments indeed belong to the [t]
     argument. *)
  type t
  type partition
  type element
  type description

  (* [create()] returns a new empty union find data structure. *)
  val create : unit -> t

  (* [singleton t e d] adds a new element [e] to [t], and create and
     returns a new partition [p] in [t], such that [e] is the only
     element of [p]. It also attach the description [d] to [p].

     The safe version checks that [e] was not previously added to
     another union-find data structure (with the same link). *)
  val singleton : t -> element -> description -> partition

  (* [find t e] returns the partition [p] of [t] that contains [e]. *)
  val find : t -> element -> partition

  (* [union t p1 p2 d] creates a new partition [p3], with description
     [d], that contains the union of all the elements in [p1] and
     [p2]. The [p1] and [p2] arguments are consumed, i.e. must not be
     used after they were passed to [union]. [p1] and [p2] must be
     different partitions. *)
  val union : t -> partition -> partition -> description -> partition

  (* [description t p] returns the description associated to [p]. *)
  val description : t -> partition -> description

  (* [description t p] changes the description associated to [p]. *)
  val set_description : t -> partition -> description -> unit
end

(*s We defined two types of "union-find makers": Fast and Safe. Both
  propose a [link] type, and each element of a union-find structure
  must be "associated" to one different link (generally the link is a
  mutable field in the element type). Initially, the link value is
  [empty_link].

  The [Make] functor, once told how access the link of an element,
  returns a module complying to [S]. Below we given an exemple of
  usage.

  Note: It is possible for an element to be present in two different
  union-find data structures; it must just have different links. 

  If the link in an element must be re-used for another union-find
  data structure, then it must be set to [empty_link], and one must
  stop using the union-find data structure that contained the element
  (even with other elements). *)
module type UNION_FIND = sig
  type ('a, 'b) link
  val empty_link:('a,'b) link

  module type LINK =
  sig
    type element
    type description
    val get : element -> (description, element) link
    val set : element -> (description, element) link -> unit
  end

  module Make(Link : LINK):S with type description = Link.description and type element = Link.element
end

(* The difference between the fast and safe version is that safe
   performs additional checks. The performance difference is small, so
   the Safe version should be prefered. *)
module Fast:UNION_FIND;;
module Safe:UNION_FIND;;

(*s Exemple of usage:

  [type test = { x:int; mutable z:(string, test) Union_find.Safe.link };;]


  [module Test = struct
  type description = string
  type element = test
  let get_link t = t.z
  let set_link t z = t.z <- z
  end]

  [module A = Union_find.Safe.Make(Test)]

  [let uf = A.create() in
  let elt1 = {x=1; z=Safe.empty_link} in
  let part1 = A.singleton t elt1 "1" in
  assert(A.description t (A.find t elt1) = "1")] *)
