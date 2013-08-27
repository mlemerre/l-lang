(* Copyright 2012 Matthieu Lemerre *)

(*s We represent each disjoint set by a tree : elements are in the
  same set than the element that they point to.

  The root of the tree is the representative of the set, and
  corresponds to elements of type [partition]. It points to a
  "partition descriptor". *)
type ('a,'b) baselink =                          
  | Partition_descriptor of 'a partition_descriptor
  | Parent of 'b

(* The partition descriptor contains the user-accessible description,
   and a rank, used to optimize the union operation.

   Note that the partition descriptor is not accessible by the users
   of the module, and the interface make it so that there can be only
   one link to the partition descriptor (from the representative).
   This allows to update the partition descriptor destructively. *)
and 'a partition_descriptor = { mutable rank:rank; mutable desc:'a }

(* The rank of a partition is is a majorant of the distance of its
   elements to the root (path compression makes so that the height of
   the tree can be lower than the rank). The union operation minimizes
   the rank, and thus the height of the tree. *)
and rank = int;;

(*s The implementation is parametrized by the safety checks that we
  perform (which differs between the Fast and Safe modules). 

  The safe module identifies all union-find data structures by a
  unique id, embed that in the links, and checks for all operation
  that they are equal. It also checks initialization of the link. *)


module type SAFETY = sig
  type t
  val create: unit -> t
  type ('a,'b) link

  (*c Create a safe link from a baselink. *)
  val securize: t -> ('a,'b) baselink -> ('a,'b) link

  (*c Returns the base_link from the safe link. *)
  val get_base: ('a,'b) link -> ('a,'b) baselink

  (*c Check the safe link withat the element (and the safe link) belong to t. *)
  val check_membership: t -> ('a,'b) link -> unit

  (*c Check that the element is not yet part of any union find. *)
  val check_unused: ('a,'b) link -> unit

  (*c Initial link. *)
  val empty_link: ('a,'b) link
end;;

module No_safety:SAFETY = struct
  type t = unit
  let create() = ()
  type ('a,'b) link = ('a,'b) baselink;;
  let securize () l = l;;
  let check_membership () l = ();;
  let check_unused l = ();;
  let get_base l = l;;
  (*c Note: This cast can make the execution fail without notice. *)
  let empty_link = Obj.magic 0          
end

type unique = int

module Unique = Unique.Make(struct end)

module Safety:SAFETY = struct
  type t = Unique.t
  let create() = Unique.fresh();;
  type ('a,'b) link = t option * ('a,'b) baselink;;
  let securize u l = (Some u,l);;
  let check_membership t (u,_) = 
    (match u with
      | Some(a) -> assert (t == a) (*r The element is in another union-find structure. *)
      | None -> assert false); () (*r The element is in no union-find structure.  *);;
  let check_unused (u,_) = 
    (match u with
      | Some(_) -> assert false (*r  The element is already in a union-find structure.  *)
      | None -> ());;
  let get_base (_,l) = l;;
  (*c Note: The cast is not dangerous, because the left-hand part is
    checked first. *)
  let empty_link = (None, Obj.magic 0)  
end

(*s The goal of the below "double functor" is to produce a module with
  the following signature. In it, [partition] and [element] are
  actually the same underlying type; the difference is that elements
  returned with type [partition] are the root of the tree). Hiding
  this in the interface provides some guarantee that arguments of type
  partition are the representative of their partition.

  Unfortunately, after calling [union] on two partitions [p1] and
  [p2], one of them will stop being the root; that is why the
  partition arguments of [union] must not be re-used. Thus, defining
  the [partition] type only guarantees that the argument has been a
  root in the past, and we ensure that by a dynamic test. *)
module type S = sig
      type t
      type partition
      type element
      type description
      val create: unit -> t
      val singleton : t -> element -> description -> partition
      val find : t -> element -> partition
      val union: t -> partition -> partition -> description -> partition
      val description: t -> partition -> description
      val set_description : t -> partition -> description -> unit
end

(*i*)  
(* These module types have already been explained in the interface. *)
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

  module Make(Elt : LINK):S with type description = Elt.description and type element = Elt.element
end
(*i*)

(* This is a double functor with two arguments; [Saf] allows to
   differenciate the "Fast" and "Safe" modules, while [Link] is used
   to find and change the link. *)
module Make(Saf:SAFETY):UNION_FIND = 
struct

  type ('a,'b) link = ('a,'b) Saf.link;;
  let empty_link = Saf.empty_link;;

  module type LINK =  sig
    type element
    type description
    val get: element -> (description, element) link 
    val set: element -> (description, element) link -> unit
  end;;

  module Make(Link: LINK) =
  struct
    type t = Saf.t
    type element = Link.element
    type description = Link.description
    type partition = Link.element

    let create = Saf.create;;

    (*s [singleton] is the only way to add new elements to the
      union-find structure, and is the place where we check that the
      element is not part of another structure. *)
    let singleton t elt desc = 
      let l = (Link.get elt) in
      Saf.check_unused l;
      Link.set elt (Saf.securize t (Partition_descriptor {rank=0;desc=desc}));
      elt ;;

    (*s Basically, [find] just walks the tree until it finds the root.

      But performance is increased if the length of the path is
      diminished: traversed nodes are linked to nodes that are closer
      to the roof. The possibility we have implemented is path
      compression: when the root is found, the elements are changed
      to link to the it, so that subsequent calls are faster. We
      implemented a tail-recursive version of this algorithm (which
      still requires two pass).
      
      Note: there are alternatives to path compression, such that
      halving; but in Tarjan's structure the root is linked to
      itself, which is not the case here, so halving would require
      more checks than in Tarjan's version. Thus we stick with path
      compression. 

      Note: we could perform a lighter check in the safe version by
      checking only the argument, and not all recursive calls; this is
      probably not worth implementing it, and the heavy check has its
      uses. *)
    let find t x = 
      (* Tail-recursive function to find the root of the algorithm. *)
      let rec find x = 
        let l = (Link.get x) in
        Saf.check_membership t l;
        match Saf.get_base l with
          | Partition_descriptor(s) -> x
          | Parent(y) -> find y in
      (* This is also tail-recursive, but we do not perform the checks
         the second time.*)
      let rec compress x r = 
        let l = (Link.get x) in
        match Saf.get_base l with
          | Partition_descriptor(s) -> ()
          | Parent(y) -> Link.set x (Saf.securize t (Parent r)) in
      let root = find x in
      compress x root;
      root;;
        
    (*s The following functions work only when the given element is
      the root of a partition, but check that. *)
    let get_partition_descriptor t p = 
      let l = (Link.get p) in
      Saf.check_membership t l;
      match Saf.get_base l with
        | Partition_descriptor(s) -> s
        | _ -> assert(false) (*r The element is not a partition. *);;

    let description t x = (get_partition_descriptor t x).desc;;

    let set_description t x desc = 
      let pd = get_partition_descriptor t x in
      pd.desc <- desc;;
    
    (*s This function performs the union of two partitions. We use
      rank to find which should be the root : we attach the smaller
      tree to the root of the larger tree, so as not to increase the
      maximum height (i.e. path length) of the resulting tree.

      The last argument allows to update the set descriptor along
      with this operation.

      Note that this function takes partitions as argument; one could
      have instead taken any element, and performed the find inside
      the function; in particular some efficient algorithms
      interleave the find and union operations. The reason why we
      take partition arguments is that it avoids a find when we know
      that the argument is a partition (for instance when merging
      with a just-created singleton), and the user needs to perform a
      find to retrieve and merge the description in the algorithms we
      use (such as unification). *)
    let union t p1 p2 newdesc = 

      (* This function also checks that p1 and p2 are partitions. *)
      let d1 = get_partition_descriptor t p1 in
      let d2 = get_partition_descriptor t p2 in

      (* Alternatively, the check that p1 and p2 are different could
         have been done here. *)
      assert (p1 != p2);
      if( d1.rank < d2.rank) then 
        begin 
	  (* Keep [d2_repr] as root. Height of the merge is
             [max(d1_height + 1, d2_height)] so does not change. *)
          Link.set p1 (Saf.securize t (Parent p2));
          d2.desc <- newdesc;
          p2
        end
      else if (d1.rank > d2.rank) then
        begin 
	  (* Keep [d1_repr] as root. Height of the merge is
             [max(d2_height + 1, d1_height)] so does not change. *)
          Link.set p2 (Saf.securize t (Parent p1));
          d1.desc <- newdesc;
	  p1
        end
      else 
        begin
	  (* We choose arbitrarily p1 to be the root. 
	     The height may have changed, as all elements in the subset
	     with root p2 are 1 step further to the root. *)
	  Link.set p2 (Saf.securize t (Parent p1));
          d1.rank <- d1.rank + 1; d1.desc <- newdesc;
          p1
        end
  end;;
end;;

(*s The double-functor is not shown in the exposed interface, and we
  only export the following, simpler modules. *)
module Fast=Make(No_safety);;
module Safe=Make(Safety);;

(*s For a survey of the implementations of union-find algorithms, one
  should read "Worst-Case Analysis of Set Union Algorithms", by
  Tarjan and Van Leeuwen.

  Recent performance comparison of these algorithms (and modern
  enhancements) can be found in "Experiments on Union-Find Algorithms
  for the Disjoint-Set Data Structure", by Md. Mostofa Ali Patwary,
  Jean Blair, Fredrik Manne. *)


(*i*)
type test = { x:int; mutable z:(string, test) Safe.link };;

module Test = struct
  type description = string
  type element = test
  let get t = t.z
  let set t z = t.z <- z
end

module A = Safe.Make(Test)

let autotest() = 
  let t = A.create()  in
  let t2 = A.create() in 

  assert (t <> t2);

  let elt1 = {x=1; z=Safe.empty_link} in
  let elt2 = {x=2; z=Safe.empty_link} in
  let elt3 = {x=3; z=Safe.empty_link} in
  let elt4 = {x=4; z=Safe.empty_link} in


  let part1 = A.singleton t elt1 "1" in
  let part2 = A.singleton t elt2 "2" in
  let part3 = A.singleton t elt3 "3" in
  let part4 = A.singleton t2 elt4 "4" in

  assert (part1 <> part2);
  assert (part1 <> part3);
  assert (part2 <> part3);

  (* Should fail: the element is already in a set.  *)
  assert ( try ignore(A.singleton t elt1 "smth"); false
    with _ -> true);

  assert (A.description t part1 = "1");
  assert (A.find t elt1 = part1);

  (* Using the wrong union-find structure, i.e. t2 instead of t,
     should fail. *)
  assert ( try  ignore(A.description t2 part1); false with _ -> true);
  assert ( try  ignore(A.set_description t2 part1 "toto"); false with _ -> true);
  assert ( try  ignore(A.find t2 elt1); false with _ -> true);
  assert (try ignore(A.union t2 part1 part2 "1 and 2"); false with _ -> true );

  (* Merging partitions from different union-find structures fails. *)
  assert (try ignore(A.union t part1 part4 "1 and 4"); false with _ -> true );
  assert (try ignore(A.union t2 part1 part4 "1 and 4"); false with _ -> true );

  let part12 = A.union t part1 part2 "1 and 2" in
  assert (A.find t elt1 = part12);
  assert (A.find t elt2 = part12);
  assert (A.find t elt3 <> part12);
  assert (A.description t part12 = "1 and 2");
  
  A.set_description t part12 "1 and 2 bis";
  assert (A.description t part12 = "1 and 2 bis");

  ();;

(* autotest();; *)

(*i*)
