module Safe = Union_find.Safe;;

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
  assert ( try ignore( A.description t2 part1); false
    with _ -> true);
  assert ( try  A.set_description t2 part1 "toto"; false
    with _ -> true);
  assert ( try ignore( A.find t2 elt1); false
    with _ -> true);
  assert (try ignore( A.union t2 part1 part2 "1 and 2"); false
    with _ -> true );

  (* Merging partitions from different union-find structures fails. *)
  assert (try ignore( A.union t part1 part4 "1 and 4"); false
    with _ -> true );
  assert (try ignore( A.union t2 part1 part4 "1 and 4"); false
    with _ -> true );

  let part12 = A.union t part1 part2 "1 and 2" in
  assert (A.find t elt1 = part12);
  assert (A.find t elt2 = part12);
  assert (A.find t elt3 <> part12);
  assert (A.description t part12 = "1 and 2");
  
  A.set_description t part12 "1 and 2 bis";
  assert (A.description t part12 = "1 and 2 bis");

  ();;

autotest();;

print_endline "test ok";;


module Fast = Union_find.Fast;;

type testf = { xf:int; mutable zf:(string, testf) Fast.link };;

module Testf = struct
  type description = string
  type element = testf
  let get t = t.zf
  let set t z = t.zf <- z
end

module Af = Fast.Make(Testf)

let autotest() = 
  let t = Af.create()  in

  let elt1 = {xf=1; zf=Fast.empty_link} in
  let elt2 = {xf=2; zf=Fast.empty_link} in
  let elt3 = {xf=3; zf=Fast.empty_link} in

  let part1 = Af.singleton t elt1 "1" in
  let part2 = Af.singleton t elt2 "2" in
  let part3 = Af.singleton t elt3 "3" in

  assert (part1 <> part2);
  assert (part1 <> part3);
  assert (part2 <> part3);

  assert (Af.description t part1 = "1");
  assert (Af.find t elt1 = part1);

  let part12 = Af.union t part1 part2 "1 and 2" in
  assert (Af.find t elt1 = part12);
  assert (Af.find t elt2 = part12);
  assert (Af.find t elt3 <> part12);
  assert (Af.description t part12 = "1 and 2");
  
  Af.set_description t part12 "1 and 2 bis";
  assert (Af.description t part12 = "1 and 2 bis");

  ();;

autotest();;

print_endline "test ok";;
