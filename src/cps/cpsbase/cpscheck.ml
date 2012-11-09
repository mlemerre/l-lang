(* Copyright 2012 Matthieu Lemerre *)

(*s This file performs some sanity checks on the CPS data structures;
  it checks that code transformation functions do not mess (too much)
  with the data structures.

  The checks that it performs are:
  \begin{description}

  \item[uplink]: Checks that each entity contains the correct uplink.
  There are currently uplinks from subterms to their enclosing
  [Let_prim] or [Let_cont] term, and from binding variables to the
  "binding site" (the term where the variable is bound); the check
  must be completed when more the CPS structure contains more
  uplinks.

  \item[occurrences]: Checks that the doubly linked list of
  occurrences in [Cpsvar], maintained for each variable, is accurate;
  i.e. that all variables in CPS terms are in the doubly linked lists,
  and that all terms in the doubly linked list are in a CPS term. The
  latter can happen, for instance, if one forget to call the
  [Var.Occur.delete] function.

  \end{description}

  There are further checks that could be done; for instance:

  \begin{itemize}
  
  \item That all variables are bound (i.e. no variable is used before
  it is bound, also stated as "a variable always dominates its
  uses"). Normal use of the [Cpsbuild] function should already
  prevent that.

  \item That all occurrences of a continuation variable is always
  bound in the same [Lambda]; that all occurrences of a variable is
  always bound in the same [Lambda] once closure conversion was
  performed.

  \end{itemize}

  Also the module should be extended to handle not only terms, but
  whole definitions. *)

open Cpsdef;;

(* For each kind of check, we provide a function to check an entire
   [term]. *)
module type S = sig
  val term: term -> unit
end

(*s This module allows checking the uplinks. We recursively traverse a
  term, and for each term [t] we find, we examine its elements
  (variables, occurrences, subterms...). If the element has an uplink,
  it must point to [t]. *)
module Uplinks:S = struct

  (* The following function is necessary (rather than just comparing 2
     enclosings with [=]) because comparison of uplinks is done with
     [==]; so we must perform a match on the enclosing term before
     comparing it with x[==]. *)
  let check_enclosing_equal_to_term enclosing term = 

    let print_error() = 
      Format.eprintf
        "\nError: wrong uplink: should point to %a@." Cpsprint.term term in

    match enclosing with
    | Enclosing_term t when t == term -> ()
    | Enclosing_term t -> 
      (print_error();
       Format.eprintf "Instead, points to %a@." Cpsprint.term t;
       assert false)
    | _ ->  (print_error();
             assert false)
  ;;

  (* The main function recursively traverse the term, performing all
     the checks. *)
  let rec term t = 

    (* Each of the following helper function performs a complete check
       on the corresponding element (e.g. [var] checks variables,
       [occ] occurences etc. In addition [body] recurses. *)
    let body t' = check_enclosing_equal_to_term t'.enclosing t; term t' in
    let var var =
      let enclosing = (Var.Var.description var).binding_site_var in
      check_enclosing_equal_to_term enclosing t in

    (* Note that [cont_var], [occ] and [cont_occ] do not yet have uplinks. *)
    let cont_var cont_var = () in
    let occ occ = () in
    let cont_occ cont_occ = () in

    let value = function
      | Void | Constant(_) -> ()
      | Tuple(l) -> List.iter occ l
      | Lambda(k,x,b) -> cont_var k; var x; body b in
    
    let prim = function 
      | Value v -> value v
      | Projection(_,t) -> occ t
      | Integer_binary_op(_,a,b) -> occ a; occ b in

    match t.term with
    | Let_prim(x, p, b) -> var x; prim p; body b
    | Let_cont(k,x,bc,b) -> cont_var k; var x; body bc; body b
    | Apply(f,k,x) -> occ f; cont_occ k; occ x
    | Apply_cont(k,x) -> cont_occ k; occ x
    | Halt(x) -> occ x
  ;;
end


(*s This module allows checking the occurrences. For each variable, we
  build the [Set] of its occurrences, $A$. Each variable is linked to
  a set of occurrences, represented by a doubly linked list, $B$. We
  check that $A = B$, i.e. that $A \subseteq B \land B \subseteq A$.
  The code performs these two checks separately (in particular because
  $A$ and $B$ have different data structures) *)
module Occurrences:S = struct

  (* This functor factorizes the code common to [Var] and [Cont_var]. *)
  module Make(Var:Cpsvar.S) = struct

    (* For each variable [x] in a term [t], we build a map mapping [x]
       to the set of occurrences of [x] we found in [t]. This map is
       build and used using a depth-first traversal on terms. The term
       data structure requires that the term containing the binding site
       of a variable [v] is an ancestor of all the terms containing
       occurrences of [v]. Thus a depth-first traversal of terms will
       encounter the binding site of the variable on the way down, then
       all occurrences, then the binding site of the variable on the way
       up.

       When we first encounter the variable, we associate it to the
       empty set (function [add_variable]).
       
       When we encounter an occurrence, we update the map associated to
       its binding variable (function [add_occurrence]).

       When we last encounter the variable, we known that we have found
       all occurrences in the term; thus we compare the sets of
       occurrences (function [compare_set]). *)

    (* Return a new map with [x] mapped to an empty set. *)
    let add_variable x map = 
      (* Check that x is not already in the map. *)
      assert( not (Var.Var.Map.mem x map));
      let map = Var.Var.Map.add x (Var.Occur.Set.empty) map in
      map ;;

    (* Return a new map with [o] added to the set of occurrences of [x]. *)
    let add_occurrence o map =
      let x = (Var.Occur.binding_variable o) in 
      try 
        let set = (Var.Var.Map.find x map) in
        let newset = Var.Occur.Set.add o set in
        Var.Var.Map.add x newset map
      with Not_found -> assert false;;

    (* Compare the set of occurrences in the doubly-linked list of x with
       the set given as an argument. Returns void if they are equal, and
       fails if they are not. 

       The algorithm is the same for both kind of set comparisons:
       \begin{enumerate}

       \item First we have or define a function that checks whether one
       element [e] in a set [s] is in the other set [s'], by comparing [e]
       with every element in [s']

       \item We iterate this function on all elements [e] in [s], which
       tells use that $s \subseteq s'$.

       \end{enumerate}

       Once we know that $s \subseteq s'$ and $s' \subseteq s$, we
       deduce that $s = s'$. *)
    let compare_sets x set =

      let check_set_subset_of_doubly_linked_list () =
        let is_in_doubly_linked_list occ = 
          try 
            Var.Var.fold_on_occurrences x () (fun () o ->
              if o == occ then failwith "found");
            false
          with _ -> true in

        Var.Occur.Set.iter (fun occ ->
          if (is_in_doubly_linked_list occ)
          then ()
          else failwith ("Element "^(Var.Occur.to_string occ)^" in CPS term "
                         ^ "was not in the doubly linked list")) set in
      
      let check_doubly_linked_list_subset_of_set () = 
        Var.Var.fold_on_occurrences x () (fun () o -> 
          if Var.Occur.Set.mem o set
          then ()
          else failwith ("Element "^(Var.Occur.to_string o)^" in the "
                         ^ "doubly linked list was not in the CPS term")) in
      
      check_set_subset_of_doubly_linked_list();
      check_doubly_linked_list_subset_of_set()
    ;;

  end;;

  module MakeVar = Make(Var);;
  module MakeContVar = Make(Cont_var);;

  (* The depth-first traversal of the variables and occurrences in the
     term is handled by the [fold_on_variables_and_occurrences]
     function. *)
  let term t = 
    let beforevar (map,contmap) x  = (MakeVar.add_variable x map, contmap) in
    let beforecontvar (map,contmap) k  = (map,MakeContVar.add_variable k contmap) in

    let focc (map,contmap) o = (MakeVar.add_occurrence o map, contmap) in
    let fcontocc (map,contmap) o = (map, MakeContVar.add_occurrence o contmap) in

    let aftervar (map,contmap) x =
      MakeVar.compare_sets x (Var.Var.Map.find x map);
      (map,contmap) in
    let aftercontvar (map,contmap) x =
      MakeContVar.compare_sets x (Cont_var.Var.Map.find x contmap);
      (map,contmap) in
    let init = (Var.Var.Map.empty, Cont_var.Var.Map.empty) in
    ignore(Cpsterm.fold_on_variables_and_occurrences t init
             beforevar focc aftervar
             beforecontvar fcontocc aftercontvar);;
end;;
