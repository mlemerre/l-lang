(* Copyright 2013 Matthieu Lemerre *)
(* Unit test for cpsconversion. Testing is currently done by manual
   inspection of the output (+ calls to Cpscheck). *)

open Cpsbase;;
open Build;;

let top_convert = Cpsclosureconversion.top_convert;;

(* Test with a single lambda. *)
let test1 =
  let_cont (fun x -> halt x) (fun k ->
    let_constant (Constant.Integer 17) (fun c17 ->
      let_lambda
        (fun (k,x) ->
          let_integer_binary_operation (Constant.Ibop.Add) x c17 (fun res ->
            apply_cont k res))
        (fun lambd ->
          let_constant (Constant.Integer 11) (fun c11 ->
            apply Closure lambd k [c11]))));;

(* Test with two imbricated lambdas; the inner lambda has two free
   variables, one in the top environment, one in the outer lambda. *)
let test2 =
  let_cont (fun x -> halt x) (fun k ->
    let_constant (Constant.Integer 11) (fun c11 ->
      let_lambda (fun (k,x) ->
        let_constant (Constant.Integer 44) (fun c44 ->
          let_lambda (fun (k,x) ->
            let_integer_binary_operation (Constant.Ibop.Sub) c44 c11 (fun res ->
              apply_cont k res)) (fun lambda ->
                let_void (fun void ->
                  apply Closure lambda k [void]))))
        (fun lambda ->
          let_void (fun void ->
            apply Closure lambda k [void]))));;

(* Simple test of an injection. *)
let test3 =
  let_constant (Constant.Integer 12) (fun c12 ->
    let_inj 0 2 c12 (fun inj ->
      let_cont (fun x -> halt x) (fun k ->
        case inj [(0,k)] ~default:(let_constant (Constant.Integer 13) (fun c13 -> halt c13)))));;

let tests = [test1; test2(* ; test3 *)];;

let print_free_set s = Cpsfree.VarSet.iter (fun v -> Printf.printf "%s " (Var.Var.to_string v)) s;;

let print_free_map m =
  Cpsfree.VarMap.iter
    (fun x s ->
      Printf.printf "free variables for %s: "(Var.Var.to_string x);
      print_free_set s;
      print_string "\n") m;;

let do_test test = 
  let print_and_inspect test = 
    Print.debug_term test;
    print_endline "\n----";
    Check.Uplinks.term test;
    Check.Occurrences.term test;
    let (_,free_map) = Cpsfree.term test in
    print_free_map free_map in
  print_and_inspect test;
  top_convert test;
  print_and_inspect test;;

List.iter do_test tests;;
