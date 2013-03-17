(* Copyright 2013 Matthieu Lemerre *)

(* Additional helper pretty-printing functions. *)
let print_list ?(empty="") ?(open_paren="") ?(close_paren="") ?(sep=format_of_string " ") f ppf l = 
  let rec pr_aux ppf = function
    | [] -> ()
    (* | a::b -> Format.fprintf ppf "%s%a%a" sep pr_el a pr_aux b in *)
    | a::b -> Format.fprintf ppf sep; Format.fprintf ppf "%a%a" f a pr_aux b in
  match l with
    | [] -> Format.fprintf ppf "%s" empty
    | a::b -> Format.fprintf ppf "%s%a%a%s" open_paren f a pr_aux b close_paren


