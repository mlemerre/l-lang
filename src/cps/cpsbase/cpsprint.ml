(* Copyright 2012 Matthieu Lemerre *)

(* These functions output a printed representation of a CPS term. One
   problem of the CPS representation is that it is difficult to read,
   especially because of the deep level of nesting. We tried to
   simplify this representation by not showing the nesting explicitely
   to the user; instead a parent always has the same level of
   indentation than its children in the tree. The syntax looks a lot
   like assembly language, or SSA representation.

   Still, this representation could be readily parsed back into a CPS
   data structure if needed. *)

open Format;;
open Cpsdef;;

let rec var ppf v = fprintf ppf "%s" (Var.Var.to_string v)
and occur ppf v = fprintf ppf "%s" (Var.Occur.to_string v)
and cont_var ppf v = fprintf ppf "%s" (Cont_var.Var.to_string v)
and cont_occur ppf v = fprintf ppf "%s" (Cont_var.Occur.to_string v)

and var_list ppf vl = (Make_printer.list ~sep:", " var) ppf vl
and occur_list ppf vl = (Make_printer.list ~sep:", " occur) ppf vl

and term ppf t = match Term.get t with
  | Halt v ->
      fprintf ppf "halt %a" occur v
  | Apply(_,callerv,k,callees) ->
      fprintf ppf "%a( %a, %a)"
        occur callerv cont_occur k occur_list callees
  | Apply_cont(k,v) ->
      fprintf ppf "%a( %a)"
        cont_occur k occur v
  | Let_cont(k,v,loccont,incont) ->
      (* fprintf ppf "do@\n{ @[<v>%a@]@\n} where %a( %a) =@\n{
         @[<v>%a@]}@]" *)
      fprintf ppf "decl %a@\n@[<v>%a@]@\n@\n%a( %a) = {@\n  @[<v>%a@]@\n}@]" cont_var k
        term incont cont_var k var v term loccont
  | Let_prim(v,p,body) ->
      fprintf ppf "@[<v>let %a = %a in@ %a@]"
        var v prim p term body
  | Case(v,l,d) ->
    let case ppf (i,k) = fprintf ppf "%d -> %a" i cont_occur k in
    let case_list ppf l = Make_printer.list ~sep:"@\n" case ppf l in
    let default ppf d =
      (match d with
      | None -> ()
      | Some(t) -> fprintf ppf "@\ndefault -> %a" term t) in
    fprintf ppf "case(%a)@\n{ @[<v>%a%a@]@\n}" occur v case_list l default d

and prim ppf = function
  | Value v -> value ppf v
  | Projection(i,x) ->
      fprintf ppf "#%d( %a)"
        i occur x
  | Integer_binary_operation(op,v1,v2) ->
      fprintf ppf "%s( %a, %a)"
        (Constant.integer_binary_operation_to_string op) occur v1 occur v2
  | Integer_binary_predicate(pred,v1,v2) ->
      fprintf ppf "%s( %a, %a)"
        (Constant.integer_binary_predicate_to_string pred) occur v1 occur v2

and value ppf = function
  | Constant(c) ->
      fprintf ppf "%s" (Constant.to_string c)
  | Tuple(l) ->
      fprintf ppf "( %a)" occur_list l
  | Injection(i,j,x) -> fprintf ppf "inj_{%d/%d}( %a)" i j occur x
  | Lambda(_,k,vl,term_) ->
      fprintf ppf "@,@[<v>{ @[<v>%a -> (%a) ->@\n@[<v>%a@]@]@\n}@]"
        cont_var k var_list vl term term_


let debug_term t =
  Format.eprintf "----------------------------------------\n";
  Format.eprintf "%a\n" term t;
  Format.eprintf "----------------------------------------\n@?";;

