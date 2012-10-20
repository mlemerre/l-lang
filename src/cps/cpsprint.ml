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
open Cpsbase;;

let rec var ppf v = fprintf ppf "%s" (Var.Var.to_string v)
and occur ppf v = fprintf ppf "%s" (Var.Occur.to_string v)
and cont_var ppf v = fprintf ppf "%s" (Cont_var.Var.to_string v)
and cont_occur ppf v = fprintf ppf "%s" (Cont_var.Occur.to_string v)

and term ppf t = match t.Cpsbase.term with
  | Halt v ->
      fprintf ppf "halt %a" occur v
  | Apply(callerv,k,calleev) ->
      fprintf ppf "%a( %a, %a)"
        occur callerv cont_occur k occur calleev
  | Apply_cont(k,v) ->
      fprintf ppf "%a( %a)"
        cont_occur k occur v
  | Let_cont(k,v,loccont,incont) ->
      fprintf ppf "%a where\n%a %a:{\n%a}"
        term incont cont_var k var v term loccont
  | Let_prim(v,p,body) ->
      fprintf ppf "let %a = %a in\n%a"
        var v prim p term body

and prim ppf = function
  | Value v -> value ppf v
  | Projection(x,i) ->
      fprintf ppf "#%d( %a)"
        i occur x
  | Integer_binary_op(op,v1,v2) ->
      fprintf ppf "%s( %a, %a)"
        (Constant.integer_binary_op_to_string op) occur v1 occur v2

and value ppf = function
  | Void ->
      fprintf ppf "void"
  | Constant(c) ->
      fprintf ppf "%s" (Constant.to_string c)
  | Tuple(l) ->
      fprintf ppf "( %a)"
        (fun ppf -> Utils.print_list_ppf ppf ", " occur) l
  | Lambda(k,v,term_) ->
      fprintf ppf "\n{ %a -> %a ->\n%a}"
        cont_var k var v term term_
