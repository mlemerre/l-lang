(* Copyright 2013 Matthieu Lemerre *)

module type BASE_PARSER = sig
  type t
  val int_handler: int -> Token.Stream.t -> t
  val ident_handler: Token.ident -> Token.Stream.t -> t
end;;

module Make = functor(Base_parser:BASE_PARSER) ->
struct

  open Token.With_info;;

  (* Binding powers types, table, and accessor. *)
  type left_binding_power = int;;
  type right_binding_power = int;;

  type left_binding_power_provider = (Token.With_info.t -> left_binding_power);;
  let left_binding_powers:(Token.keyword, left_binding_power_provider) Hashtbl.t 
      = Hashtbl.create 17;;
  let left_binding_power token =
    match token.token with
      | Token.Keyword(kwd) ->
        (try let f = Hashtbl.find left_binding_powers kwd in
             f token
         with Not_found -> 0)
      | _ -> 0


  (* Prefix handlers, types, table, and accessor. *)
  type prefix_handler = Token.Stream.t -> Base_parser.t;;
  let prefix_handlers:(Token.keyword,prefix_handler) Hashtbl.t =
    Hashtbl.create 17;;

  let prefix_handler {token} stream =
    let raise_error token = Log.Parser.raise_user_error 
      "`%s' cannot be used in prefix position." (Token.to_string token)
    in match token with
      | Token.Keyword(kwd) ->
	(try ((Hashtbl.find prefix_handlers kwd) stream)
	 with Not_found -> raise_error token)
      | Token.Int(n) -> Base_parser.int_handler n stream
      | Token.Ident(id) -> Base_parser.ident_handler id stream
      | Token.Float(n) -> failwith "Floats not yet handled"
      | Token.String(s) -> failwith "Strings not yet handled"
      | Token.End -> raise_error token


  (* Infix handlers, types, table, and accessor. *)
  type infix_handler = (Token.Stream.t -> Base_parser.t -> Base_parser.t);;
  let infix_handlers:(Token.keyword,infix_handler) Hashtbl.t =
    Hashtbl.create 17;;
  let infix_handler token left = 
    let fail() =
      Log.Parser.raise_user_error "Token `%s' cannot be used as infix"
        (Token.to_string token.token) in
    match token.token with
      | Token.Keyword(kwd) ->
	(try ((Hashtbl.find infix_handlers kwd) left)
	 (* This error cases can only happen only if a token has a
	    non-zero lbp, but its infix handler is not defined.  *)
	 with Not_found ->  fail())
      | _ -> fail()
         ;;

    
  (* Parse an expression with a right binding power of [rbp]: all
     infix tokens with a left binding power strictly greater than
     [rbp] are part of the \emph{current} call to [parse]; other
     tokens are not. 

     To understand how this works, we put here the stack of calls
     to parse on the "-a+b*c+d" example. We write [parse_i] for the
     ith invocation of [parse]. We write $E_i$ or $(E)_i$ when the
     expression is created by $i$.

     \includegraphics{tdop_example.png}

     \begin{itemize}

     \item The first invocation calls [parse_1] with a right binding
     power of 0, with the expression "-a+b*c+d" (1st invocation of
     [parse] in the stack).

     \item [parse_1] finds "-" in prefix position. The function
     associated to "-" calls [parse_2 3 "a+b*c+d"]

     \item [parse_2] finds "a" in prefix position. The left binding
     power of "+" is 1 < 3, so [parse_2] returns $a_2$.
     The "-" handler returns $(- a_2)_-$

     \item The left binding power of "+" is 1 > 0, so [parse_1]
     continues with "+" in infix position, passing $(- a_2)_-$ to the
     "+" handler. The "+" handler then calls [parse_3 1 "b*c+d"]

     \item [parse_3] finds "b" in prefix position. The left binding
     power of "*" is 2 > 1, so [parse_3] continues with "*"
     in infix position, passing it $b_3$. "*" calls [parse_4 2
     "c+d"]

     \item [parse_4] finds "c" in prefix position. The left binding
     power of "+" is 1 < 2, so [parse_4] returns $c_4$. The
     "*" handler returns with $(b_3 * c_4)_*$. 

     \item The left binding power of "+" is 1 = 1, so
     [parse_3] returns $(b_3 * c_4)_{*,3}$. The "+" handler returns
     with $((- a_2)_- + (b_3 * c_4)_{*,3})_{+}$.

     \item The left binding power of "+" is 1 > 0, so [parse_1]
     continues with "+" in infix position, passing it $((- a_2)_- +
     (b_3 * c_4)_{*,3})_{+}$. "+" calls [parse_5 1 "d"]

     \item [parse_5] finds "d" in prefix position. The left binding
     power of "eof" is 0 = 0, so [parse_5] returns $d_5$. The "+"
     handler returns with $(((- a_2)_- + (b_3 * c_4)_{*,3})_{+} +
     d_5)_{+}$

     \item The left binding power of "eof" is 0 = 0, so [parse_1]
     returns $(((- a_2)_- + (b_3 * c_4)_{*,3})_{+} + d_5)_{+,1}$
     \end{itemize} *)
  let parse stream rbp = 

    (* Get the first token of the expression, and handles it as a prefix
       (i.e. as the beginning of an expression). *)
    let token = Token.Stream.peek stream in
    let left = prefix_handler token stream in
       
    (* Parses tokens while we find tokens with left binding power
       strictly greater than [rbp]. Tail-recursive. *)
    let rec loop left =
      let token = Token.Stream.peek stream in
      if (left_binding_power token) > rbp
      then let handler = infix_handler token in
           let new_left = handler stream left in
           loop new_left
      else left
    in loop left;;

  (* Defines a new prefix operator: when [token] is encountered, the
     parse function [f] is called. *)
  let define_prefix token f =
    match token with
    | Token.Keyword(kwd) ->
      (if Hashtbl.mem prefix_handlers kwd
      then Log.Parser.warning "Redefinition of prefix symbol `%s'" 
          (Token.to_string token));
      Hashtbl.replace prefix_handlers kwd f
    | _ -> assert false
  ;;

  (* General case "infix": a parser is called when the infix [token]
     is encountered. *)
  let define_infix token binding_power parse_fun =
    match token with
    | Token.Keyword(kwd) ->
      (if Hashtbl.mem infix_handlers kwd
      then Log.Parser.warning "Redefinition of infix symbol `%s'" 
          (Token.to_string token));
      Hashtbl.replace infix_handlers kwd parse_fun;
      Hashtbl.replace left_binding_powers kwd binding_power
    | _ -> assert false
  ;;

  (* Common case infix: the right part is another expression, and the
     infix operator is associative to the left. *)
  let define_infix_left_associative string binding_powerf sem_act =
    let left_associative_infix stream left =
      let token = Token.Stream.next stream in
      let right = parse stream (binding_powerf token) in
      sem_act token ~left ~right
    in define_infix string binding_powerf left_associative_infix;;

  (* Common case infix: the right part is another expression, and the
     infix operator is associative to the right. *)
  let define_infix_right_associative string binding_powerf sem_act =
    let right_associative_infix stream left =
      let token = Token.Stream.next stream in
      let right = parse stream ((binding_powerf token) - 1) in
      sem_act token ~left ~right
    in define_infix string binding_powerf right_associative_infix

end;;


