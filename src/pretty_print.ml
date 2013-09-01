(* Copyright 2013 Matthieu Lemerre *)

(* Pretty print L code in various output formats. 
   To use, type ./src/main.run --pretty file.l
*)

(* This is an HTML printer.  *)
module Print = struct
  let newline() = print_string "<br />";;
  let space() = print_string "&nbsp;";;
  let token token = 
    let string = Token.to_string token in
    let print_in_span class_ = 
      print_string "<span class=\"highlight ";
      print_string class_;
      print_string "\">";
      print_string string;
      print_string "</span>"
    in
    match token with
    | Token.Int(_) -> print_in_span "mi"
    | Token.String(_) -> print_in_span "s"
    | Token.Keyword(_) -> 
      let class_ =
        if token = Token.Keywords.let_ then "kd"
        else if token = Token.Keywords.def then "kd"
        else if token = Token.Keywords.declare then "kd"
        else "kr" in
      print_in_span class_
          
end


(* This is an ASCII printer. *)
module PrintAscii = struct
  let newline() = print_string "\n";;
  let space() = print_string " ";;
  let token token = 
    let string = Token.to_string token in
    print_string string;;

end

(* TODO: Add a hook in the parser to provide informations such as "is
   it allowed to cut after/before this symbol", and use spans to
   highlight the parsing algorithm. *)

(* TODO: Make it a functor. *)
let pretty file = 
  let stream = Token.Stream.make file in
  let tok = ref (Token.Stream.next stream) in
  let cur_coords = ref (0,0) in
  
  let position_to_coords pos =
    let open Lexing in 
    (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
  in
  (* Print \n and/or spaces until cur_coords is reached. *)
  (* Note: this is ugly, and cause comments to be forgotten. I should
     have a "next_with_spacing" in the lexer instead, that would also
     return the contents of the blank before the token. *)
  let update_coordinates (cur_line, cur_col) pos = 
    let (line,col) = position_to_coords pos in
      assert ((cur_line < line) || (cur_line == line && cur_col <= col));

    let cur_line = ref cur_line and cur_col = ref cur_col in
    while !cur_line != line do
      Print.newline();
      incr cur_line;
      cur_col := 0;
    done;
    while !cur_col != col do
      Print.space();
      incr cur_col
    done;
    (line,col) 
  in
  let incr_coordinates (cur_line, cur_col) string = 
    (cur_line, cur_col + String.length string) 
  in

  while (!tok).Token.With_info.token != Token.End do
    let (start_pos,_) = (!tok).Token.With_info.location in
    cur_coords := update_coordinates !cur_coords start_pos;
    let token = (!tok).Token.With_info.token in
    Print.token token;
    let string = (Token.to_string token) in
    cur_coords := incr_coordinates !cur_coords string;
    tok := Token.Stream.next stream
  done

