(* Copyright 2013 Matthieu Lemerre *)

(* The usual way to intern the string would be to have a [(string,
   unit) Hashtbl.t], add the string if not already present, and return
   the string that was added first otherwise. In addition to saving
   nmemory, this allows to test equality of strings in constant time
   (using the [==] operator).

   Instead, we return a unique int associated to the string; the the
   string can be retrieved from the int using the "inverted" hash
   table. The int further allow fast comparison and hashing, which is
   more efficient when using the string as the key to a Map or Hashtbl
   data structure. *)

module Make(S:sig end) = struct

  module Id = Unique.Make(S)
  include Id;;

  let table:(string,t) Hashtbl.t = Hashtbl.create 31;;
  let inverse_table:(t,string) Hashtbl.t = Hashtbl.create 31;;
  let intern string =
    try Hashtbl.find table string 
    with Not_found -> 
      let num = Id.fresh() in
      Hashtbl.replace table string num;
      Hashtbl.replace inverse_table num string;
      num;;
  let to_string id = Hashtbl.find inverse_table id
end

