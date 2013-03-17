include Cpsbasepack.Cpsdef;;

module Print = Cpsbasepack.Cpsprint;;
module Change = Cpsbasepack.Cpschange;;
module Build = Cpsbasepack.Cpsbuild;;
module Check = Cpsbasepack.Cpscheck;;
module Traverse = Cpsbasepack.Cpstraverse;;

module type VAR = sig
  type var
  type occur_maker
  type occur
  module Var: sig
    type number_of_occurrences =
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences
    val number_of_occurrences: var -> number_of_occurrences
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a
    val binding_site: var -> enclosing
    val to_string : var -> string
    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur: sig
    type maker = occur_maker
    val maker: var -> maker
    val rec_maker: var -> maker
    val binding_variable : occur -> var
    val to_string : occur -> string
    module Map : Map.S with type key = occur
    module Set : Set.S with type elt = occur
  end
end
