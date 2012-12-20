include Cpsdef;;

module Print = Cpsprint;;
module Change = Cpschange;;
module Build = Cpsbuild;;
module Check = Cpscheck;;
module Traverse = Cpstraverse;;

module type VAR = sig
  type var
  type occur
  module Var: sig
    type occurrence_number =
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences
    val occurrence_number: var -> occurrence_number
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a
    val binding_site: var -> enclosing
    val to_string : var -> string
    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur: sig
    val binding_variable : occur -> var
    val to_string : occur -> string
    module Map : Map.S with type key = occur
    module Set : Set.S with type elt = occur
  end
end
