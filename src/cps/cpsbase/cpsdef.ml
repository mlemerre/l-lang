(* Copyright 2012 Matthieu Lemerre.  *)
(* Cpsdef: definitions of data types for CPS representation. *)

module CaseMap = Extensions.Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end);;

type 'a case_map = 'a CaseMap.t;;

(* \subsection*{Variables} *)

(*s The definition or variables and occurrences originate from
  [Cpsvar]. Note that [Cpsvar] makes variables and occurrences
  unique, avoiding any need for alpha conversion. *)

type var = (var_desc,occur_desc) Cpsvar.variable
and occur =  (var_desc,occur_desc) Cpsvar.occurrence
and cont_var = (cont_var_desc,cont_occur_desc) Cpsvar.variable
and cont_occur =  (cont_var_desc,cont_occur_desc) Cpsvar.occurrence

(* \subsection*{Expressions} *)

and expression_ =
  | Let_prim of var * primitive *  expression
  | Let_cont of cont_var * var * expression * expression
  | Apply_cont of cont_occur * occur
  | Apply of function_type * occur * cont_occur * occur list
  | Case of occur * cont_occur case_map * cont_occur option
  | Halt of occur

and primitive =
  | Value of value
  | Projection of int * occur
  | Integer_binary_operation of Constant.Ibop.t * occur * occur
  | Integer_binary_predicate of Constant.Ibpred.t * occur * occur

(*i Note: it is unclear now whether it is interesting to put these
  values together in a value data type, or split them directly in the
  "primitive" data structure. It depends on whether we can take
  advantage of the definition of "value", i.e. values need no further
  computations; but this is not truly the case for tuples and
  lambdas, which may require dynamic allocation. i*)
and value =
  | Constant of Constant.t
  | Tuple of occur list
  | Injection of int * int * occur
  | Lambda of function_type * cont_var * var list *  expression
  | External of string

and function_type =
  | Closure
  | No_environment

(* \subsection*{Definitions} *)

(*i Note: to avoid redundancies, definitions should look like
  let-bindings, so that they can be handled similarly. i*)

and definition = Definition of visibility * definition_type
and visibility = Public of var | Private of var | Unused
and definition_type =
  | Static_value of value
  | Dynamic_value of expression
and definitions = definition list

(* \subsection*{Backlinks and mutability for efficient operations} *)

(*s The [expression_] type defines the logical structure of an
  expression, and we insert this structure between any two
  [expression_]s, or between a [expression_] and its [definition]. It
  implements a mutable double-link between an expression and its
  parent, (which can be an expression or a top-level definition).

  Note that an expression has only one parent, and thus can appear
  only once in a tree. *)
and expression =
  { mutable enclosing:enclosing;
    mutable expression:expression_ option }

and enclosing =
  | Enclosing_definition of definition
  | Enclosing_expression of expression
  | Enclosing_uninitialized

(*s For now we have only implemented uplinks from variables to their
  binding sites. We should hade uplinks from occurrences to the site
  that use them, and do the same for continuation variables.  *)
and var_desc = { mutable binding_site_var:enclosing; }
and occur_desc = unit
and cont_var_desc = unit
and cont_occur_desc = unit


(* \subsection*{Instantiations of [Cpsvar]} *)

type occur_type = Cpsvar.occur_type = Recursive | Non_recursive

module type VAR_RW = sig
  type var
  type occur_maker
  type occur
  module Var: sig
    val make : unit -> var
    val init: var -> enclosing -> unit
    type number_of_occurrences =
      | No_occurrence
      | One_occurrence of occur
      | Several_occurrences
    val number_of_occurrences: var -> number_of_occurrences
    val fold_on_occurrences: var -> 'a -> ('a -> occur -> 'a) -> 'a
    val replace_with: var -> var -> unit
    val replace_all_non_recursive_occurrences_of_with: var -> var -> unit
    val to_string : var -> string
    val binding_site: var -> enclosing
    val set_binding_site: var -> enclosing -> unit
    module Map : Map.S with type key = var
    module Set : Set.S with type elt = var
  end

  module Occur: sig
    type maker = var * occur_type
    val maker: var -> maker
    val rec_maker: var -> maker
    val make : maker -> occur
    val delete: occur -> unit
    val binding_variable : occur -> var
    val to_string : occur -> string
    module Map : Map.S with type key = occur
    module Set : Set.S with type elt = occur
  end
end


module Var_ = Cpsvar.Make(struct
  type variable_description = var_desc
  type occurrence_description = occur_desc
  let var_prefix = "x"
end);;

module Cont_var_ = Cpsvar.Make(struct
  type variable_description = unit
  type occurrence_description = unit
  let var_prefix = "k"
end);;

module Var = struct
  type var = Var_.var
  type occur_maker = var * occur_type;;
  type occur = Var_.occur
  module Var = struct
    include Var_.Var
    let binding_site v =
      let desc = Var_.Var.description v in
      desc.binding_site_var;;

    let init v e =
      Var_.Var.set_description v { binding_site_var=e};;

    (* Note that [set_binding_site] requies that
       [Var_.Var.description] exists, i.e. that [init] was called. *)
    let set_binding_site v e =
      let desc = Var_.Var.description v in
      desc.binding_site_var <- e;;

  end
  module Occur = Var_.Occur
end

type occur_maker = Var.Occur.maker;;

module Cont_var = struct
  type var = Cont_var_.var
  type occur_maker = var * occur_type
  type occur = Cont_var_.occur
  module Var = struct
    include Cont_var_.Var
    let binding_site v
        = failwith "binding_site for continuation variables not implemented";;

    let set_binding_site v e
        = failwith "binding_site for continuation variables not implemented";;

    let init v e = ();;

  end
  module Occur = Cont_var_.Occur
end

type cont_occur_maker = Cont_var.Occur.maker;;

(* \subsection*{The [Expression], [Empty], and [Fresh] modules} *)

(* Definitions common to [Expression], [Fresh], et [Empty]. *)
module Common = struct
  let get t = match t.expression with Some(t_) -> t_ | None -> assert false;;
  let set t t_ = t.expression <- Some t_;;
  let empty t = t.expression <- None;;
  let is_empty t = t.expression == None;;

  let enclosing t = t.enclosing;;
  let set_enclosing t e = t.enclosing <- e;;
  let delete_enclosing t = t.enclosing <- Enclosing_uninitialized;;
  let is_fresh t = t.enclosing == Enclosing_uninitialized;;
end;;


module Empty = struct

  type t = expression

  let is_empty = Common.is_empty;;
  let empty expression =
    assert( not( is_empty expression));
    Common.empty expression;
    expression;;

  let set t t_ =
    assert( is_empty t);
    Common.set t t_;;
  let fill t t_ = set t t_; t;;
end

module Fresh = struct

  type t = expression

  let make t_ = { expression = Some t_;
                  enclosing = Enclosing_uninitialized }
  let is_fresh = Common.is_fresh;;

  let get t = assert( is_fresh t); Common.get t

  let set_enclosing t e = assert( is_fresh t); Common.set_enclosing t e;
end

module Expression = struct
  let get = Common.get
  let set = Common.set

  let enclosing = Common.enclosing;;
  let set_enclosing = Common.set_enclosing;;

  let make ?reconnect t_ = match reconnect with
    | None -> Fresh.make t_
    | Some(t) -> Empty.fill t t_
  ;;

  let discard expression =
    Common.delete_enclosing expression;
    Common.empty expression
  ;;

end
