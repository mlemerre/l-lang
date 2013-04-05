(*s Copyright 2012 Matthieu Lemerre *)

(* We just make the main CPS definitions more accessible. *)

module Base:module type of Cpspack.Base;;

type term = Base.term;;
type definition = Base.definition;;
type var = Base.var;;
type cont_var = Base.cont_var;;

module Var_Map: Map.S with type 'a t = 'a Base.Var.Var.Map.t
                      and type key = var;;

module Print:module type of Base.Print;;
