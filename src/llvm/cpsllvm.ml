(* Copyright 2012-2013 Matthieu Lemerre. *)

module List = Extensions.List;;         (* For List.fold_left_with_index. *)

(*i \section{Module Cpsllvm} i*)

(*s This module translates a term written in CPS representation to
  LLVM instructions in SSA form.

  The CPS representation stems from the paper "Compiling with
  continuations, continued" by Andrew Kennedy. In particular this
  representation separates continuations from standard lambda
  functions, which allows calling and returning from functions using
  the normal stack, and allow close correspondance with the SSA form.

  This module assumes that functions have no free variables (or
  continuation variables). Closure conversion removes free variables
  from functions. Free continuation variables should never happen
  when translating normal terms to CPS.

  The module also assumes that the CPS values do not refer to
  primitive operations, such as +,-,*,/. Previous passes must
  transform calls to primitive operations to $let x =
  primitive(args)$; and $\eta$-expand primitive operations passed as
  functions (e.g. $let x = f(\+)$ must have been transformed).

  To keep things simple in this first version, no external functions
  is called (only lambdas defined in the body of the expression, and
  primitive operations, can be called).

  In addition, all data is boxed, allocated using malloc (and never
  freed; this could be improved by using libgc). Unboxed data would
  requires to carry typing information in the CPS terms. *)

(*s To get an overview of the translation algorithm, the best is to
  understand how the CPS concepts are mapped to the SSA concepts. In
  the following, we denote by $[x]$ the translation of $x$.

  \begin{itemize}

  \item Lambda are translated to LLVM functions with one argument and
  one return value.

  \item Other values (i.e. int, floats, and tuples) are all translated
  boxed. Thus they all have a single llvm type, which is i8 *.

  \item A local CPS variable $x$ is mapped to a SSA variables (of type
  [Llvm.llvalue]). CPS variables are introduced as arguments to lambda
  and continuations, and in the $let x = ... $ form. CPS variables and
  SSA variables have the same name in their respective printed
  representation.

  \item A CPS continuation variable $k$ introduced by $\lambda k. x. t$
  corresponds to the return from the lambda. A call $k(y)$ to this
  continuation with a value $y$ is translated to a "ret" instruction
  returning the translation of $y$.

  \item A CPS continuation variable $k$ introduced by $let k(x) = t_1;
  t_2$ is mapped to the SSA basic block $[t1]$ (of type
  [Llvm.basicblock]). The $x$ formal argument of $k$ corresponds to a
  phi node at the start of $[t1]$. A call $k( y$ to this continuation
  with a value $y$ is translated to a "jmp" instruction to the basic
  block $[t1]$, that binds $[y]$ to the phi node at the start of
  [[t1]].

  \item A call $f( k, x)$ of a regular (non-continuation) function $f$
  with first argument being a continuation variable argument $k$ and
  second argument being a variable $v$ is translated to a call to
  $[f]$ with argument $[x]$, followed by the translation of $k( r)$,
  with $r$ being the value returned by the call to $f$. This is because
  after calling a function in the LLVM SA, the control is returned to
  the following instruction. LLVM optimization passes like simplifycfg
  can optimize this if needed. Note: this allows tail call
  optimizations
  \url{http://llvm.org/docs/CodeGenerator.html#tail-calls} to take
  place.

  \item Primitive operations, such as $let x = primitive(args)$
  are translated to the corresponding LLVM operations.

  \item A global CPS variables $x$ is mapped to a SSA variable, but
  may have additional indirection. If $x$ is defined as a
  [Dynamic_value(term)], its size cannot be statically computed; so we
  allocate the space for a global variable [s] that contains one
  pointer, and compile [term] as a constructor that stores the
  dynamically allocated result of initialization, in [s]. Accesses to
  [x] are transformed to dereferences to [s]. A future
  "staticalization" transformation will try to maximize the amount of
  static values, to avoid this indirection.

  \end{itemize}

  Note that the SSA representation are well-formed only if "the
  definition of a variable \verb|%x| does not dominate all of its uses"
  (\url{http://llvm.org/docs/LangRef.html#introduction}). The translation
  from a CPS term (without free variables) ensures that. *)

(*s Here is a simplified example of how the translation from CPS to
  SSA works.

  The CPS code:
  \begin{verbatim}
  let v = 3;
  let k(x) = k(2+x);
  k(11)  \end{verbatim}

  Is translated to SSA (ignoring boxing):
  \begin{verbatim}
  entry:
    v = 3
    n_ = 11
    jmp k

  k:
    x = phi (entry n_) (k o_)
    m_ = 2
    o_ = m_ + x
    jmp k \end{verbatim}

  This shows how $k$ is translated to a separate basic block, and the
  argument $x$ to a phi node connected to all the uses of $k$.
*)

(*s If one encounters segmentation faults when changing the LLVM
  related  code, this may be caused by:

  \begin{itemize}

  \item Calling [Llvm.build_call] on a value which does not have the
  function [lltype], or [Llvm.build_gep] with operations that do not
  correspond to the lltype of the value.

  \item Calling [build_phi] with an empty list of "incoming".

  \item Calling [ExecutionEngine.create the_module] before calling
  [Llvm_executionengine.initialize_native_target()] can also segfault.

  \end{itemize}

  Using valgrind or gdb allows to quickly locate the problematic Ocaml
  Llvm binding.
*)

let context = Llvm.global_context();;

(* We extend the existing, "environment" module. This allows access to
   existing variables. *)
let the_module = Llvmenv.the_module;;

let void_type = Llvm.void_type context;;
let i32_type = Llvm.i32_type context;;
let i32star_type = Llvm.pointer_type i32_type;;
let anystar_type = Llvm.pointer_type (Llvm.i8_type context);;

let undef_anystar = Llvm.undef anystar_type;;
let null_anystar = Llvm.const_null anystar_type;;

(* Note: Base will be (in the future) Cps.Base. *)
module Base = Cpsbase;;
module Var_Map = Base.Var.Var.Map;;
module Cont_var_Map = Base.Cont_var.Var.Map;;
open Base;;

(* \subsection*{Creating and accessing memory objects}  *)

(*s These helper functions create or read-from memory object.
  Currently LLVM compiles using a very simple strategy: every value is
  boxed (including integers and floats). This simplifies compilation a
  lot: every value we create has type void *, and we cast the type
  from void * according to how we use it.

  LLVM does not (yet?) know how to replace heap allocations with stack
  allocations, so we should do that (using an escape analysis). But
  LLVM has passes that allow promotion of stack allocations to
  register ("mem2reg" and "scalarrepl"), so once this is done (plus
  passing and returning arguments in registers), many values should be
  unboxed by the compiler (and this would not be that inefficient).
  Additional performances could then be obtained by monomorphizing the
  code. *)

(*s Store [llvalue] in heap-allocated memory. *)
let build_box name llvalue builder =
  let lltype = Llvm.type_of llvalue in
  let pointer = Llvm.build_malloc lltype (name ^ "_uncasted") builder in
  ignore(Llvm.build_store llvalue pointer builder);
  Llvm.build_bitcast pointer anystar_type name builder;;

(*s Unbox a [llvalue] of type [lltype].  *)
let build_unbox name llvalue lltype builder =
  let typeptr = Llvm.pointer_type lltype in
  let castedptr = Llvm.build_bitcast llvalue typeptr (name ^ "_casted") builder in
  Llvm.build_load castedptr name builder;;

(*s A n-tuple is allocated as an array of n [anystar_type]. Each
   element of the array contains the llvalue in l. *)
let build_tuple name l builder =
  let length = List.length l in
  let array_type = Llvm.array_type anystar_type length in
  let pointer = Llvm.build_malloc array_type (name ^ "_tuple") builder in

  let f elem int =
    (* Note: the first 0 is because pointer is not the start of
       the array, but a pointer to the start of the array, that
       must thus be dereferenced. *)
    let path = [| (Llvm.const_int i32_type 0); (Llvm.const_int i32_type int) |] in
    let gep_ptr = Llvm.build_gep pointer path (name ^ "_tuple_" ^ (string_of_int int)) builder in
    ignore(Llvm.build_store elem gep_ptr builder) in

  List.iter_with_index f l;
  Llvm.build_bitcast pointer anystar_type name builder;;

(*s Retrieve an element from a tuple.  *)
let build_letproj name pointer i builder =
  (* First we compute an acceptable LLvm type, and cast the pointer to
     that type (failure to do that makes [Llvm.build_gep] segfault).
     As we try to access the ith element, we assume we are accessing
     an array of size i+1. *)
  let array_type = Llvm.array_type anystar_type (i+1) in
  let arraystar_type = Llvm.pointer_type array_type in
  let cast_pointer = Llvm.build_bitcast pointer arraystar_type (name ^ "_casted") builder in
  let gep_ptr = Llvm.build_gep cast_pointer [| (Llvm.const_int i32_type 0);
                                               (Llvm.const_int i32_type i) |]
    (name ^ "_gep" ^ (string_of_int i)) builder in
  let result = Llvm.build_load gep_ptr name builder in
  result ;;


(*s Create variants and retrieve elements from a variant. *)
module Variant:sig
  open Llvm
  val build:string -> llvalue -> llvalue -> llbuilder -> llvalue
  val bind: string -> llvalue -> llbuilder -> (llvalue * llvalue)
end = struct
  let variant_type = Llvm.struct_type context [| i32_type; anystar_type |];;
  let variant_star_type = Llvm.pointer_type variant_type;;

  let get_variant_ptrs name ptr builder =
    let cast_pointer = Llvm.build_bitcast ptr variant_star_type (name ^ "_casted") builder in
    let ptr_to_tag = Llvm.build_gep cast_pointer [| (Llvm.const_int i32_type 0);
                                                    (Llvm.const_int i32_type 0) |]
      (name ^ "_tag_ptr") builder in
    let tag = Llvm.build_load ptr_to_tag (name ^ "_tag") builder in
    let ptr_to_value = Llvm.build_gep cast_pointer [| (Llvm.const_int i32_type 0);
                                                    (Llvm.const_int i32_type 1) |]
      (name ^ "_value_ptr") builder in
    (ptr_to_tag,ptr_to_value);;

  (*c Note: tag is a i32bit unboxed llvalue. value is a anystar_type llvalue. *)
  let build name tag value builder =
    let ptr = Llvm.build_malloc variant_type (name ^ "_variant") builder in
    let (ptr_to_tag, ptr_to_value) = get_variant_ptrs name ptr builder in
    ignore(Llvm.build_store tag ptr_to_tag builder);
    ignore(Llvm.build_store value ptr_to_value builder);
    Llvm.build_bitcast ptr anystar_type name builder;;

  let bind name ptr builder =
    let (ptr_to_tag, ptr_to_value) = get_variant_ptrs name ptr builder in
    let tag = Llvm.build_load ptr_to_tag (name ^ "_tag") builder in
    let value = Llvm.build_load ptr_to_value (name ^ "_value") builder in
    (tag,value) ;;

end;;

(*s Create a boolean. For now, boolean are stored as regular variants;
  as they have no associated information beside the tag, the
  associated value is just undef. *)
let build_boolean name value builder =
  let ext_value = Llvm.build_zext_or_bitcast value i32_type (name ^ "_icmp_ext") builder in
  Variant.build name ext_value undef_anystar builder;;

(*s Apply primitive operations.  *)
let build_integer_binary_op name op a b builder =
  let build_fn = match op with
    | Constant.Ibop.Add -> Llvm.build_add
    | Constant.Ibop.Sub -> Llvm.build_sub
    | Constant.Ibop.Mul -> Llvm.build_mul
    | Constant.Ibop.Div -> Llvm.build_udiv in
  let a_unbox = (build_unbox (name ^ "_a") a i32_type builder) in
  let b_unbox = (build_unbox (name ^ "_b") b i32_type builder) in
  let res = build_fn a_unbox b_unbox (name ^ "_bop") builder in
  build_box name res builder;;

let build_integer_comparison name op a b builder =
  let llvm_pred = match op with
    | Constant.Ibpred.Eq -> Llvm.Icmp.Eq
    | Constant.Ibpred.Ne -> Llvm.Icmp.Ne
    | Constant.Ibpred.Ugt -> Llvm.Icmp.Ugt
    | Constant.Ibpred.Uge -> Llvm.Icmp.Uge
    | Constant.Ibpred.Ult -> Llvm.Icmp.Ult
    | Constant.Ibpred.Ule -> Llvm.Icmp.Ule
    | Constant.Ibpred.Sgt -> Llvm.Icmp.Sgt
    | Constant.Ibpred.Sge -> Llvm.Icmp.Sge
    | Constant.Ibpred.Slt -> Llvm.Icmp.Slt
    | Constant.Ibpred.Sle -> Llvm.Icmp.Sle in
  let a_unbox = (build_unbox (name ^ "_a") a i32_type builder) in
  let b_unbox = (build_unbox (name ^ "_b") b i32_type builder) in
  let res = Llvm.build_icmp llvm_pred a_unbox b_unbox (name ^ "_icmp") builder in
  build_boolean (name ^ "_boolean") res builder;;

(*s Build a call instruction, casting [caller] to a function pointer. *)
let build_call name caller callees builder =
  let function_type = Llvm.pointer_type (Llvm.function_type anystar_type [| anystar_type; anystar_type |]) in
  let casted_caller = Llvm.build_bitcast caller function_type (name ^ "_function") builder in
  let retval = Llvm.build_call casted_caller (Array.of_list callees) (name ^"_result") builder in
  retval;;

(* \subsection*{Creating and accessing basic blocks}  *)

(*s This special value is used to ensure, via the type checker, that
   compilation to LLVM never leaves a basic-block halfly built. LLVM
   basic blocks should all end with a terminator instruction; whenever
   one is inserted, the function should return [End_of_block]. When
   building non-terminator instructions, the code must continue
   building the basic block. *)
type termination = End_of_block;;

(*s This creates a new basic block in the current function.

   Note that LLVM basic blocks are associated to a parent function,
   that we need to retrieve to create a new basic block. *)
let new_block name builder =
  let current_bb = Llvm.insertion_block builder in
  let the_function = Llvm.block_parent current_bb in
  let new_bb = Llvm.append_block context name the_function in
  new_bb;;

(*s Returns [Some(phi)] if the block already begins with a phi instruction,
   or [None] otherwise. *)
let begin_with_phi_node basic_block =
  let pos = Llvm.instr_begin basic_block in
  match pos with
    | Llvm.At_end(_) -> None
    | Llvm.Before(inst) ->
      (match Llvm.instr_opcode inst with
        | Llvm.Opcode.PHI -> Some(inst)
        | _ -> None);;

(*s Create a new block with instructions set by f() in this new block,
  and return it; then one can continue building instruction at the
  current place. *)
let with_new_block name builder f =
  let current_bb = Llvm.insertion_block builder in
  let new_bb = new_block name builder in
  Llvm.position_at_end new_bb builder;
  f();
  Llvm.position_at_end current_bb builder;
  new_bb

(* Create a new, unreachable block, and return it. *)
let new_unreachable_block builder =
  with_new_block "unreachable" builder (fun () -> Llvm.build_unreachable builder);;

(*i   XXX: To handle conditional branch in the following, we will use a "condition"
   parameter? But this won't end the block anymore.  i*)

(*s This builds a jmp instruction to [destination_block], also passing
  the [v] value. This is achieved by setting [v] as an incoming value
  for the phi instruction that begins [destination_block]. If
  [destination_block] does not start with a phi node, then it is the
  first time that [destination_block] is called, and we create this
  phi node. *)
let build_jmp_to_and_add_incoming destination_block v builder =

  let add_incoming_to_block basic_block (value,curblock) =
    match begin_with_phi_node basic_block with
      | Some(phi) -> Llvm.add_incoming (value,curblock) phi
      | None ->
        (* Temporarily create a builder to build the phi instruction. *)
        let builder = Llvm.builder_at context (Llvm.instr_begin basic_block) in
        ignore(Llvm.build_phi [value,curblock] "phi" builder) in

  let current_basic_block = Llvm.insertion_block builder in
  add_incoming_to_block destination_block (v, current_basic_block);

  ignore(Llvm.build_br destination_block builder);
  End_of_block;;

(*s We use the following sum type to establish a distinction between:
   \begin{itemize}

   \item continuation variables bound with lambda: calling them
   returns from the function, and the parameter [x] of the call [k( x)]
   is returned;

   \item and continuation variables bound with letcont: calling them
   jumps to the corresponding basic block, and the parameter [x] of
   the call [k( x)] is passed to the phi node starting this basic
   block.

  \end{itemize}

  The CPS$\to{}$LLVM translation maps continuation variables to
  [dest_type]s. *)
type dest_type =
| Ret
| Jmp_to of Llvm.llbasicblock

(* Corresponds to building a call to a continuation [k x], but [k]
  and [x] are already translated to their corresponding [dest_type]
  and [llvalue]. *)
let build_applycont k x builder =
  match k with
  | Ret -> ignore(Llvm.build_ret x builder); End_of_block
  | Jmp_to(destination) -> build_jmp_to_and_add_incoming destination x builder;;

(* Some LLVM instructions, such as br and switch, takes a label
   argument, while the corresponding CPS construct takes a continuation
   [k]. Even if continuations are translated to a code label, calling a
   continuation also requires to pass an argument [x]. This function
   creates a small basic block that just calls the [k] with the
   argument [x], to be used by such LLVM instructions. *)
(*i Note: If [k] is [Ret], we need to create this block (or reuse it if
   it exists already). But if it is a [Jmp_to], we could just return
   the basic block, and add an incoming to the phi node corresponding
   to the value of [x].

   But it is simpler (and thus done now) to always create the new
   block. i*)
let basic_block_that_calls name k x builder =
  with_new_block name builder (fun () ->
    build_applycont k x builder);;

(* \subsection*{Main CPS term translation} *)

(* It is important for LLVM that function names are unique. *)
module UniqueFunctionId = Unique.Make(struct end);;
let uniquify_name name = name ^ "_uniq" ^ (UniqueFunctionId.to_string (UniqueFunctionId.fresh()));;

(* The environment comprehends [contvarmap], a mapping from local
   continuation variables to dest_type; [globalvarmap], a mapping from
   the global variables to llvalues; [varmap], containing a mapping
   from both the global and local variables to llvalues; and
   [handle_halt], which explains how [Halt] is translated. *)
type env = { contvarmap: dest_type Cont_var_Map.t;
             varmap: Llvm.llvalue Var_Map.t;
             globalvarmap: Llvm.llvalue Var_Map.t;
             handle_halt: handle_halt
           }

(* This type states how a [Halt(x)] CPS term must be translated:
   either we return [x], or we ignore [x] return nothing, or [x] is
   stored in some memory region. *)
and handle_halt =
  | Halt_returns_value
  | Halt_returns_void
  | Halt_stores_results_in of Llvm.llvalue


(*s This function builds the CPS term [cps], in the current block
  pointed to by [builder]. [varmap] maps CPS variables to LLVM
  llvalues. [contvarmap] maps CPS continuation variables to values of
  type [contvar_type].

  All the free variables or continuation variables in [cps] must be
  in [contvarmap] or in [varmap]. [cps] can contain lambda, but they
  must not contain any free variables or free continuation variables
  (even the one in [varmap] and [contvarmap]). Closure conversion
  deals with this. Note: previously-defined global variables are not
  considered free. *)
let rec build_term cps env builder =

  (*s These functions return a llvalue corresponding to the occurrence
    of a variable or continuation variable given as an argument. *)
  let translate_occurrence x =
    let bound_var = Var.Occur.binding_variable x in
    let llvalue =
      try Var_Map.find bound_var env.varmap
      with _ -> failwith ("in translate_var " ^ (Var.Occur.to_string x)) in
    match Var.Var.binding_site bound_var with
      (* Global dynamic values are allocated with an extra level of
         indirection, so we need to unbox them. *)
      | Enclosing_definition(Definition(_,Dynamic_value(_))) ->
        build_unbox (Var.Occur.to_string x) llvalue anystar_type builder
      (* Note: we could directly return constant integer here. It
         seems not worth it, because LLVM should be able to deal
         with them itself. *)
      | _ -> llvalue
  in

  let translate_cont_occurrence k =
    try Cont_var_Map.find (Cont_var.Occur.binding_variable k) env.contvarmap
    with _ -> failwith "in translate_cont_occurrence" in

  let add_to_varmap var value = Var_Map.add var value env.varmap in
  let add_to_contvarmap contvar block = Cont_var_Map.add contvar (Jmp_to block) env.contvarmap in

  (*s Converting the term is done by inductive decomposition. There are
    three kind of cases:
    \begin{itemize}
    \item those that only build new values (letvalue, letproj,
    letprimop...) in the current basic block
    \item those that return a value and end a basic block
    (apply, applycont, and halt)
    \item the one that build a new basic blocks (letcont).
    \end{itemize}

    To keep the implementation simple, all values are boxed (i.e. put
    in the heap and accessed through a pointer), and of llvm type "i8
    *". Pointer conversions are done according to the use of the
    value. *)
  match Expression.get cps with

    (*s For [Let_prim(x,prim,body)] we just build the new llvalue
      corresponding to [prim], map it to [x], then continue building
      [body]. *)
    | Let_prim(x,prim,body) ->
      let xname = (Var.Var.to_string x) in
      let result = (match prim with
        | Value( Constant(Constant.Integer i)) ->
          let llvalue = Llvm.const_int i32_type i in
          build_box (xname ^ "_is_const_" ^ string_of_int i) llvalue  builder

        | Value( Constant(Constant.Float(_) | Constant.String(_))) ->
          failwith "Float and strings not yet implemented"

        | Value( External( id)) ->
          let llvalue = Llvmenv.lookup_global id in
          Llvm.build_bitcast llvalue anystar_type ("external_" ^ id) builder

        (* For now, any value is a pointer, so we compile void to
           pointers; but void values should not be dereferenced, so we
           can just use undef as a pointer. *)
        | Value( Tuple []) ->
          Llvm.undef anystar_type

        | Value( Tuple(l)) ->
          let llvalues = List.map translate_occurrence l in
          build_tuple xname llvalues builder

        | Value( Injection( i,_,value)) ->
          let llvalue = translate_occurrence value in
          Variant.build xname (Llvm.const_int i32_type i) llvalue builder

        (* This build a new function, with private linkage (since
           that it can be used only by the current term), which
           allows llvm optimizations.

           Note that [build_function] will use a new builder, so the
           lambda can be built in parallel with the current
           function. Also it will use new variables and continuation
           variable maps (with only the x parameter), so the lambda
           expression must not contain any free variables. *)
        | Value( Lambda(ft,k,vl,body)) -> assert(ft == No_environment);
          let funname =  ((Var.Var.to_string x) ^ "fun") in

          (* Declare the function, and add it to the map, so as to
             allow recursive use of the function.

             TODO: Extend this to any mutually recursive value
             (Lamdba, Tuples, Injections). This is an argument for
             separating let_prim(x,prim,body) from
             let_values([(x1,value1);...(xn,valuen)],body).

             FIXME: The function is build from [env.globalvarmap]. This
             can cause issues when an inner function recursively calls
             the function in which it is defined. It works currently
             because the closure conversion algorithm pass the outer
             function in the environment, but optimisations will break
             that. One solution would be to lift the lamdba to global
             scope. Another solution would be to pass the current
             varmap, but then cpsllvm will not be able to catch some
             errors. *)
          let the_function = declare_llvm_function funname (List.length vl) true in
          let the_function = Llvm.build_bitcast the_function anystar_type (funname ^ "cast") builder in
          let function_build_map = Var_Map.add x the_function env.globalvarmap in

          let f = build_function funname k vl body function_build_map in
          Llvm.set_linkage Llvm.Linkage.Private f;
          Llvm.build_bitcast f anystar_type xname builder

        (* Primitive ops are handled here. Notice that we handle the
           translation of a call to a primitive operation (e.g.
           +(a,b)), and not the use of a primitive as a function (e.g.
           let a = +). *)
        | Integer_binary_operation(op,xa,xb) ->
          build_integer_binary_op xname op (translate_occurrence xa) (translate_occurrence xb) builder
        | Integer_binary_predicate(pred,xa,xb) ->
          build_integer_comparison xname pred (translate_occurrence xa) (translate_occurrence xb) builder
        | Projection(i,x) -> build_letproj xname (translate_occurrence x) i builder

        (* Expressions such as $let x = primitive$ should have been
           eta-expanded into something like $let x = { (a,b) ->
           primitiveop( a,b) }$ in previous compilation stage, so we
           fail here. *)
        | Value (Constant(c)) -> print_endline (Constant.to_string c);
          failwith "ICE: primitive operations as value in LLVM translation."
      )
      in build_term body {env with varmap=(add_to_varmap x result)} builder

    (*s Building new basic blocks. The algorithm first creates an
      empty basic block, bound to [k], then build [body], then build
      [term] (if [k] is really called), binding [x] to the phi
      node. *)

    (* An unused continuation is translated to a basic block with no
       predecessor, which makes LLVM complain. So we optimize this
       case. *)
    | Let_cont(k,_,_,body) when
        Cont_var.Var.number_of_occurrences k == Cont_var.Var.No_occurrence ->
      build_term body env builder

    (* The general case. The tricky part is that the llvm bindings do
       not allow to create an "empty" phi node (even if it would, in
       future implementations which would not box everything we would
       still have to know the llvm type of the phi node, and that llvm
       type is not known until we have processed the jumps to that
       node). So it is the calls to k that create or change the phi
       node; no phi node means [k] is never called (so we do not
       bother building it).

       Doing the operations in this order ensures that calls to [k] are
       processed before [k] is built. *)
    | Let_cont(k,x,term,body) ->
      let new_bb = new_block (Cont_var.Var.to_string k) builder in
      let newcvm = add_to_contvarmap k new_bb in
      let End_of_block = build_term body {env with contvarmap=newcvm} builder in
      Llvm.position_at_end new_bb builder;
      (match begin_with_phi_node new_bb with
        | None -> End_of_block
        | Some(phi) -> build_term term {env with contvarmap=newcvm; varmap=add_to_varmap x phi} builder)

    (*s Cases that change or create basic blocks. *)
    (* Depending on k, applycont either returns or jumps to k.  *)
    | Apply_cont(k,x) ->
      build_applycont (translate_cont_occurrence k) (translate_occurrence x) builder

    (* The CPS semantics state that caller should return to k, but
       LLVM SSA does not require that calls end basic blocks. So we
       just build a call instruction, and then a call to [k]. LLVM
       optimizations will eliminate the superfluous jump if needed. *)
    | Apply(ft,func,k,args) -> assert(ft == No_environment);
      let retval = build_call
        (Var.Occur.to_string func)
        (translate_occurrence func)
        (List.map translate_occurrence args) builder in
      build_applycont (translate_cont_occurrence k) retval builder

    | Case(x,cases,default) ->
      begin
        let xval = translate_occurrence x in
        let cases_nb = CaseMap.cardinal cases in
        let default_bb = (match default with
          | None -> new_unreachable_block builder
          | Some(k) ->
            basic_block_that_calls
              ("bb_" ^ (Cont_var.Occur.to_string k))
              (translate_cont_occurrence k) xval builder) in
        let (tag,value) = Variant.bind (Var.Occur.to_string x) xval builder in
        let switch = Llvm.build_switch tag default_bb cases_nb builder in
        CaseMap.iter (fun i k ->
          Llvm.add_case switch (Llvm.const_int i32_type i)
            (basic_block_that_calls
               ("bb_" ^ (Cont_var.Occur.to_string k))
               (translate_cont_occurrence k) value builder))
          cases;
        End_of_block
      end

    | Halt(x) -> (match env.handle_halt with
        | Halt_returns_void -> ignore(Llvm.build_ret_void builder)
        | Halt_returns_value -> ignore(Llvm.build_ret (translate_occurrence x) builder)
        | Halt_stores_results_in(llvalue) ->
          Llvm.build_store (translate_occurrence x) llvalue builder;
          ignore(Llvm.build_ret_void builder)
    ); End_of_block

(* Declare a llvm function with name [name], [numparams] parameters.
   [returns] if true if the function returns a value, and false if it
   returns void. *)
and declare_llvm_function name numparams returns =
  match (Llvm.lookup_function name the_module) with
  | Some(f) -> f
  | None ->
    let args_type = Array.make numparams anystar_type in
    let ret_type = if returns then anystar_type else void_type in
    let function_type = Llvm.function_type ret_type args_type in
    let the_function = Llvm.declare_function name function_type the_module in
    the_function

(*s The following function factorizes the creation of a function with
  LLVM. It takes the following arguments:

  \begin{itemize}

  \item [name], a string [name] does not need to be unique, just
  informative.

  \item [params] is [Some(cont_var,var)] if the LLvm function takes
  arguments, or None otherwise.

  \item [cpsbody] is the CPS term representing the body of the
  function to be translated.

  \item [handle_halt] states how [Halt(x)] CPS terms must be
  translated.

  \item [globalvarmap] is the mapping from global CPS variables to
  llvalues.

  \end{itemize}
*)
and created_functions = ((Hashtbl.create 47):(string, unit) Hashtbl.t)
and build_llvm_function name ~params cpsbody handle_halt globalvarmap =

  (* It is important for LLVM that function names are unique; but
     names are used by LLVM as identifiers, so we cannot uniquify them
     here; [name] must already be uniquifed by the caller. The
     hashtable allows to check that: a name is in the hashtable iff
     a function with the same name has been built. *)
  if Hashtbl.mem created_functions name
  then failwith ("Calling build_llvm_function twice with name `" ^ name ^"'")
  else Hashtbl.add created_functions name ();

  let numparams = match params with
      | Some(_,l) -> (List.length l)
      | None -> 0 in
  let returns = match handle_halt with
       | Halt_returns_value -> true
       | Halt_stores_results_in _ | Halt_returns_void -> false in
  let the_function = declare_llvm_function name numparams returns in

  (* Compute the initial environment; this requires that [the_function] is created. *)
  let (initial_contvarmap, initial_varmap) = match params with
    | Some(k,l) -> (Cont_var_Map.singleton k Ret,
                    List.fold_left_with_index (fun map x i ->
                      Var_Map.add x (Llvm.param the_function i) map)
                      globalvarmap l)
    | None -> (Cont_var_Map.empty, globalvarmap) in
  let initial_env = { contvarmap = initial_contvarmap;
                      varmap = initial_varmap;
                      globalvarmap = globalvarmap;
                      handle_halt = handle_halt
                    } in

  (* Build the function. *)
  let bb = Llvm.append_block context "entry" the_function in
  (* Note that we use a new builder. If OCaml supported SMP, functions
     could even be built in parallel. *)
  let builder = Llvm.builder context in
  Llvm.position_at_end bb builder;
  try
    ignore(build_term cpsbody initial_env builder);
    (* Prints the textual representation of the function to stderr. *)
    if Log.Llvm_output.is_output Log.Debug
    then Llvm.dump_value the_function
    else ();
    (* Validate the code we just generated.  *)
    Llvm_analysis.assert_valid_function the_function;
    the_function
  (* Normally, no exception should be thrown, be we never know. *)
  with e -> Llvm.delete_function the_function; raise e

(* A function takes parameters and returns a result with "return". *)
and build_function name contparam params cpsbody globalvarmap =
  build_llvm_function name ~params:(Some(contparam,params)) cpsbody Halt_returns_value globalvarmap
;;

(* Build a thunk that executes an expression when called.  *)
let build_unbound_def cpsbody globalvarmap =
  build_llvm_function (uniquify_name "nodef") ~params:None cpsbody Halt_returns_void globalvarmap;;

(* A definition is a global variable, plus a constructor function that
   stores a value in it. The constructor is also a thunk, that stores a
   result in the global variable when called.*)
let build_bound_def var cpsbody globalvarmap =
  let varname = Var.Var.to_string var in
  let funname = ("construct_" ^ varname) in
  let the_variable = Llvm.define_global varname undef_anystar the_module in
  let the_function =
    build_llvm_function funname ~params:None cpsbody (Halt_stores_results_in the_variable) globalvarmap in
  (the_variable, the_function)
;;

(* Build a toplevel definition. *)
let build_definition def globalvarmap =
  let (Definition(visib,Dynamic_value(expr))) = def in

  match visib with
    (* The result of the expression is meaningful, and bound to a variable.  *)
    | Public(var) | Private(var) ->
      let (the_variable, the_function) = build_bound_def var expr globalvarmap in
      let newmap = Var_Map.add var the_variable globalvarmap in
      (the_function, newmap)
    (* We do not care about the result of the expression. *)
    | Unused ->
      let the_function = build_unbound_def expr globalvarmap in
      (the_function, globalvarmap)
;;
