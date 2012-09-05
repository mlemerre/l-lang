(* Copyright 2012 Matthieu Lemerre. *)

(*i \section{Module Cpsllvm} i*)

(*s This module translates a term written in CPS representation to
  LLVM instructions in SSA form.

  The CPS representations stems from the paper "Compiling with
  continuations, continued" by Andrew Kennedy. In particular this
  representation separates continuations from standard lambda
  functions, which allows calling and returning from functions using
  the normal stack.

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

  \item A CPS variable $x$ is mapped to a SSA variables (of type
  [Llvm.llvalue]). CPS variables are introduced as arguments to lambda
  and continuations, and in the $let x = ... $ form.

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
let the_module = Llvm.create_module context "my jitted module";;

let void_type = Llvm.void_type context;;
let i32_type = Llvm.i32_type context;;
let i32star_type = Llvm.pointer_type i32_type;;
let anystar_type = Llvm.pointer_type (Llvm.i8_type context);;

open Cpsbase;;

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
let build_box llvalue name builder = 
  let lltype = Llvm.type_of llvalue in
  let pointer = Llvm.build_malloc lltype name builder in
  ignore(Llvm.build_store llvalue pointer builder);
  Llvm.build_bitcast pointer anystar_type (name ^ "box") builder;;

(*s Unbox a [llvalue] of type [lltype].  *)
let build_unbox llvalue lltype name builder = 
  let typeptr = Llvm.pointer_type lltype in
  let castedptr = Llvm.build_bitcast llvalue typeptr (name ^ "castedptr") builder in
  Llvm.build_load castedptr (name ^ "unbox") builder;;
  
(*s A n-tuple is allocated as an array of n [anystar_type]. Each
   element of the array contains the llvalue in l. *)
let build_tuple l builder = 
  let length = List.length l in
  let array_type = Llvm.array_type anystar_type length in 
  let pointer = Llvm.build_malloc array_type "tuple" builder in

  let f () (int,elem) = 
    (* Note: the first 0 is because pointer is not the start of
       the array, but a pointer to the start of the array, that
       must thus be dereferenced. *)
    let path = [| (Llvm.const_int i32_type 0); (Llvm.const_int i32_type int) |] in
    let gep_ptr = Llvm.build_gep pointer path "gep" builder in
    ignore(Llvm.build_store elem gep_ptr builder) in

  Utils.Int.fold_with_list f () (0,l);
  Llvm.build_bitcast pointer anystar_type ("tuplecast") builder;;

(*s Retrieve an element from a tuple.  *)
let build_letproj pointer i builder = 
  let stringi = (string_of_int i) in 
  (* First we compute an acceptable LLvm type, and cast the pointer to
     that type (failure to do that makes [Llvm.build_gep] segfault).
     As we try to access the ith element, we assume we are accessing
     an array of size i+1. *)
  let array_type = Llvm.array_type anystar_type (i+1) in 
  let arraystar_type = Llvm.pointer_type array_type in
  let cast_pointer = Llvm.build_bitcast pointer arraystar_type ("castptr") builder in
  let gep_ptr = Llvm.build_gep cast_pointer [| (Llvm.const_int i32_type 0);
                                               (Llvm.const_int i32_type i) |] 
    ("gep" ^ stringi) builder in 
  let result = Llvm.build_load gep_ptr ("builder" ^ stringi) builder in
  result ;;

(*s Apply primitive operations.  *)
let build_integer_binary_op op a b builder = 
  let build_fn = match op with
    | Constant.IAdd -> Llvm.build_add
    | Constant.ISub -> Llvm.build_sub
    | Constant.IMul -> Llvm.build_mul
    | Constant.IDiv -> Llvm.build_udiv in
  let a_unbox = (build_unbox a i32_type "a" builder) in
  let b_unbox = (build_unbox b i32_type "b" builder) in
  let res = build_fn a_unbox b_unbox "bop" builder in
  build_box res "res" builder;;
  


(*s Build a call instruction, casting [caller] to a function pointer. *)
let build_call caller callee builder =
  let function_type = Llvm.pointer_type (Llvm.function_type anystar_type [| anystar_type |]) in
  let casted_caller = Llvm.build_bitcast caller function_type "function" builder in 
  let retval = Llvm.build_call casted_caller [| callee |] "retval" builder in
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
let new_block builder = 
  let current_bb = Llvm.insertion_block builder in
  let the_function = Llvm.block_parent current_bb in
  let new_bb = Llvm.append_block context "k" the_function in
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
  The CPS$\to{}$LLVM translation maps continuation variables to [dest_type]s.
*)
type dest_type = 
  | Ret 
  | Jmp_to of Llvm.llbasicblock 

(* Build a call to a continuation [k x]. *)
let build_applycont k x builder = 
  match k with
    | Ret -> ignore(Llvm.build_ret x builder); End_of_block
    | Jmp_to(destination) -> build_jmp_to_and_add_incoming destination x builder;;

(* \subsection*{Main CPS term translation} *)

(* It is important for LLVM that function names are unique. *)
module UniqueFunctionId = Unique.Make(struct end);;

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
let rec build_term cps (contvarmap, varmap) builder = 

  (*s Helper functions to retrieve/add values from/to maps.  *)
  let lookup_var x = 
    try VarMap.find x varmap 
    with _ -> failwith "in lookup" in

  let lookup_contvar k = 
    try ContVarMap.find k contvarmap 
    with _ -> failwith "in contvar lookup" in

  let add_to_varmap var value = VarMap.add var value varmap in
  let add_to_contvarmap contvar block = ContVarMap.add contvar (Jmp_to block) contvarmap in

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
  match cps with 

    (*s These cases build a new value, then continue building the
      basic block. *)
    | Let_value(x, value, body) -> 
      let newllvalue = 
        (match value with 
          | Constant(Constant.Int i) ->
            let llvalue = Llvm.const_int i32_type i in
            build_box llvalue ("int" ^ string_of_int i) builder
              
          | Tuple(l) ->
            let llvalues = List.map lookup_var l in
            build_tuple llvalues builder

          (* This build a new function, with private linkage (since
             that it can be used only by the current term), which
             allows llvm optimizations.

             Note that [build_function] will use a new builder, so the
             lambda can be built in parallel with the current
             function. Also it will use new variables and continuation
             variable maps (with only the x parameter), so the lambda
             expression must not contain any free variables. *)
          | Lambda(k,x,body) ->         
            let f = build_function "lambda" k x body in
            Llvm.set_linkage Llvm.Linkage.Private f;
            Llvm.build_bitcast f anystar_type "lambdacast" builder

          (* Expressions such as $let x = primitive] should have
             been translated into something like $let x = { (a,b) ->
             primitiveop( a,b) }$] in previous compilation stage, so
             should fail here. *)
          | Constant(c) -> 
            assert( Constant.is_function c);
            failwith "ICE: primitive operations as value in LLVM translation."
        )
      in build_term body (contvarmap, (add_to_varmap x newllvalue)) builder

    (* Primitive operations are similar to letvalue.  *)
    | Let_primop(x,prim,body) -> 
      let result = (match prim with 
        | Integer_binary_op(op,xa,xb) -> 
          build_integer_binary_op op (lookup_var xa) (lookup_var xb) builder
        | Projection(x,i) -> build_letproj (lookup_var x) i builder
      ) in
      build_term body (contvarmap, (add_to_varmap x result)) builder

    (*i
      XXX: And If? as it is special, we will wait until we have if to
      decide for how cpsbase should look like. I don't know yet how
      ifs (and and, or...) will be handled. It is not a terminator
      instruction in CPS.

      i*)


    (*s Building new basic blocks. The algorithm first creates an
      empty basic block, bound to [k], then build [body], then build
      [term] (if [k] is really called), binding [x] to the phi node.

      The tricky part is that the llvm bindings do not allow to create
      an "empty" phi node (even if it would, in future implementations
      which would not box everything we would still have to know the
      llvm type of the phi node, and that llvm type is not known until
      we have processed the jumps to that node). So it is the calls to
      k that create or change the phi node; no phi node means [k] is
      never called.

      Doing the operations in this order ensures that calls to [k] are
      processed before [k] is built. *)
    | Let_cont(k,x,term,body) -> 
      let new_bb = new_block builder in
      let newcvm = add_to_contvarmap k new_bb in
      let End_of_block = build_term body (newcvm, varmap) builder in
      Llvm.position_at_end new_bb builder;
      (match begin_with_phi_node new_bb with
        | None -> End_of_block
        | Some(phi) -> build_term term (newcvm, (add_to_varmap x phi)) builder)

    (*s Cases that change or create basic blocks. *)
    (* Depending on k, applycont either returns or jumps to k.  *)
    | Apply_cont(k,x) -> 
      build_applycont (lookup_contvar k) (lookup_var x) builder

    (* The CPS semantics state that caller should return to k, but
       LLVM SSA does not require that calls end basic blocks. So we
       just build a call instruction, and then a call to [k]. LLVM
       optimizations will eliminate the superfluous jump if needed. *)
    | Apply(caller,k,callee) -> 
      let retval = build_call (lookup_var caller) (lookup_var callee) builder in
      build_applycont (lookup_contvar k) retval builder

    | Halt(x) -> ignore(Llvm.build_ret (lookup_var x) builder); End_of_block

(* Expression built out of a definition are put in a "void -> void" function. *)  
and build_nodef name cpsbody = 
  prepare_build name cpsbody None

and build_function name contparam param cpsbody =
  prepare_build name cpsbody (Some (contparam,param))

(* Build the function around the main term cpsbody, possibly taking
   some parameters k and x. *)
and prepare_build name cpsbody param = 
  let params_type = match param with None -> [| |] | _ -> [| anystar_type |] in
  let function_type = Llvm.function_type anystar_type params_type in
  (* Note: it is important for LLVM that function names are unique.  *)
  let funname = name ^ "#" ^ (UniqueFunctionId.to_string (UniqueFunctionId.fresh())) in
  let the_function = Llvm.declare_function funname function_type the_module in
  let bb = Llvm.append_block context "entry" the_function in
  (* Note that we use a new builder. We could even build the functions in parallel. *)
  let builder = Llvm.builder context in
  Llvm.position_at_end bb builder;
  try 
    let initial_varmaps = 
      match param with 
        | None -> (ContVarMap.empty, VarMap.empty)
        | Some(k,x) -> (ContVarMap.singleton k Ret,
                        VarMap.singleton x (Llvm.param the_function 0)) in

    ignore(build_term cpsbody initial_varmaps builder);
    (* Prints the textual representation of the function to stderr. *)
    Llvm.dump_value the_function;
    (* Validate the code we just generated.  *)
    Llvm_analysis.assert_valid_function the_function;
    the_function
  (* Normally, no exception should be thrown, be we never know. *)
  with e -> Llvm.delete_function the_function; raise e;;


