(* Copyright 2013 Matthieu Lemerre *)

(* The compiler is organized into \emph{passes}. Each pass converts
   definitions of the language from a previous representation,
   performs some changes and/or optimization, and outputs definition
   in the newer, lower-level representation.

   The definitions are organized into \emph{streams}, i.e. a lazy
   infinite sequence of elements. A pass is just a \emph{stream
   transformer}, that transforms data from one stream to another.
   Transformations need not be one-to-one: i.e. a single element from
   the previous language can be translated to no, or several,
   elements. Also, a pass can keep an internal state (to record
   translation information).

   There are several ways to write a compilation pass as a stream
   transformer. In a lexer and a parser, the internal state is kept
   mainly in the stack, and the lexer and parser will repeatedly
   request new elements of the stream from various points in the code.
   In later passes, when the language is more structured into a
   succession of definitions, is is more natural to have the code
   handle one definition at a time, using a [filter] (a function that
   maps an element of the input stream to zero, one or several
   elements in the output stream; and can update its internal state).

   There are many advantages to using streams to model compilation
   passes:

   \begin{itemize}

   \item Minimal memory usage (on a uniprocessor implementation).
   Using lazy streams means that elements are produced just when they
   are about to be consumed, so they never stay for long in the system

   \item Easy switch to a parallel version. By using stream
   transformers, it is easy to create a parallel version using a
   pipeline. You just have to create one thread per compilation pass,
   and transform the streams to be a simple parallel consumer/producer
   buffer. Almost no change to the compiler would be necessary
   (ignoring OCaml poor support for parallel compilation).

   \item State encapsulation. A stream is really the encapsulation of
   a state; the state is hidden, and the only thing that you can see
   is the list of elements coming from the stream (this definition is
   called a catamorphism in mathematics). From a programming point of
   view, it means that the state in a pass, and its type, is hidden
   from the rest of the compiler; this makes the compiler more
   modular, and gives to compiler passes simpler interfaces. Using
   explicit folds instead of streams would make the code less modular:
   even if the type of the internal state could have been hidden by
   making it abstract, there would still be an abstract type and an
   abstract initial value, that would have been part of the interface,
   and would have been passed around.

   \item Following state encapsulation, streams provide a simple
   interface for passes. The only things exposed is the data type, a
   stream transformer, and a printing function for the data type. This
   make it easy to understand a pass, or to add a new pass.

   \end{itemize}*)

module Stream = Extensions.Stream;;

(* Prints elements in a stream each time they are requested. *)
let print_stream cond printf print_elt stream =
  if cond
  then Stream.iter_and_copy
    (fun elt -> printf (format_of_string "@.%a") print_elt elt) stream
  else stream

let process_file file =

  Log.Compilation_passes.info "================ File %s================" file;

  let parsetree_stream = Parser.make_stream file in

  let ast_stream = Astfromsexp.from_stream parsetree_stream in
  let ast_stream = print_stream (Log.Ast_elaboration.is_output Log.Debug)
    Log.Ast_elaboration.debug Astprint.definition ast_stream in

  let cps_stream = Cpstransform.from_stream ast_stream in
  let cps_stream = print_stream (Log.Cps_transformation.is_output Log.Debug)
    Log.Cps_transformation.debug Cpsbase.Print.definition cps_stream in

  let converted_stream =
    Stream.iter_and_copy Cpsconvertclosures.in_definition cps_stream in
  let converted_stream =
    print_stream (Log.Closure_conversion.is_output Log.Debug)
      Log.Closure_conversion.debug Cpsbase.Print.definition converted_stream in

  let llvm_stream = Cpsllvm.from_stream converted_stream in
  Stream.iter (fun elt -> Llvmexec.exec_nodef () elt) llvm_stream;;


(*i
\(* Simple version without debug information. *\)
let process_file file =
  let parsetree_stream = Parser.make_stream file in
  let ast_stream = Astfromsexp.from_stream parsetree_stream in
  let cps_stream = Cpstransform.from_stream ast_stream in
  let converted_stream =
    Stream.iter_and_copy Cpsconvertclosures.in_definition cps_stream in
  let llvm_stream = Cpsllvm.from_stream converted_stream in
  Stream.iter (fun elt -> Llvmexec.exec_nodef () elt) llvm_stream;;
i*)

(*i TODO:
   - Hide CPS and all its sub-passes in a pack.
   - Return a stream of LLVM definitions, and print them.
   - Add some code to print the parsetree.

 i*)
