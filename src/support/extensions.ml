(* Copyright 2013 Matthieu Lemerre. *)

(* This module contains extensions to the standard library (i.e. add
   functions to existing module).

   \begin{itemize}
   \item If you want to access all the extensions (including
   additional modules), just do [open Extensions].

   \item If you want to use the additional functions in existing
   modules, but without using the now modules, just do [open
   Extensions.Std].

   \item To extend a specific module of the standard library (e.g.
   [List]), type [module List = Extensions.List]
   \end{itemize} *)
module List = struct
  include List
  include Extensionspack.Extlist;;
end

module Std = struct
  module List = List;;
end
