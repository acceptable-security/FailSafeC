(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Checks modules/libraries given and collects all modules to be included (resolves library files). *)

val modname_of_stdlib : string

val f : string list -> (string * Parse_typeinfo.module_info) list
