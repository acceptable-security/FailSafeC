(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Canonicalize IL0 basic types. *)

val translate_c_type_mem : Ctt_abstree.c_type -> Ctt_abstree.c_type (* used in ils_ssa_translate *)

val translate_program :
  genv:Ctt_abstree.environment ->
  Il.il_initializer Il0.il0_global_declaration list ->
  Ctt_abstree.environment *
  Il.il_initializer Il0.il0_global_declaration list
