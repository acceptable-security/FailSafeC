(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Reduces unions to structs and casts. *)

val translate_program :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.global_declaration list ->
  Ctt_abstree.environment * Ctt_abstree.global_declaration list
