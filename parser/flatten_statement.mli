(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Reduces labelled branches to basic-block jumps (IL0 to IL1) *)

val translate_program :
  genv:Ctt_abstree.environment ->
  Il.il_initializer Il0.il0_global_declaration list ->
  Il1.il1_global_declaration list
