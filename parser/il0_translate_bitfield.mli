(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Translate bitfield operations to bit-wise arithmetic operations. *)

val translate_program :
  genv:Ctt_abstree.environment ->
  Il.il_initializer Il0.il0_global_declaration list ->
  Ctt_abstree.environment *
  Il.il_initializer Il0.il0_global_declaration list
