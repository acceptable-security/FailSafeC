(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Reduces lvalue-direct assignments to pointer-indirect (ML style) assignments *)

val translate_program :
  Ctt_abstree.global_declaration list ->
  Cttm_abstree.global_declaration list
