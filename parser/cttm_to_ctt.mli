(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Translate CTTM expressions back to CTT expressions *)

val translate_expr : Cttm_abstree.mexpr -> Ctt_abstree.expr (* used in il0_to_ctt *)
val translate_initializer :
    Cttm_abstree.cttm_initializer -> Ctt_abstree.ctt_initializer

val translate_declaration :
  Ctt_abstree.global_storage_class * Ctt_abstree.c_type * Ctt_abstree.identifier * Cttm_abstree.cttm_initializer option ->
  Ctt_abstree.global_storage_class * Ctt_abstree.c_type * Ctt_abstree.identifier * Ctt_abstree.ctt_initializer option
