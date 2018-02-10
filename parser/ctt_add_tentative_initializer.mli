(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Adds tentative zero initializers for all uninitialized global declarations. *)

val fill_bss_initializers : bool ref
(** Whether [reduce_program] should add initializers for global, non-static, uninitialized variables. *)

val reduce_program :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.global_declaration list ->
  Ctt_abstree.environment * Ctt_abstree.global_declaration list
