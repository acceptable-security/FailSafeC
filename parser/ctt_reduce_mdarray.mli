(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Converts all occurence of multi-dimensional arrays to single-dimension ones. *)

(** Note: this module introduces post-incremented integers with increments other than 1. *)

val reduce_program :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.global_declaration list ->
  Ctt_abstree.environment * Ctt_abstree.global_declaration list
