(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Converts all local [static] declarations to alpha-converted global [static] declarations. *)

val reduce_program :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.global_declaration list ->
  Ctt_abstree.global_declaration list
