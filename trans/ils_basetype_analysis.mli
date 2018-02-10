(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Type analysis for handling polymorphic pointers (on ILS) *)

val f :
  genv:Ctt_abstree.environment ->
  Ils.ilr_global_declaration list -> Ils.ilr_global_declaration list
