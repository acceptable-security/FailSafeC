(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Classify local variables for stack/heap allocation (IL1 to IL2) *)
val translate_program :
  genv:Ctt_abstree.environment ->
  Il1.il1_global_declaration list -> Il2.il2_global_declaration list
