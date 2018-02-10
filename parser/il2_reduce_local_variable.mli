(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Reduce local variables in IL2 to temporary variables *)

type reduction_parameter = {
  is_setjmp : Il2.identifier -> bool;
  forced_stack_alloc : bool;
}
val default_reduction_parameter : reduction_parameter
val translate_program :
  genv:Ctt_abstree.environment ->
  ?param:reduction_parameter ->
  Il2.il2_global_declaration list -> Il2.il2_global_declaration list
