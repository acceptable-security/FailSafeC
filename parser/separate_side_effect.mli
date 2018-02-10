(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Reduce tree-expressions to simple streamlined instructions (converts CTTM to IL0) *)

val translate_program_to_il0 :
  genv:Ctt_abstree.environment ->
  Cttm_abstree.global_declaration list ->
  Il.il_initializer Il0.il0_global_declaration list

(*val print_il0_program_cttminit :
  genv:'a ->
  Cttm_abstree.cttm_initializer Il0.il0_global_declaration list -> unit
val print_il0_program :
  genv:'a -> Il.il_initializer Il0.il0_global_declaration list -> unit

*)
