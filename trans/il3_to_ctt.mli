(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** [Il3_to_ctt]: generating C tree from IL3 intermediate language. *)

val translate_program :
  genv:Ctt_abstree.environment ->
  (Il3.il3b_rexp,
   Il3_optimize.cogen_decisions * Il3_decompose_ssa.block_phi_info array)
  Il3.il3_global_declaration list -> Ctt_abstree.program
(** the main function. *)

val translate_from_raw_il3 :
  genv:Ctt_abstree.environment ->
  (Il.temp_id, unit) Il3.il3_global_declaration list -> Ctt_abstree.program
(** a shortcut function for use with linker. 
    it performs all necessary transformation from raw IL3 (without analysis). *)
