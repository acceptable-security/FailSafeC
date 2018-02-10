(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Dummy bound-check condition analyzer (ILS -> ILR) *)

val f :
  genv:Ctt_abstree.environment ->
  (Ils.ils, Ils.ils_type, Ils.ils_initializer) Il.il_global_declaration_base list
  -> Ils.ilr_global_declaration list
