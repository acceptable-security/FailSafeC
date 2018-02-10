(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Inserts bounds-check conditions (ILR -> ILC) *)

val attr_nocheck : Ilc.check_attribute

val f :
  genv:Ctt_abstree.environment ->
  Ils.ilr_global_declaration list ->
  (Ilc.ilc, Ils.ilr_type, Ils.ils_initializer) Il.il_global_declaration_base
  list

val dump_program :
  genv:Ctt_abstree.environment ->
  (Ilc.ilc, 'b, Ils.ils_initializer) Il.il_global_declaration_base list ->
  unit
