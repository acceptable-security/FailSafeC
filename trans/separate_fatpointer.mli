(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Generate base/ofs separate representation (IL2 to ILS) *)

val translate_program :
  genv:Ctt_abstree.environment ->
  Il2.il2_global_declaration list ->
  (Ils.ils, Ils.ils_type, Ils.ils_initializer) Il.il_global_declaration_base
  list
      (** main function. *)

(** {6 support functions used in Translate_to_il3} *)

val is_pointer : Ctt_abstree.c_type -> bool

val get_field_offset :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.c_type ->
  (Ctt_abstree.identifier * Ctt_abstree.c_type) list ->
  Ctt_abstree.struct_ofsinfo


