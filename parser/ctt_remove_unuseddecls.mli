(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Removes any unused external declarations. *)

val external_definition_filter_hook :
    (ext:Ctt_abstree.extension_list ->
     Ctt_abstree.c_type -> Ctt_abstree.identifier -> bool -> bool)
    ref
(** A hook for modifying decisions for external declarations (not definitions).
   The function is called with default decisions. *)

val f :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.global_declaration list ->
  Ctt_abstree.environment * Ctt_abstree.global_declaration list
(** main function. *)
