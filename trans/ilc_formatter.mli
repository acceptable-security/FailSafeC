(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Pretty printer for ILC. *)

val pp_check_attribute : Format.formatter -> 'a -> unit
val pp_ctype_option : Format.formatter -> Ctt_abstree.c_type option -> unit
val pp_ilc_expr : Format.formatter -> Ilc.ilc_expr -> unit
val pp_ilc : Format.formatter -> Ilc.ilc -> unit
val pp_ilc_basic_block :
  int -> Format.formatter -> Ilc.ilc Il.il_basic_block_base -> unit
val pp_ilc_function :
  Format.formatter -> (Ilc.ilc, 'a) Il.il_function_base -> unit
val pp_ilc_global_declaration :
  Format.formatter ->
  (Ilc.ilc, 'a, Ils.ils_initializer) Il.il_global_declaration_base -> unit
val pp_ilc_program :
  Format.formatter ->
  (Ilc.ilc, 'a, Ils.ils_initializer) Il.il_global_declaration_base list -> unit
