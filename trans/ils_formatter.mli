(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Pretty printer for ILS and ILR. *)

(** printers for ILS. *)

val pp_list :
  (Format.formatter -> 'a -> unit) ->
  string -> Format.formatter -> 'a list -> unit

val pp_temp_id : Format.formatter -> int -> unit
val pp_identifier : Format.formatter -> string -> unit
val pp_field : Format.formatter -> string * Ctt_abstree.c_type -> unit
val pp_ils_switch_label : Format.formatter -> Il.il_switch_label -> unit
val pp_ils_if_type : Format.formatter -> Il.il_if_type -> unit
val pp_ils_binop : Format.formatter -> Il.il_binop -> unit
val pp_ctt_type : Format.formatter -> Ctt_abstree.c_type -> unit
val pp_ils_type : Format.formatter -> Ils.ils_type -> unit
val pp_ils_vartype : Format.formatter -> Ils.ils_vartype -> unit
val pp_ils_lvalue : Format.formatter -> Ils.ils_lvalue -> unit
val pp_ils_initializer_base :
  Format.formatter -> Ils.ils_initializer_base -> unit
val pp_ils_initializer_offset :
  Format.formatter -> Ils.ils_initializer_offset -> unit
val pp_ils_initializer_value :
  Format.formatter ->
  Ils.ils_initializer_base * Ils.ils_initializer_offset -> unit
val pp_ils_initializer : Format.formatter -> Ils.ils_initializer -> unit
val pp_ils_funcarg : Format.formatter -> Ils.ils_funcarg -> unit
val pp_ils_expr : Format.formatter -> Ils.ils_expr -> unit
val pp_ils_error : Format.formatter -> Ils.ils_error -> unit
val pp_ils : Format.formatter -> Ils.ils -> unit
val pp_ils_basic_block :
  int -> Format.formatter -> Ils.ils Il.il_basic_block_base -> unit
val pp_ils_function :
  Format.formatter -> (Ils.ils, 'a) Il.il_function_base -> unit
val pp_ils_global_declaration :
  Format.formatter ->
  (Ils.ils, 'a, Ils.ils_initializer) Il.il_global_declaration_base -> unit
val pp_ils_program :
  Format.formatter ->
  (Ils.ils, 'a, Ils.ils_initializer) Il.il_global_declaration_base list -> unit

(** Printers for ILR. *)

val pp_trivalue : Format.formatter -> Ils.trivalue -> unit
val pp_base_size_info : Format.formatter -> Ils.base_size_info -> unit
val pp_ilr_type : Format.formatter -> Ils.ilr_type -> unit
val pp_ilr_base_usetype_info :
  Format.formatter -> Ils.ilr_base_usetype_info -> unit
val pp_ilr_function :
  Format.formatter -> (Ils.ils, 'a) Il.il_function_base -> unit
val pp_ilr_global_declaration :
  Format.formatter ->
  (Ils.ils, 'a, Ils.ils_initializer) Il.il_global_declaration_base -> unit
