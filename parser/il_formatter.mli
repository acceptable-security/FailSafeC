(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Pretty printer for IL types. *)

val pp_list :
  (Format.formatter -> 'a -> unit) ->
  string -> Format.formatter -> 'a list -> unit

val pp_ctt_type : Format.formatter -> Ctt_abstree.c_type -> unit
val pp_unaryop : Format.formatter -> Ctt_abstree.unaryop -> unit
val pp_ctt_constant : Format.formatter -> Ctt_abstree.c_constants -> unit

val pp_temp_id : Format.formatter -> int -> unit
val pp_identifier : Format.formatter -> string -> unit
val pp_field : Format.formatter -> string * Ctt_abstree.c_type -> unit
val pp_fields : Format.formatter -> (string * Ctt_abstree.c_type) list -> unit
val pp_il_switch_label : Format.formatter -> Il.il_switch_label -> unit
val pp_il_if_type : Format.formatter -> Il.il_if_type -> unit
val pp_il_binop : Format.formatter -> Il.il_binop -> unit
val pp_il_abort_reason : Format.formatter -> Il.abort_reason -> unit
val pp_il_lvalue_gen : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Il.il_lvalue -> unit
val pp_il_lvalue : Format.formatter -> int Il.il_lvalue -> unit
val pp_il_expr : Format.formatter -> Il.il_expr -> unit
val pp_il_initializer : Format.formatter -> 'a -> unit
val pp_il_basic_block_base :
  (Format.formatter -> 'a -> unit) ->
  int -> Format.formatter -> 'a Il.il_basic_block_base -> unit
val pp_il_function_base :
  (Format.formatter -> 'a -> unit) ->
  'b -> Format.formatter -> ('a, 'c) Il.il_function_base -> unit
val pp_il_global_declaration_base :
  (Format.formatter -> 'a -> unit) ->
  'b ->
  (Format.formatter -> 'c -> unit) ->
  Format.formatter -> ('a, 'd, 'c) Il.il_global_declaration_base -> unit
