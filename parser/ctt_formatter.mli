(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Pretty printers for a typed CTT tree. *)

val encode_c_string : string -> string

val pp_list :
  Format.formatter ->
  elem_pp:(Format.formatter -> 'a -> unit) ->
  sep_pp:(Format.formatter -> unit) -> 'a list -> unit

val pp_builtin_type : Format.formatter -> Ctt_abstree.builtin_type -> unit
val pp_type_qualifiers : Format.formatter -> Ctt_abstree.c_type -> unit
val pp_c_type : Format.formatter -> Ctt_abstree.c_type -> unit
val pp_c_type_long :
  ?name:string ->
  ?argnames:string list ->
  ?genv:Ctt_abstree.environment ->
  unit -> Format.formatter -> Ctt_abstree.c_type -> unit
val pp_union_flag : Format.formatter -> Ctt_abstree.union_flag -> unit
val pp_binop : Format.formatter -> Cttm_abstree.binop -> unit
val pp_unaryop : Format.formatter -> Ctt_abstree.unaryop -> unit
val pp_ctt_constant : Format.formatter -> Cttm_abstree.c_constants -> unit
val pp_local_storage_class :
  Format.formatter -> Ctt_abstree.local_storage_class -> unit
val strof_global_storage_class : Ctt_abstree.global_storage_class -> string
val pp_global_storage_class :
  Format.formatter -> Ctt_abstree.global_storage_class -> unit
val pp_expr_desc : st:bool -> Format.formatter -> Ctt_abstree.texpr -> unit
(* st: a switch to show types *)
val pp_expr : Format.formatter -> Ctt_abstree.expr -> unit
val pp_expr_with_type : Format.formatter -> Ctt_abstree.expr -> unit
val pp_ctt_initalizer :
  Format.formatter -> Ctt_abstree.ctt_initializer -> unit
val pp_local_variable_declaration :
  Format.formatter ->
  Ctt_abstree.local_storage_class * Ctt_abstree.c_type * string *
  Ctt_abstree.ctt_initializer option -> unit
val pp_statement : Format.formatter -> Ctt_abstree.statement -> unit
val pp_global_declaration_desc :
  Format.formatter -> Ctt_abstree.global_declaration_desc -> unit
val pp_global_declaration :
  Format.formatter -> Ctt_abstree.global_declaration_desc Locterm.t -> unit
val pp_global_declarations :
  Format.formatter ->
  Ctt_abstree.global_declaration_desc Locterm.t list -> unit
val pp_mexpr_desc : Format.formatter -> Cttm_abstree.mexpr_t -> unit
val pp_mexpr : Format.formatter -> Cttm_abstree.mexpr -> unit
val pp_cttm_lv : Format.formatter -> Cttm_abstree.mem_object -> unit
val pp_fields_suffix : Format.formatter -> Cttm_abstree.field list -> unit
val pp_cttm_initalizer :
  Format.formatter -> Cttm_abstree.cttm_initializer -> unit
