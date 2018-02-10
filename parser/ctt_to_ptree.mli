(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** convert typed C-tree to untyped C-tree *)

val encode_c_string : string -> string (* used in add_support_funcs *)
  val builtin_to_typespec :  (* used in add_support_funcs *)
    Ctt_abstree.builtin_type -> C_abstree.declaration_specifier list

val typeinfo_converter_hook :
    (genv:Ctt_abstree.environment ->
     make:(C_abstree.expr_desc -> C_abstree.expr) ->
     Ctt_abstree.c_type -> C_abstree.expr_desc)
    ref
val use_gnu_hex_float_constant : bool ref
val convert_program :
  genv:Ctt_abstree.environment ->
  ?emit_structs:bool -> Ctt_abstree.program -> C_abstree.program

val convert_declarator :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.c_type ->
  ?argnames:C_abstree.identifier list ->
  C_abstree.identifier ->
  C_abstree.declaration_specifier list * C_abstree.abstract_declarator
