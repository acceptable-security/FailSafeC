(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Pretty printer for IL2 types. *)

val strof_il2_vartype : Il2.il2_vartype -> string
val pp_il2_vartype : Format.formatter -> Il2.il2_vartype -> unit
val pp_il2_lvalue : Format.formatter -> Il2.il2_lvalue -> unit
val pp_il2_expr : Format.formatter -> Il2.il2_expr -> unit
val pp_il2_cexp : Format.formatter -> Il2.il2_constant_exp -> unit
val pp_il2_initializer : Format.formatter -> Il2.il2_initializer -> unit
val pp_il2_desc : Format.formatter -> Il2.il2_desc -> unit
val pp_il2 : Format.formatter -> Il2.il2 -> unit
val pp_il2_basic_block :
  int -> Format.formatter -> Il2.il2_basic_block -> unit
val pp_il2_function : Format.formatter -> Il2.il2_function_body -> unit
val pp_il2_global_declaration_desc :
  Format.formatter -> Il2.il2_global_declaration_desc -> unit
val pp_il2_global_declaration :
  Format.formatter -> Il2.il2_global_declaration_desc Locterm.t -> unit
val pp_il2_program :
  Format.formatter -> Il2.il2_global_declaration_desc Locterm.t list -> unit
