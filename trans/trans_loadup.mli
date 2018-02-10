(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Loader module for OCaml top-level. *)

val print_bigint : Big_int.big_int -> unit
val print_c_type : Ctt_abstree.c_type -> unit
val print_expr : Ctt_abstree.expr -> unit
val print_mexpr : Cttm_abstree.mexpr -> unit
val print_global_declaration : Ctt_abstree.global_declaration -> unit
val print_global_declarations : Ctt_abstree.global_declaration list -> unit
val print_il0 : Il0.il0 -> unit
val print_int_set : int Set_list.set -> unit
val print_c_program : C_abstree.program -> unit
val print_untyped_expr : Format.formatter -> C_abstree.expr -> unit
val print_untyped_statement : Format.formatter -> C_abstree.statement -> unit
val open_preprocess_chan : string -> in_channel
val read : string -> C_abstree.program
val step0 : string -> C_abstree.program
val step1 :
  string ->
  Ctt_abstree.environment * Ctt_abstree.global_declaration list
val step2 :
  string ->
  Ctt_abstree.environment * Ctt_abstree.global_declaration list
val step2_1 :
  string ->
  Ctt_abstree.environment * Cttm_abstree.global_declaration list
val step3 :
  string ->
  Ctt_abstree.environment *
  Il.il_initializer Il0.il0_global_declaration list
val step3_1 :
  string ->
  Ctt_abstree.environment *
  Il.il_initializer Il0.il0_global_declaration list
val step4 :
  string -> Ctt_abstree.environment * Il1.il1_global_declaration list
val print_ils : Ils.ils -> unit
val print_ilsinit : Ils.ils_initializer -> unit
val print_ilc : Ilc.ilc -> unit
val step5_1 :
  string -> Ctt_abstree.environment * Il2.il2_global_declaration list
val step5_2 :
  string -> Ctt_abstree.environment * Il2.il2_global_declaration list
val step5 :
  string ->
  Ctt_abstree.environment *
  (Ils.ils, Ils.ils_type, Ils.ils_initializer) Il.il_global_declaration_base
  list
val step6 :
  string ->
  Ctt_abstree.environment *
  (Ils.ils, Ils.ils_type, Ils.ils_initializer) Il.il_global_declaration_base
  list
val step7 :
  string -> Ctt_abstree.environment * Ils.ilr_global_declaration list
val step8 :
  string ->
  Ctt_abstree.environment *
  (Ilc.ilc, Ils.ilr_type, Ils.ils_initializer) Il.il_global_declaration_base
  list
val step9 :
  string ->
    Ctt_abstree.environment *
      (Il3.temp_id, unit) Il3.il3_global_declaration list
val step10 :
    string ->
      Ctt_abstree.environment *
	(Il3.il3b_rexp, Il3_optimize.cogen_decisions)
	Il3.il3_global_declaration list
val step11 : string -> Ctt_abstree.environment * Ctt_abstree.program
val step12 : string -> Ctt_abstree.environment * C_abstree.program
val step13 :
  string ->
  orig_t:'a Il0.il0_global_declaration list -> C_abstree.declaration list
