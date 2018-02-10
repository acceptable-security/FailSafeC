(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** IL0: simple stream-lined intermediate language with gotos *)

type il0_loop_label = BREAK | CONTINUE (* used in Separate_side_effect *)
type identifier = string
type label = string
type temp_id = int
type initfield = Iindexed of Big_int.big_int | Inamed of identifier
type il0_desc =
    IL0stmtLabel of label
  | IL0stmtDeclAutoScalar of Ctt_abstree.local_storage_class *
      Ctt_abstree.c_type * identifier * temp_id option
  | IL0stmtDeclBulk of Ctt_abstree.local_storage_class * Ctt_abstree.c_type *
      identifier * Il.il_initializer option
  | IL0stmtIf of Il.il_if_type * temp_id * label
  | IL0stmtSwitch of temp_id * (Il.il_switch_label * label) list
  | IL0stmtGoto of label
  | IL0stmtReturn of temp_id option
  | IL0stmtAbort of Il.abort_reason
  | IL0stmtDefTemp of temp_id * Ctt_abstree.c_type * Il.il_expr
  | IL0stmtReadToTemp of temp_id * Ctt_abstree.c_type * temp_id Il.il_lvalue *
      Il.field list
  | IL0stmtWrite of temp_id Il.il_lvalue * Il.field list * temp_id
  | IL0stmtSequence of il0 list
  | IL0stmtParallel of il0 list
and il0_t = {
  il0_depends : Int_set.t;
  il0_defines : Int_set.t;
  il0_nobranch : bool;
  il0_t : il0_desc;
}
and il0 = il0_t Locterm.t

type il0_function_body = {
  il0_var_types : Ctt_abstree.c_type Util.earray;
  il0_funcbody : il0;
}
type 'a il0_global_declaration_desc =
    IL0declFunction of Ctt_abstree.global_storage_class *
      Ctt_abstree.c_type * identifier * identifier list * il0_function_body
  | IL0declVariable of Ctt_abstree.global_storage_class *
      Ctt_abstree.c_type * identifier * 'a option

type 'a il0_global_declaration = 'a il0_global_declaration_desc Locterm.t

val defines_il0 : il0 -> Int_set.t
val defines_desc : il0_desc -> Int_set.int_set
val depends_il0 : il0 -> Int_set.t
val depends_il0lvalue : temp_id Il.il_lvalue -> Int_set.int_set
val depends_il0expr : Il.il_expr -> Int_set.int_set
(* val depends_desc : il0_desc -> Int_set.int_set
val hasnobranch_desc : il0_desc -> bool *)
val make_il0 : loc:Locterm.location -> il0_desc -> il0
(* val separate_sequence : il0 list -> il0 list -> il0 list *)
val enclose_sequence : loc:Locterm.location -> il0 list -> il0
val enclose_parallel : loc:Locterm.location -> il0 list -> il0

val pp_il0 : Format.formatter -> il0 -> unit
val pp_il0_global_declaration : 
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a il0_global_declaration -> unit
val pp_il0_program : 
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a il0_global_declaration list -> unit
