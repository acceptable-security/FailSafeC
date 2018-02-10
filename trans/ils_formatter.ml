(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007. *)

open Util
open Locterm
open Il
open Ils
open Format

let pp_list = Il_formatter.pp_list

let pp_temp_id = Il_formatter.pp_temp_id

let pp_identifier = Il_formatter.pp_identifier

let pp_field = Il_formatter.pp_field

let pp_ils_switch_label = Il_formatter.pp_il_switch_label

let pp_ils_if_type = Il_formatter.pp_il_if_type

let pp_ils_binop = Il_formatter.pp_il_binop

let pp_ctt_type = Il_formatter.pp_ctt_type

let pp_ils_type ppf = function
      | ILStypeVal ct -> fprintf ppf "value(%a)" pp_ctt_type ct
      | ILStypeBase ct -> fprintf ppf "base(%a)" pp_ctt_type ct
      | ILStypeBaseTemp ct -> fprintf ppf "basetemp(%a)" pp_ctt_type ct
      | ILStypeOfs ct -> fprintf ppf "ofs(%a)" pp_ctt_type ct

let pp_ils_vartype ppf vt = 
  pp_print_string ppf
    (match vt with
      GlobalVar -> "GLOBAL" | HeapVar -> "HEAP"
    | StackVar -> "STACK" | RegVar -> "REG")

let pp_ils_lvalue ppf = function
    ILSlvPtr(t1,t2) -> fprintf ppf "*(%a,%a)" pp_temp_id t1 pp_temp_id t2
  | ILSlvPtrToFunc(t1,t2) -> fprintf ppf "fptr*(%a,%a)" pp_temp_id t1 pp_temp_id t2
  | ILSlvVar(vt, id, ct)
    -> fprintf ppf "(%s : %a %a)" id pp_ils_vartype vt pp_ctt_type ct
  | ILSlvSVar(vt, id, ty)
    -> fprintf ppf "(%s : %a %a)" id pp_ils_vartype vt pp_ils_type ty
  | ILSlvTemp(t1) -> fprintf ppf "temp(%a)" pp_temp_id t1

let pp_ils_initializer_base ppf = function
    ILSinitbaseVar(vt,id,ct) -> fprintf ppf "%s[%a %a]" id pp_ils_vartype vt pp_ctt_type ct
  | ILSinitbaseNone -> fprintf ppf "None"
  | ILSinitbaseTypeInfo ct -> fprintf ppf "typeinfo(%a)" pp_ctt_type ct
  | ILSinitbaseString s -> fprintf ppf "string(%S)" s

let pp_ils_initializer_offset ppf = function
    ILSinitofsInt i -> fprintf ppf "%s" (Big_int.string_of_big_int i)
  | ILSinitofsFloat f -> fprintf ppf "%F" f

let pp_ils_initializer_value ppf (b, o) = 
  fprintf ppf "(%a,%a)" pp_ils_initializer_base b pp_ils_initializer_offset o

let rec pp_ils_initializer ppf v = 
  let pp_idval ppf (id, v) = fprintf ppf ".%s=%a" id pp_ils_initializer v
  in match locval v with
    ILSinitConstant(ct,b,o) -> fprintf ppf "(%a,%a [%a])" 
	pp_ils_initializer_base b pp_ils_initializer_offset o pp_ctt_type ct
  | ILSinitPostFill -> fprintf ppf "(postfill)"
  | ILSinitArray(ct,l) -> 
      fprintf ppf "{%a [%a]}" 
	(pp_list pp_ils_initializer ", ") l pp_ctt_type ct
  | ILSinitStruct(ct,l) -> 
      fprintf ppf "{%a [%a]}" 
	(pp_list pp_idval ", ") l pp_ctt_type ct

let pp_ils_funcarg ppf = function
    ILSFuncArgNarrow t1 -> fprintf ppf "%a" pp_temp_id t1
  | ILSFuncArgWide(t1, t2) -> fprintf ppf "(%a,%a)" pp_temp_id t1 pp_temp_id t2

let pp_ils_expr ppf = function
  | ILSexpCoerce1(ct, t1) -> fprintf ppf "(%a)%a" pp_ctt_type ct pp_temp_id t1
  | ILSexpCoerce2(ct, t1, t2) -> fprintf ppf "(%a)(%a,%a)" pp_ctt_type ct pp_temp_id t1 pp_temp_id t2
  | ILSexpConstant cc -> Il_formatter.pp_ctt_constant ppf cc
  | ILSexpUndefined -> pp_print_string ppf "(undefined)"
  | ILSexpBinop(bop, t1, t2) -> fprintf ppf "%a %a %a" pp_temp_id t1 pp_ils_binop bop pp_temp_id t2
  | ILSexpBinop21(bop, (t11, t12), t2) -> 
      fprintf ppf "(%a,%a) %a %a" 
	pp_temp_id t11 pp_temp_id t12 pp_ils_binop bop pp_temp_id t2
  | ILSexpUnaryop(uop, t1) -> fprintf ppf "%a%a" Il_formatter.pp_unaryop uop pp_temp_id t1
  | ILSexpInvoke(lv, tl) -> fprintf ppf "%a(%a)" pp_ils_lvalue lv (pp_list pp_ils_funcarg ", ") tl
  | ILSexpAddress(lv, flds) -> fprintf ppf "&%a%a" pp_ils_lvalue lv (pp_list pp_field "") flds
  | ILSexpArgument i -> fprintf ppf "argument(%d)" i
  | ILSexpArgumentB i -> fprintf ppf "argumentB(%d)" i
  | ILSexpArgumentV i -> fprintf ppf "argumentV(%d)" i
  | ILSexpIdent t1 -> pp_temp_id ppf t1

let pp_ils_error ppf err = 
  pp_print_string ppf
    (match err with
      ILSerrPtrMinus -> "ERR_PTR_MINUS"
    | ILSerrPtrComp -> "ERR_PTR_COMP"
    | ILSerrAborted (Il.ILabortNotReached) -> "ABORT_NOT_REACHED"
    | ILSerrAborted (Il.ILabortOthers s) -> "ABORT(" ^ s ^ ")")

let rec pp_ils ppf i = match locval i with
    ILSstmtIf(ift, tid, l)
    -> fprintf ppf "%a %a then goto %d"
	pp_ils_if_type ift pp_temp_id tid l
  | ILSstmtSwitch(tid, tlist) ->
      fprintf ppf "switch %a:@\n    @[<v>" pp_temp_id tid;
      List.iter
	(function l, t -> fprintf ppf "%a => %d@," pp_ils_switch_label l t) tlist;
      fprintf ppf "@]"
  | ILSstmtGoto t -> fprintf ppf "goto %d" t
  | ILSstmtReturn0 -> fprintf ppf "return"
  | ILSstmtReturn1 t1 -> fprintf ppf "return %a" pp_temp_id t1
  | ILSstmtReturn2(t1, t2) -> fprintf ppf "return (%a,%a)" pp_temp_id t1 pp_temp_id t2
  | ILSstmtAssign (t1, e1) 
    -> fprintf ppf "%a = %a"
	pp_temp_id t1 pp_ils_expr e1
  | ILSstmtAssign2 (t1, t2, e1) 
    -> fprintf ppf "(%a,%a) = %a"
	pp_temp_id t1 pp_temp_id t2 pp_ils_expr e1
  | ILSstmtRead1(t1, lv, flds)
    -> fprintf ppf "%a = READ(%a%a)"
	pp_temp_id t1
	pp_ils_lvalue lv (pp_list pp_field "") flds
  | ILSstmtRead2(t1, t2, lv, flds)
    -> fprintf ppf "(%a,%a) = READ(%a%a)"
	pp_temp_id t1 pp_temp_id t2
	pp_ils_lvalue lv (pp_list pp_field "") flds
  | ILSstmtWrite1(lv, flds, t1)
    -> fprintf ppf "WRITE(%a%a) = %a"
	pp_ils_lvalue lv (pp_list pp_field "") flds
	pp_temp_id t1
  | ILSstmtWrite2(lv, flds, t1, t2)
    -> fprintf ppf "WRITE(%a%a) = (%a,%a)"
	pp_ils_lvalue lv (pp_list pp_field "") flds
	pp_temp_id t1 pp_temp_id t2
  | ILSstmtDeclScalar(vt, ct, id)
    -> fprintf ppf "DECL %s : SCALAR %a %a"
	id pp_ils_vartype vt pp_ctt_type ct
  | ILSstmtDeclBulk(vt, ct, id)
    -> fprintf ppf "DECL %s : BULK %a %a"
	id pp_ils_vartype vt pp_ctt_type ct
  | ILSstmtInitialize(vt, ct, id, inits)
      -> fprintf ppf "INITIALIZE(%s : %a %a) = %a"
	  id pp_ils_vartype vt pp_ctt_type ct pp_ils_initializer inits
  | ILSstmtSequence l
      -> fprintf ppf "SEQUENCE:@\n    @[<v>";
	List.iter (fun i -> fprintf ppf "%a@;" pp_ils i) l;
	fprintf ppf "@]"
  | ILSstmtParallel l
      -> fprintf ppf "PARALLEL:@\n    @[<v>";
	List.iter (fun i -> fprintf ppf "%a@;" pp_ils i) l;
	fprintf ppf "@]"
  | ILSstmtAbortIf(e1, err) -> fprintf ppf "ABORT_IF %a %a"
	pp_ils_error err pp_ils_expr e1
  | ILSstmtAbort err -> fprintf ppf "ABORT %a"
	pp_ils_error err

let pp_ils_basic_block = Il_formatter.pp_il_basic_block_base pp_ils

let pp_ils_function ppf f = Il_formatter.pp_il_function_base pp_ils pp_ils_type ppf f

let pp_ils_global_declaration ppf f = 
  Il_formatter.pp_il_global_declaration_base
    pp_ils pp_ils_type pp_ils_initializer ppf f

let pp_ils_program ppf = List.iter (pp_ils_global_declaration ppf)

(* formatters for ILR_type *)

let pp_trivalue ppf tv = 
  pp_print_string ppf (match tv with Always -> "always" | Maybe -> "maybe" | Never -> "never")

let pp_base_size_info ppf = function
    Base_exact i ->
      fprintf ppf "==%s" (Big_int.string_of_big_int i)
  | Base_lowerbound i ->
      fprintf ppf ">=%s" (Big_int.string_of_big_int i)

(*let pp_ilr_base_attributes = {
    ilr_base_null : trivalue;
    ilr_base_cast : trivalue;
    ilr_base_size : base_size_info;
  }*)

let pp_ilr_type ppf = function
  | ILRtypeVal(ct, _range) -> fprintf ppf "val(%a)" pp_ctt_type ct
  | ILRtypeBase(ct, _battr) -> fprintf ppf "base(%a)" pp_ctt_type ct
  | ILRtypeBaseTemp(ct, _battr) -> fprintf ppf "basetemp(%a)" pp_ctt_type ct
  | ILRtypeOfs(ct, _range) -> fprintf ppf "ofs(%a)" pp_ctt_type ct

(* let pp_ilr_variable_attribute = pp_il_variable_attribute_base pp_ilr_type *)

let pp_ilr_base_usetype_info ppf = function
    Bunanalyzed -> pp_print_string ppf "unanalyzed"
  | Bunknown -> pp_print_string ppf "UNKNOWN"
  | Brettype -> pp_print_string ppf "RET"
  | Bused ct -> fprintf ppf "USED:%a" pp_ctt_type ct
  | Bcast ct -> fprintf ppf "CAST:%a" pp_ctt_type ct
  | Bscalar -> pp_print_string ppf "SCALAR"

let pp_ilr_function ppf = Il_formatter.pp_il_function_base pp_ils pp_ilr_type ppf

let pp_ilr_global_declaration ppf = 
  Il_formatter.pp_il_global_declaration_base pp_ils pp_ilr_type pp_ils_initializer ppf

(*
let pp_ilr_definition =
    Variable of ils_initializer
  | Function of ( ils, ils_type ) il_function_base

let pp_mode = Definite of definition | Tentative
*)
