(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007. *)

open Util
open Locterm
open Il
open Ilc
open Format

open Ils_formatter

let pp_check_attribute ppf a = pp_print_string ppf "(?)" (* TODO *)

let pp_ctype_option ppf = function
    None -> pp_print_string ppf "--"
  | Some t -> pp_ctt_type ppf t

let pp_ilc_expr ppf = function
  | ILCexpCoerce1(ct, t1) -> fprintf ppf "(%a)%a" pp_ctt_type ct pp_temp_id t1
  | ILCexpCoerce2(ct, t1, t2) -> fprintf ppf "(%a)(%a,%a)" pp_ctt_type ct pp_temp_id t1 pp_temp_id t2
  | ILCexpConstant cc -> Il_formatter.pp_ctt_constant ppf cc
  | ILCexpUndefined -> pp_print_string ppf "(undefined)"
  | ILCexpBinop(bop, t1, t2) -> fprintf ppf "%a %a %a" pp_temp_id t1 pp_ils_binop bop pp_temp_id t2
  | ILCexpBinop21(bop, (t11, t12), t2) -> 
      fprintf ppf "(%a,%a) %a %a" 
	pp_temp_id t11 pp_temp_id t12 pp_ils_binop bop pp_temp_id t2
  | ILCexpUnaryop(uop, t1) -> fprintf ppf "%a%a" Il_formatter.pp_unaryop uop pp_temp_id t1
  | ILCexpInvoke(lv, tl, cc, rth) ->
      fprintf ppf "%a(%a)<%a,%a>" pp_ils_lvalue lv (pp_list pp_ils_funcarg ", ") tl 
	pp_check_attribute cc pp_ilr_base_usetype_info rth
  | ILCexpAddress(lv, flds) -> fprintf ppf "&%a%a" pp_ils_lvalue lv (pp_list pp_field "") flds
  | ILCexpArgument i -> fprintf ppf "argument(%d)" i
  | ILCexpArgumentB i -> fprintf ppf "argumentB(%d)" i
  | ILCexpArgumentV i -> fprintf ppf "argumentV(%d)" i
  | ILCexpIdent t1 -> pp_temp_id ppf t1

let rec pp_ilc ppf i = match locval i with
    ILCstmtIf(ift, tid, l)
    -> fprintf ppf "%a %a then goto %d"
	pp_ils_if_type ift pp_temp_id tid l
  | ILCstmtSwitch(tid, tlist) ->
      fprintf ppf "switch %a:@\n    @[<v>" pp_temp_id tid;
      List.iter
	(function l, t -> fprintf ppf "%a => %d@," pp_ils_switch_label l t) tlist;
      fprintf ppf "@]"
  | ILCstmtGoto t -> fprintf ppf "goto %d" t
  | ILCstmtReturn0 -> fprintf ppf "return"
  | ILCstmtReturn1 t1 -> fprintf ppf "return %a" pp_temp_id t1
  | ILCstmtReturn2(t1, t2) -> fprintf ppf "return (%a,%a)" pp_temp_id t1 pp_temp_id t2
  | ILCstmtAssign (t1, e1) 
    -> fprintf ppf "%a = %a"
	pp_temp_id t1 pp_ilc_expr e1
  | ILCstmtAssign2 (t1, t2, e1) 
    -> fprintf ppf "(%a,%a) = %a"
	pp_temp_id t1 pp_temp_id t2 pp_ilc_expr e1
  | ILCstmtRead1(t1, lv, flds, cc)
    -> fprintf ppf "%a = READ(%a%a)<%a>"
	pp_temp_id t1
	pp_ils_lvalue lv (pp_list pp_field "") flds
	pp_check_attribute cc
  | ILCstmtRead2(t1, t2, lv, flds, cc)
    -> fprintf ppf "(%a,%a) = READ(%a%a)<%a>"
	pp_temp_id t1 pp_temp_id t2
	pp_ils_lvalue lv (pp_list pp_field "") flds
	pp_check_attribute cc
  | ILCstmtWrite1(lv, flds, t1, cc)
    -> fprintf ppf "WRITE(%a%a) = %a<%a>"
	pp_ils_lvalue lv (pp_list pp_field "") flds
	pp_temp_id t1
	pp_check_attribute cc
  | ILCstmtWrite2(lv, flds, t1, t2, cc)
    -> fprintf ppf "WRITE(%a%a) = (%a,%a)<%a>"
	pp_ils_lvalue lv (pp_list pp_field "") flds
	pp_temp_id t1 pp_temp_id t2
	pp_check_attribute cc
  | ILCstmtDeclScalar(vt, ct, id)
    -> fprintf ppf "DECL %s : SCALAR %a %a"
	id pp_ils_vartype vt pp_ctt_type ct
  | ILCstmtDeclBulk(vt, ct, id)
    -> fprintf ppf "DECL %s : BULK %a %a"
	id pp_ils_vartype vt pp_ctt_type ct
  | ILCstmtInitialize(vt, ct, id, inits)
      -> fprintf ppf "INITIALIZE(%s : %a %a) = %a"
	  id pp_ils_vartype vt pp_ctt_type ct pp_ils_initializer inits
  | ILCstmtSequence l
      -> fprintf ppf "SEQUENCE:@\n    @[<v>";
	List.iter (fun i -> fprintf ppf "%a@;" pp_ilc i) l;
	fprintf ppf "@]"
  | ILCstmtParallel l
      -> fprintf ppf "PARALLEL:@\n    @[<v>";
	List.iter (fun i -> fprintf ppf "%a@;" pp_ilc i) l;
	fprintf ppf "@]"
  | ILCstmtAbort err -> fprintf ppf "ABORT_IF %a"
	pp_ils_error err
  | ILCstmtAbortIf(e1, err) -> fprintf ppf "ABORT_IF %a %a"
	pp_ils_error err pp_ilc_expr e1

let pp_ilc_basic_block = Il_formatter.pp_il_basic_block_base pp_ilc

let pp_ilc_function ppf f = Il_formatter.pp_il_function_base pp_ilc pp_ilr_type ppf f

let pp_ilc_global_declaration ppf f = 
  Il_formatter.pp_il_global_declaration_base
    pp_ilc pp_ilr_type pp_ils_initializer ppf f

let pp_ilc_program ppf = List.iter (pp_ilc_global_declaration ppf)
