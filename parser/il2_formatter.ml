(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

open Format
open Util
open Locterm
open Il2
open Il_formatter

let strof_il2_vartype = function
    GlobalVar -> "global"
  | HeapVar -> "localH"
  | StackVar -> "localS"
  | RegVar -> "localR"

let pp_il2_vartype ppf vt = pp_print_string ppf (strof_il2_vartype vt)

let pp_il2_lvalue ppf lv = 
  match lv with
    IL2lvPtr(id) -> fprintf ppf "*%a" pp_temp_id id
  | IL2lvVar(k, id, typ) -> 
      fprintf ppf "(%s : %s %a)"
	id (strof_il2_vartype k) pp_ctt_type typ
  | IL2lvTemp(id) -> fprintf ppf "temp(%a)" pp_temp_id id

let pp_il2_expr ppf = function
  | IL2expCoerce(ct, t1) -> fprintf ppf "(%a)%a" pp_ctt_type ct pp_temp_id t1
  | IL2expConstant cc -> pp_ctt_constant ppf cc
  | IL2expUndefined -> pp_print_string ppf "(undefined)"
  | IL2expBinop(bop, t1, t2) -> fprintf ppf "%a %a %a" pp_temp_id t1 pp_il_binop bop pp_temp_id t2
  | IL2expUnaryop(uop, t1) -> fprintf ppf "%a%a" pp_unaryop uop pp_temp_id t1
  | IL2expInvoke(lv, tl) -> fprintf ppf "%a(%a)" pp_il2_lvalue lv (pp_list pp_temp_id ", ") tl
  | IL2expAddress(lv, flds) -> fprintf ppf "&%a%a" pp_il2_lvalue lv pp_fields flds
  | IL2expArgument i -> fprintf ppf "argument(%d)" i
  | IL2expIdent t1 -> pp_temp_id ppf t1

let rec pp_il2_cexp ppf c =
  match (locval c).il2_cexp_t with
    IL2cexpCoerce(ct, c1) -> fprintf ppf "@[(%a)@;<2>%a@]" pp_ctt_type ct pp_il2_cexp c1
  | IL2cexpConstant c -> pp_ctt_constant ppf c
  | IL2cexpBinop(b, c1, c2) ->
      fprintf ppf "@[%a %a@;<2>%a@]" pp_il2_cexp c1 pp_il_binop b pp_il2_cexp c2
  | IL2cexpUnaryop(u, c1) ->
      fprintf ppf "@[%a%a@]" pp_unaryop u pp_il2_cexp c1
  | IL2cexpAddress(lv, flds) -> 
      fprintf ppf "@[&%a@;<2>%a@]" pp_il2_lvalue lv pp_fields flds
  | IL2cexpAddFieldOfs(c1, flds) ->
      fprintf ppf "@[&(%a->@;<2>%a)@]" pp_il2_cexp c1 pp_fields flds

let rec pp_il2_initializer ppf v = 
  let pp_idval ppf (id, v) = fprintf ppf ".%s=%a" id pp_il2_initializer v
  in match locval v with
    IL2initConstantExp(ct, ce) -> fprintf ppf "(%a [%a])" 
        pp_il2_cexp ce pp_ctt_type ct
  | IL2initPostFill -> fprintf ppf "(postfill)"
  | IL2initArray(ct,l) -> 
      fprintf ppf "{@[%a@ [%a]@]}" 
        (Util.pp_list pp_il2_initializer C_pp.pp_print_sep_comma) l pp_ctt_type ct
  | IL2initStruct(ct,l) -> 
      fprintf ppf "{%a [%a]}" 
        (Util.pp_list pp_idval C_pp.pp_print_sep_comma) l pp_ctt_type ct

let rec pp_il2_desc ppf = function
  | IL2stmtDeclAutoScalar (lsc, ct, id, None) ->
      fprintf ppf "@[decl_scalar %a %a@ %s@]"
	pp_il2_vartype lsc pp_ctt_type ct id
  | IL2stmtDeclAutoScalar (lsc, ct, id, Some t1) ->
      fprintf ppf "@[decl_scalar %a %a@ %s =@ %a@]"
	pp_il2_vartype lsc pp_ctt_type ct id pp_temp_id t1
  | IL2stmtDeclBulk (lsc, ct, id, None) ->
      fprintf ppf "@[decl_bulk %a %a@ %s@]"
	pp_il2_vartype lsc pp_ctt_type ct id
  | IL2stmtDeclBulk (lsc, ct, id, Some init) ->
      fprintf ppf "@[decl_bulk %a %a %s =@;<1 2>%a]"
	pp_il2_vartype lsc pp_ctt_type ct id
	pp_il_initializer init
  | IL2stmtGoto t -> fprintf ppf "goto %d" t
  | IL2stmtIf(ift, tid, l)
    -> fprintf ppf "%a %a then goto %d"
        pp_il_if_type ift pp_temp_id tid l
  | IL2stmtSwitch(tid, tlist) ->
      fprintf ppf "switch %a:@\n    @[<v>" pp_temp_id tid;
      List.iter
        (function l, t -> fprintf ppf "%a => %d@," pp_il_switch_label l t) tlist;
      fprintf ppf "@]"
  | IL2stmtReturn None -> fprintf ppf "return"
  | IL2stmtReturn (Some t1) -> fprintf ppf "return %a" pp_temp_id t1
  | IL2stmtAbort r -> fprintf ppf "abort %a" pp_il_abort_reason r
  | IL2stmtAssign (t1, ct, e) ->
      fprintf ppf "%a = %a\t/* <%a> */" pp_temp_id t1 pp_il2_expr e pp_ctt_type ct
  | IL2stmtRead (tid, ct, lv, flds) ->
      fprintf ppf "%a := READ(%a%a)\t/* <%a> */" 
        pp_temp_id tid
        pp_il2_lvalue lv
        pp_fields flds
        pp_ctt_type ct
  | IL2stmtWrite (lv, flds, t1) ->
      fprintf ppf "WRITE(%a%a) <- %a"
	pp_il2_lvalue lv
        pp_fields flds
        pp_temp_id t1
  | IL2stmtSequence l
      -> fprintf ppf "SEQUENCE:@\n    @[<v>";
        List.iter (fun i -> fprintf ppf "%a@;" pp_il2 i) l;
        fprintf ppf "@]"
  | IL2stmtParallel l
      -> fprintf ppf "PARALLEL:@\n    @[<v>";
        List.iter (fun i -> fprintf ppf "%a@;" pp_il2 i) l;
        fprintf ppf "@]"
and pp_il2 ppf i = pp_il2_desc ppf (locval i).il2_t

let pp_il2_basic_block bnum ppf b =
  fprintf ppf "BLOCK %d:@\n" bnum;
  fprintf ppf "  from:%a@\n" (pp_list pp_print_int ", ") b.predecessor;
  fprintf ppf "  to  :%a@\n" (pp_list pp_print_int ", ") b.successor;
  List.iter (fun i -> fprintf ppf "    @[<v>%a@\n@]@." pp_il2 i) b.code

let pp_il2_function ppf f =
  fprintf ppf "FUNCTION:@\n";
  (* TODO: attr *)
  Array.iteri
    (fun i b -> pp_il2_basic_block i ppf b)
    f.body

let pp_il2_global_declaration_desc ppf =
  function
      IL2declFunction(gs, ct, id, argids, b) ->
        fprintf ppf "%a %a %s(%a): "
          Ctt_formatter.pp_global_storage_class gs
          pp_ctt_type ct
          id
          (pp_list pp_print_string ", ") argids;
        pp_il2_function ppf b
  | IL2declVariable(gs, ct, id, initopt) ->
      fprintf ppf "%a %a %s"
        Ctt_formatter.pp_global_storage_class gs
        pp_ctt_type ct id;
      match initopt with
        None -> fprintf ppf " : DECL@."
      | Some i -> fprintf ppf " = %a@." pp_il2_initializer i

let pp_il2_global_declaration ppf decl =
  pp_il2_global_declaration_desc ppf (Locterm.locval decl)

let pp_il2_program ppf p = 
  List.iter (pp_il2_global_declaration ppf) p

