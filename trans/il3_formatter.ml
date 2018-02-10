(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007. *)

open Util
open Locterm
open Il
open Il3
open Format

let pp_ils_if_type = Ils_formatter.pp_ils_if_type
let pp_temp_id = Ils_formatter.pp_temp_id
let pp_list = Ils_formatter.pp_list
let pp_temp_id_list = pp_list pp_temp_id ", "
let pp_c_type ppf ctype = Il_formatter.pp_ctt_type ppf ctype

let pp_rwtype ppf = function
    Byte -> fprintf ppf "byte"
  | Hword -> fprintf ppf "hword"
  | Word -> fprintf ppf "word"
  | Dword -> fprintf ppf "dword"
  | RWstruct id -> fprintf ppf "struct_%d" id
  | RWnone -> fprintf ppf "NONE?"

let pp_internal_immobile_functions ppf = function
    Alloc_heapvar -> fprintf ppf "alloc_heapvar"
  | Alloc_varargs -> fprintf ppf "alloc_varargs"
  | Alloc_valtempoline -> fprintf ppf "alloc_valtrampoline"
  | Is_offset_ok -> fprintf ppf "check_ofs_ok"
  | Get_boundary -> fprintf ppf "get_boundary"
  | Is_boundary_offset_ok -> fprintf ppf "check_boundary_ofs_ok"
  | Invoke_generic_func -> fprintf ppf "invoke_genfunc"

let pp_internal_immobile_operations ppf = function
    Dealloc_heapvar -> fprintf ppf "dealloc_heapvar"
  | Dealloc_varargs_finished -> fprintf ppf "dealloc_varargs_finished"
  | Put_varargs -> fprintf ppf "put_varargs"
  | Put_varargs_2 -> fprintf ppf "put_varargs_2"
  | Finish_varargs -> fprintf ppf "finish_varargs"
  | Enter_stack_unwind_area -> fprintf ppf "enter_stack_unwind_area"
  | Exit_stack_unwind_area -> fprintf ppf "exit_stack_unwind_area"

let pp_primitive_reducible ppf = function
    IL3pr_cons Value -> fprintf ppf "cons_val"
  | IL3pr_cons Dvalue -> fprintf ppf "cons_dval"
  | IL3pr_cons PtrValue -> fprintf ppf "cons_ptrval"

  | IL3pr_car Value -> fprintf ppf "base_of_value"
  | IL3pr_car Dvalue -> fprintf ppf "base_of_dvalue"
  | IL3pr_car PtrValue -> fprintf ppf "base_of_ptrvalue"

  | IL3pr_cdr Value -> fprintf ppf "vaddr_of_value"
  | IL3pr_cdr Dvalue -> fprintf ppf "vaddr_of_dvalue"
  | IL3pr_cdr PtrValue -> fprintf ppf "ofs_of_ptrvalue"

  | IL3pr_convert Value_of_base_ofs -> fprintf ppf "value_of_ptr"
  | IL3pr_convert Value_of_ptrvalue -> fprintf ppf "value_of_ptrvalue"
  | IL3pr_convert Ptrvalue_of_value(ct) -> fprintf ppf "ptrvalue_of_value<%a>" pp_c_type ct
  | IL3pr_convert Value_of_int -> fprintf ppf "nullexpand_value"
  | IL3pr_convert Dvalue_of_dword -> fprintf ppf "nullexpand_dvalue"

  | IL3pr_convert Value_of_dvalue -> fprintf ppf "value_of_dvalue"
  | IL3pr_convert Dvalue_of_value -> fprintf ppf "dvalue_of_value"

  | IL3pr_convert Dvalue_of_double -> fprintf ppf "dvalue_of_double"
  | IL3pr_convert Dword_of_double -> fprintf ppf "dword_of_double"
  | IL3pr_convert Double_of_dvalue -> fprintf ppf "double_of_dvalue"
  | IL3pr_convert Double_of_dword -> fprintf ppf "double_of_dword"
  | IL3pr_convert Value_of_float -> fprintf ppf "value_of_float"
  | IL3pr_convert Word_of_float -> fprintf ppf "word_of_float"
  | IL3pr_convert Float_of_value -> fprintf ppf "float_of_value"
  | IL3pr_convert Float_of_word -> fprintf ppf "float_of_word"

  | IL3pr_convert Ofs_of_value -> fprintf ppf "ofs_of_value"
  | IL3pr_convert Vaddr_of_base_ofs -> fprintf ppf "vaddr_of_base_ofs"
  | IL3pr_convert Ofs_of_base_vaddr -> fprintf ppf "ofs_of_base_vaddr"

  | IL3pr_misc Base_remove_castflag -> fprintf ppf "base_remove_castflag"
  | IL3pr_misc Base_put_castflag -> fprintf ppf "base_put_castflag"
  | IL3pr_misc Set_base_castflag ct -> fprintf ppf "set_base_castflag<%a>" pp_c_type ct
  | IL3pr_misc Add_fat_pointer ct -> fprintf ppf "add_fat_pointer<%a>" pp_c_type ct
  | IL3pr_misc Is_cast -> fprintf ppf "check_cast"
  | IL3pr_misc Get_realoffset_pointer -> fprintf ppf "get_realoffset_ptr"
  | IL3pr_misc Get_realoffset_funcptr -> fprintf ppf "get_funcptr"
  | IL3pr_misc Get_realoffset(ct) -> fprintf ppf "get_realoffset<%a>" pp_c_type ct

let pp_il3_rexp pp_s_tempid ppf = function
  | IL3RexCoerce(cty, tid) -> fprintf ppf "(%a)%a" pp_c_type cty pp_s_tempid tid
  | IL3RexConstant c -> fprintf ppf "%a" Ctt_formatter.pp_ctt_constant c
  | IL3RexUndefined -> pp_print_string ppf "undefined"
  | IL3RexBinop(bop, t1, t2) -> fprintf ppf "%a %a %a" pp_s_tempid t1 Il_formatter.pp_il_binop bop pp_s_tempid t2
  | IL3RexArgument i -> fprintf ppf "%%arg%d" i
  | IL3RexAddress(lv, flds) ->
      fprintf ppf "&%a%a" 
	(Il_formatter.pp_il_lvalue_gen pp_s_tempid) lv
	(pp_list Il_formatter.pp_field "") flds
  | IL3RexUnaryop(uop, tid) -> fprintf ppf "%a%a" Ctt_formatter.pp_unaryop uop pp_s_tempid tid

let pp_il3_iexp pp_s_tempid ppf = function
  | IL3IexInvoke(lv, tl) -> fprintf ppf "%a(%a)" (Il_formatter.pp_il_lvalue_gen pp_s_tempid) lv (pp_list pp_s_tempid ", ") tl
  | IL3IexBinop(bop, t1, t2) -> fprintf ppf "%a %a %a" pp_s_tempid t1 Il_formatter.pp_il_binop bop pp_s_tempid t2

let rec pp_il3_desc pp_s_tempid ppf = function
  | IL3stmtIf(ift, tid, l)
    -> fprintf ppf "%a %a then goto %d"
	pp_ils_if_type ift pp_s_tempid tid l
  | IL3stmtSwitch(tid, tlist) ->
      fprintf ppf "switch %a:@\n    @[<v>" pp_s_tempid tid;
      List.iter
	(function l, t -> fprintf ppf "%a => %d@," Ils_formatter.pp_ils_switch_label l t) tlist;
      fprintf ppf "@]"
  | IL3stmtGoto t -> fprintf ppf "goto %d" t
  | IL3stmtReturn None -> fprintf ppf "return"
  | IL3stmtReturn (Some t1) -> fprintf ppf "return %a" pp_s_tempid t1
  | IL3stmtMove(t1, ctype, t2) ->
      fprintf ppf "%a = %a\t/* <%a> */" pp_temp_id t1 pp_s_tempid t2 pp_c_type ctype
  | IL3stmtRexp(t1, ctype, re) ->
      fprintf ppf "%a = %a\t/* <%a> */" pp_temp_id t1 (pp_il3_rexp pp_s_tempid) re pp_c_type ctype
  | IL3stmtIexp(t1, ctype, ie) ->
      fprintf ppf "%a := %a\t/* <%a> */" pp_temp_id t1 (pp_il3_iexp pp_s_tempid) ie pp_c_type ctype
  | IL3stmtReadRaw (tid, ctype, lv, flds) ->
      fprintf ppf "%a := READ(%a%a)\t/* <%a> */" 
	pp_temp_id tid
	(Il_formatter.pp_il_lvalue_gen pp_s_tempid) lv
	(pp_list Il_formatter.pp_field "") flds
	pp_c_type ctype
  | IL3stmtWriteRaw(lv, flds, t1)
    -> fprintf ppf "WRITE(%a%a) <- %a"
	(Il_formatter.pp_il_lvalue_gen pp_s_tempid) lv
	(pp_list Il_formatter.pp_field "") flds
	pp_s_tempid t1
  | IL3stmtCallReducibleFun(tid, pr, tids) ->
      fprintf ppf "%a = RF::%a(%a)" pp_temp_id tid pp_primitive_reducible pr (pp_list pp_s_tempid ", ") tids
  | IL3stmtCallReducibleFunOverwriting(tid, pr, tids) ->
      fprintf ppf "%a <== RF::%a(%a)" pp_temp_id tid pp_primitive_reducible pr (pp_list pp_s_tempid ", ") tids
  | IL3stmtCallImmobileFun(tid, pr, tids) ->
      fprintf ppf "%a := IRF::%a(%a)" pp_temp_id tid pp_internal_immobile_functions pr (pp_list pp_s_tempid ", ") tids
  | IL3stmtCallImmobileOp(pr, tids) ->
      fprintf ppf "IO::%a(%a)" pp_internal_immobile_operations pr (pp_list pp_s_tempid ", ") tids
  | IL3stmtCallAbort(t, a) ->
      fprintf ppf "ABORT::(%a, %a)" pp_s_tempid t pp_s_tempid a
  | IL3stmtCallReaderHelper(tid, rwt, i1, i2) ->
      fprintf ppf "%a := READ::%a(%a, %a)" pp_temp_id tid pp_rwtype rwt pp_s_tempid i1 pp_s_tempid i2
  | IL3stmtCallWriterHelper(rwt, i1, i2, i3, None) ->
      fprintf ppf "WRITE::%a(%a, %a <- %a)" pp_rwtype rwt pp_s_tempid i1 pp_s_tempid i2 pp_s_tempid i3
  | IL3stmtCallWriterHelper(rwt, i1, i2, i3, Some i4) ->
      fprintf ppf "WRITE::%a(%a, %a <- %a, %a)" pp_rwtype rwt pp_s_tempid i1 pp_s_tempid i2 pp_s_tempid i3 pp_s_tempid i4
  | IL3stmtConditional(ids, id, i1, i2) ->
      fprintf ppf "[%a] := if %a:@\n    @[<v>" pp_temp_id_list ids pp_s_tempid id;
      fprintf ppf "%a@\n@]else@\n    @[<v>" (pp_il3 pp_s_tempid) i1;
      fprintf ppf "%a@\n@]end" (pp_il3 pp_s_tempid) i2
  | IL3stmtSequence l
      -> fprintf ppf "SEQUENCE:@\n    @[<v>";
	List.iter (fun i -> fprintf ppf "%a@;" (pp_il3 pp_s_tempid) i) l;
	fprintf ppf "@]"
  | IL3stmtParallel l
      -> fprintf ppf "PARALLEL:@\n    @[<v>";
	List.iter (fun i -> fprintf ppf "%a@;" (pp_il3 pp_s_tempid) i) l;
	fprintf ppf "@]"
and pp_il3 pp_s_tempid ppf i = pp_il3_desc pp_s_tempid ppf (locval i).il3_desc

let pp_il3_basic_block pp_s_tempid = Il_formatter.pp_il_basic_block_base (pp_il3 pp_s_tempid)

let pp_il3_function pp_s_tempid ppf f =
  fprintf ppf "@\nFUNCTION:@\n";
  fprintf ppf "  maxvar:%d@\n" f.max_variable_number;
  if f.prologue <> [] then
      fprintf ppf "PROLOGUE:@\n@[<v>%a@]@\n"
      (Util.pp_list Il0.pp_il0 C_pp.pp_print_sep_none)
      f.prologue;
  Array.iteri
    (fun i b -> pp_il3_basic_block pp_s_tempid i ppf b)
    f.body

let pp_il3_global_declaration pp_s_tempid ppf decl =
  match locval decl with
    IL3declFunction(gs, ct, id, argids, b) ->
      fprintf ppf "%a %a %s(%a): "
	Ctt_formatter.pp_global_storage_class gs
	Ctt_formatter.pp_c_type ct
	id
	(Ils_formatter.pp_list pp_print_string ", ") argids;
	pp_il3_function pp_s_tempid ppf b
  | IL3declVariable(gs, ct, id, initopt) ->
      fprintf ppf "%a %a %s"
	Ctt_formatter.pp_global_storage_class gs
	Ctt_formatter.pp_c_type ct id;
      match initopt with
	None -> fprintf ppf " : DECL@."
      | Some i -> fprintf ppf " = %a@." Il_formatter.pp_il_initializer i

let pp_unit ppf () = ()
let pp_ignore ppf _ = ()

let pp_il3_program pp_s_tempid pp_more_info ppf p = 
  List.iter (pp_il3_global_declaration pp_s_tempid ppf) p

let rec pp_il3b_rexp ppf = 
  function
      IL3BTemp i -> pp_temp_id ppf i
    | IL3BRfun { locterm_v = (pr, args) } ->
	fprintf ppf "RF::%a(%a)" pp_primitive_reducible pr (pp_list pp_il3b_rexp ", ") args
    | IL3BRexp { locterm_v = (_t, e) } ->
	fprintf ppf "(%a)" (pp_il3_rexp pp_il3b_rexp) e
