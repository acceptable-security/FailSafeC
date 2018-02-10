(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007. *)

(* pretty-formatting functions for IL family *)

open Il
open Format

let pp_ctt_type ppf ct = Ctt_formatter.pp_c_type ppf ct

let rec pp_list pp_elem sep ppf v = 
  match v with
    [] -> ()
  | [v] -> pp_elem ppf v
  | h::t -> pp_elem ppf h; pp_print_string ppf sep; pp_list pp_elem sep ppf t

let pp_temp_id ppf v = fprintf ppf "%%%d" v
let pp_identifier = pp_print_string

let pp_field ppf (id, ct) = fprintf ppf ".(%s : %a)" id pp_ctt_type ct

let pp_fields ppf l = List.iter (pp_field ppf) l

let pp_il_switch_label ppf =
   function 
       CASE bi -> fprintf ppf "case %s:" (Big_int.string_of_big_int bi)
     | DEFAULT -> pp_print_string ppf "default:"
	   
let pp_il_if_type ppf s = 
  pp_print_string ppf (if s = IFNOT then "ifnot" else "if")

let pp_il_binop ppf s = 
  pp_print_string ppf
    (match s with
      ILbinTimes -> "*"
    | ILbinDiv -> "/"
    | ILbinPlusVV -> "+" 
    | ILbinMinusVV -> "-"
    | ILbinPlusPV -> "p+" 
    | ILbinMinusPP -> "p-p" 
    | ILbinMinusPV -> "p-" 
    | ILbinModulo -> "%"
    | ILbinLshift -> "<<" 
    | ILbinRshift -> ">>" 
    | ILbinLogAnd -> "&&" 
    | ILbinLogOr -> "||" 
    | ILbinIntAnd -> "&" 
    | ILbinIntOr -> "|" 
    | ILbinIntXor -> "^" 
    | ILbinLessThan -> "<" 
    | ILbinLessEqual -> "<=" 
    | ILbinGtrThan -> ">" 
    | ILbinGtrEqual -> ">=" 
    | ILbinEqual -> "=="
    | ILbinNotEqual -> "!=")

let pp_il_abort_reason ppf = function
  | ILabortNotReached -> pp_print_string ppf "NOT_REACHED"
  | ILabortOthers s -> fprintf ppf "OTHER(%s)" s

let pp_il_lvalue_gen pp_temp_id ppf = function
    ILlvPtr t1 -> fprintf ppf "*%a" pp_temp_id t1
  | ILlvVar(id, ct) -> fprintf ppf "(%s : %a)" id pp_ctt_type ct
  | ILlvTemp t1 -> fprintf ppf "temp(%a)" pp_temp_id t1

let pp_il_lvalue = pp_il_lvalue_gen pp_temp_id

let pp_unaryop = Ctt_formatter.pp_unaryop

let pp_ctt_constant = Ctt_formatter.pp_ctt_constant

let pp_il_expr ppf = function
  | ILexpCoerce(ct, t1) -> fprintf ppf "(%a)%a" pp_ctt_type ct pp_temp_id t1
  | ILexpConstant cc -> pp_ctt_constant ppf cc
  | ILexpUndefined -> pp_print_string ppf "(undefined)"
  | ILexpBinop(bop, t1, t2) -> fprintf ppf "%a %a %a" pp_temp_id t1 pp_il_binop bop pp_temp_id t2
  | ILexpUnaryop(uop, t1) -> fprintf ppf "%a%a" pp_unaryop uop pp_temp_id t1
  | ILexpInvoke(lv, tl) -> fprintf ppf "%a(%a)" pp_il_lvalue lv (pp_list pp_temp_id ", ") tl
  | ILexpAddress(lv, flds) -> fprintf ppf "&%a%a" pp_il_lvalue lv (pp_list pp_field "") flds
  | ILexpArgument i -> fprintf ppf "argument(%d)" i
  | ILexpIdent t1 -> pp_temp_id ppf t1
(*
type il_constant_exp_desc = Il1.il_constant_exp_desc =
    ILcexpCoerce of c_type * il_constant_exp
  | ILcexpConstant of c_constants
  | ILcexpBinop of il_binop * il_constant_exp * il_constant_exp
  | ILcexpUnaryop of unaryop * il_constant_exp
  | ILcexpAddress of il_lvalue * field list
  | ILcexpAddFieldOfs of il_constant_exp * field list

and il_constant_exp = Il1.il_constant_exp = 
    { il_cexp_type : c_type; il_cexp_t : il_constant_exp_desc }

type il_initializer = Il1.il_initializer =
    ILinitConstantExp of c_type * il_constant_exp
  | ILinitPostFill
  | ILinitStruct of c_type * (identifier * il_initializer) list
  | ILinitArray of c_type * il_initializer list
  | ILinitAbstractCtt of Cttm_abstree.mexpr (* only for macro emit *)
*)
let pp_il_initializer ppf i = pp_print_string ppf "(initializer(unimp))"

let pp_il_basic_block_base pp_instr bnum ppf b =
  fprintf ppf "BLOCK %d:@\n" bnum;
  fprintf ppf "  from:%a@\n" (pp_list pp_print_int ", ") b.predecessor;
  fprintf ppf "  to  :%a@\n" (pp_list pp_print_int ", ") b.successor;
  if b.immediate_dominator >= 0 then fprintf ppf "  idom:%d@\n" b.immediate_dominator;
  if b.nest_level >= 0 then fprintf ppf "  nlvl:%d@\n" b.nest_level;
  List.iter
    (function i, p ->
      fprintf ppf "    PHI: %a := {%a}@\n" 
	pp_temp_id i (pp_list pp_print_int ", ") (Array.to_list p)) b.phi_function;
  List.iter (fun i -> fprintf ppf "    @[<v>%a@\n@]@." pp_instr i) b.code

(*type 'typ il_variable_attribute_base =
    {
      original_name : string option;
      variable_type : 'typ;
      storage_class : local_storage_class
    }

type il_variable_attribute = c_type il_variable_attribute_base
*)

let pp_il_function_base pp_instr pp_typ ppf f =
  fprintf ppf "FUNCTION:@\n";
  fprintf ppf "  maxvar:%d@\n" f.max_variable_number;

  Array.iteri
    (fun i b -> pp_il_basic_block_base pp_instr i ppf b)
    f.body

let pp_il_global_declaration_base_desc pp_instr pp_typ pp_init ppf =
  function
      ILdeclFunction(gs, ct, id, argids, b) ->
	fprintf ppf "%a %a %s(%a): "
	  Ctt_formatter.pp_global_storage_class gs
	  pp_ctt_type ct
	  id
	  (pp_list pp_print_string ", ") argids;
	pp_il_function_base pp_instr pp_typ ppf b
  | ILdeclVariable(gs, ct, id, initopt) ->
      fprintf ppf "%a %a %s"
	Ctt_formatter.pp_global_storage_class gs
	pp_ctt_type ct id;
      match initopt with
	None -> fprintf ppf " : DECL@."
      | Some i -> fprintf ppf " = %a@." pp_init i

let pp_il_global_declaration_base pp_instr pp_typ pp_init ppf decl =
  pp_il_global_declaration_base_desc pp_instr pp_typ pp_init ppf (Locterm.locval decl)

