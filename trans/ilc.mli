(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

(** ILC: ILS with check attributes *)

open Big_int
open Ctt_abstree
open Cttm_abstree
open Il
open Ils

type target_size_info = 
    ILCbaseConstant of big_int
  | ILCbaseUnknown
  | ILCbaseOk

type check_attribute = 
    {
     ilc_cattr_null : trivalue;
     ilc_cattr_cast : trivalue;
     ilc_cattr_tsize : target_size_info;
   }

type rettype_hint = Ils.ilr_base_usetype_info

type ilc_expr =
  | ILCexpCoerce1 of c_type * temp_id
  | ILCexpCoerce2 of c_type * temp_id * temp_id
  | ILCexpConstant of c_constants
  | ILCexpUndefined
  | ILCexpBinop of il_binop * temp_id * temp_id
  | ILCexpBinop21 of il_binop * (temp_id * temp_id) * temp_id
  | ILCexpUnaryop of unaryop * temp_id
  | ILCexpInvoke of ils_lvalue * ils_funcarg list * check_attribute * rettype_hint
  | ILCexpAddress of ils_lvalue * field list
  | ILCexpArgument of int
  | ILCexpArgumentV of int
  | ILCexpArgumentB of int
  | ILCexpIdent of temp_id

type ilc_t =
    ILCstmtIf of il_if_type * temp_id * int
  | ILCstmtSwitch of temp_id * ( il_switch_label * int ) list
  | ILCstmtGoto of int
  | ILCstmtReturn0
  | ILCstmtReturn1 of temp_id
  | ILCstmtReturn2 of temp_id * temp_id
  | ILCstmtAssign of temp_id * ilc_expr
  | ILCstmtAssign2 of temp_id * temp_id * ilc_expr
  | ILCstmtRead1 of temp_id * ils_lvalue * field list * check_attribute
  | ILCstmtRead2 of temp_id * temp_id * ils_lvalue * field list * check_attribute
  | ILCstmtWrite1 of ils_lvalue * field list * temp_id * check_attribute
  | ILCstmtWrite2 of ils_lvalue * field list * temp_id * temp_id * check_attribute
  | ILCstmtDeclScalar of ils_vartype * c_type * identifier
  | ILCstmtDeclBulk of ils_vartype * c_type * identifier
  | ILCstmtInitialize of ils_vartype * c_type * identifier * 
	ils_initializer (* bulk assignment *)

  | ILCstmtSequence of ilc list
  | ILCstmtParallel of ilc list
  | ILCstmtAbort of ils_error
  | ILCstmtAbortIf of ilc_expr * ils_error

and ilc = ilc_t Locterm.t

type ilc_variable_attribute = ilr_type il_variable_attribute_base

type ilc_basic_block = ilc il_basic_block_base

type ilc_function = (ilc,ilr_type) il_function_base

type ilc_global_declaration = (ilc,ilr_type,ils_initializer) il_global_declaration_base

