(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

(** ILS: an intermediate language with base/ofs separation *)

(** The type of IL in this module is used for both SSA/non-SSA representations. *)

open Big_int
open Ctt_abstree
open Cttm_abstree
open Il

(**{6 ILS: Intermediate language with base/offset separation}*)

type temp_id = int

type identifier = string

and field = identifier * c_type

type ils_switch_label = Il.il_switch_label = CASE of big_int | DEFAULT

type ils_if_type = Il.il_if_type = IFNOT | IFTRUE

type ils_binop = Il.il_binop = 
    ILbinTimes | ILbinDiv
  | ILbinPlusVV | ILbinMinusVV 
  | ILbinPlusPV | ILbinMinusPP | ILbinMinusPV
  | ILbinModulo | ILbinLshift
  | ILbinRshift | ILbinLogAnd | ILbinLogOr | ILbinIntAnd | ILbinIntOr | ILbinIntXor
  | ILbinLessThan | ILbinLessEqual | ILbinGtrThan | ILbinGtrEqual
  | ILbinEqual | ILbinNotEqual


type ils_type = 
  | ILStypeVal of c_type
  | ILStypeBase of c_type
  | ILStypeBaseTemp of c_type (* obsolete? *)
  | ILStypeOfs of c_type

type ils_vartype = Il2.il2_vartype = 
    GlobalVar | HeapVar | StackVar | RegVar

type ils_lvalue = 
    ILSlvPtr of temp_id * temp_id
  | ILSlvPtrToFunc of temp_id * temp_id
  | ILSlvVar of ils_vartype * identifier * c_type
  | ILSlvSVar of ils_vartype * identifier * ils_type
  | ILSlvTemp of temp_id (* bulk type *)

type ils_initializer_base =
    ILSinitbaseVar of ils_vartype * identifier * c_type
  | ILSinitbaseNone
  | ILSinitbaseTypeInfo of c_type
  | ILSinitbaseString of string

type ils_initializer_offset =
    ILSinitofsInt of big_int
  | ILSinitofsFloat of float

type ils_initializer_t =
    ILSinitConstant of c_type * ils_initializer_base * ils_initializer_offset
  | ILSinitPostFill
  | ILSinitArray of c_type * ils_initializer list
  | ILSinitStruct of c_type * (identifier * ils_initializer) list

and ils_initializer = ils_initializer_t Locterm.t

type ils_funcarg = 
    ILSFuncArgNarrow of temp_id
  | ILSFuncArgWide of temp_id * temp_id

type ils_expr =
  | ILSexpCoerce1 of c_type * temp_id
  | ILSexpCoerce2 of c_type * temp_id * temp_id
  | ILSexpConstant of c_constants
  | ILSexpUndefined
  | ILSexpBinop of il_binop * temp_id * temp_id
  | ILSexpBinop21 of il_binop * (temp_id * temp_id) * temp_id (** a special case of binary operator which operates 2x1 arguments *)
  | ILSexpUnaryop of unaryop * temp_id
  | ILSexpInvoke of ils_lvalue * ils_funcarg list
  | ILSexpAddress of ils_lvalue * field list
  | ILSexpArgument of int
  | ILSexpArgumentB of int
  | ILSexpArgumentV of int
  | ILSexpIdent of temp_id

type ils_error = 
    ILSerrPtrMinus
  | ILSerrPtrComp
  | ILSerrAborted of Il.abort_reason

type ils_t =
    ILSstmtIf of il_if_type * temp_id * int
  | ILSstmtSwitch of temp_id * ( il_switch_label * int ) list
  | ILSstmtGoto of int
  | ILSstmtReturn0
  | ILSstmtReturn1 of temp_id
  | ILSstmtReturn2 of temp_id * temp_id
  | ILSstmtAssign of temp_id * ils_expr
  | ILSstmtAssign2 of temp_id * temp_id * ils_expr
  | ILSstmtRead1 of temp_id * ils_lvalue * field list
  | ILSstmtRead2 of temp_id * temp_id * ils_lvalue * field list
  | ILSstmtWrite1 of ils_lvalue * field list * temp_id
  | ILSstmtWrite2 of ils_lvalue * field list * temp_id * temp_id
  | ILSstmtDeclScalar of ils_vartype * c_type * identifier
  | ILSstmtDeclBulk of ils_vartype * c_type * identifier
  | ILSstmtInitialize of ils_vartype * c_type * identifier * 
	ils_initializer (* bulk assignment *)
  | ILSstmtSequence of ils list
  | ILSstmtParallel of ils list

  | ILSstmtAbort of ils_error
  | ILSstmtAbortIf of ils_expr * ils_error

and ils = ils_t Locterm.t

type ils_basic_block = ils Il.il_basic_block_base

type ils_variable_attribute = ils_type il_variable_attribute_base

type ils_function = (ils,ils_type) il_function_base

type ils_global_declaration = (ils,ils_type,ils_initializer) il_global_declaration_base

(**{6 ILR: ILS with value range analysis}*)

(* result of range analysis *)

type trivalue = Always | Maybe | Never

type base_size_info = Base_exact of big_int | Base_lowerbound of big_int
    (* unknown == Base_lowerbound 0 *)

type ilr_base_attributes = {
    ilr_base_null : trivalue;
    ilr_base_cast : trivalue;
    ilr_base_size : base_size_info;
  }

type value_range_info = Value_any (** TODO *)

type ilr_type = 
  | ILRtypeVal of c_type * value_range_info
  | ILRtypeBase of c_type * ilr_base_attributes
  | ILRtypeBaseTemp of c_type * ilr_base_attributes
  | ILRtypeOfs of c_type * value_range_info

type ilr_variable_attribute = ilr_type il_variable_attribute_base

type ilr_base_usetype_info = 
    Bunanalyzed | Bunknown | Brettype | Bused of c_type | Bcast of c_type | Bscalar

type base_var_usetype_info = ilr_base_usetype_info Util.earray

type ilr_function = (ils,ilr_type) il_function_base * base_var_usetype_info

type ilr_global_declaration_desc = 
    ILRdeclFunction of global_storage_class * c_type 
	* identifier * identifier list * ilr_function
  | ILRdeclVariable of 
      global_storage_class * c_type * identifier * ils_initializer option

type ilr_global_declaration = 
    ilr_global_declaration_desc Locterm.t

type definition =
    Variable of ils_initializer
  | Function of ( ils, ils_type ) il_function_base

type mode = Definite of definition | Tentative

