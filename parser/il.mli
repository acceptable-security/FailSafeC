(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

(** IL: Defines common elements of IL family. *)

open Big_int
open Ctt_abstree
open Cttm_abstree
open Set_list

type temp_id = int

type identifier = string

and field = identifier * c_type

type il_switch_label = CASE of big_int | DEFAULT

type il_if_type = IFNOT | IFTRUE

type il_binop =
    ILbinTimes | ILbinDiv
  | ILbinPlusVV | ILbinMinusVV 
  | ILbinPlusPV | ILbinMinusPP | ILbinMinusPV
  | ILbinModulo | ILbinLshift
  | ILbinRshift | ILbinLogAnd | ILbinLogOr | ILbinIntAnd | ILbinIntOr | ILbinIntXor
  | ILbinLessThan | ILbinLessEqual | ILbinGtrThan | ILbinGtrEqual
  | ILbinEqual | ILbinNotEqual

type 't il_lvalue =
    ILlvPtr of 't
  | ILlvVar of identifier * c_type
  | ILlvTemp of 't

type il_expr =
  | ILexpCoerce of c_type * temp_id
  | ILexpConstant of c_constants
  | ILexpUndefined
  | ILexpBinop of il_binop * temp_id * temp_id
  | ILexpUnaryop of unaryop * temp_id
  | ILexpInvoke of temp_id il_lvalue * temp_id list
  | ILexpAddress of temp_id il_lvalue * field list
  | ILexpArgument of int
  | ILexpIdent of temp_id

type il_constant_exp_desc =
    ILcexpCoerce of c_type * il_constant_exp
  | ILcexpConstant of c_constants
  | ILcexpBinop of il_binop * il_constant_exp * il_constant_exp
  | ILcexpUnaryop of unaryop * il_constant_exp
  | ILcexpAddress of temp_id il_lvalue * field list
  | ILcexpAddFieldOfs of il_constant_exp * field list

and il_constant_exp_t =
    { il_cexp_type : c_type; il_cexp_t : il_constant_exp_desc }

and il_constant_exp = il_constant_exp_t Locterm.t

type il_initializer_desc =
    ILinitConstantExp of c_type * il_constant_exp
  | ILinitPostFill
  | ILinitStruct of c_type * (identifier * il_initializer) list
  | ILinitArray of c_type * il_initializer list
  | ILinitAbstractCtt of Cttm_abstree.mexpr (* only for macro emit *)

and il_initializer = il_initializer_desc Locterm.t

type abort_reason =
  | ILabortNotReached
  | ILabortOthers of string

type 'instr il_basic_block_base =
    {
      location : Locterm.location;
      predecessor : int list;
      successor : int list;
      immediate_dominator : int;
      nest_level : int;
      mutable phi_function : ( temp_id * temp_id array ) list;
      mutable code : 'instr list
   }

type 'typ il_variable_attribute_base =
    {
      original_name : string option;
      variable_type : 'typ;
      storage_class : local_storage_class
    }

type il_variable_attribute = c_type il_variable_attribute_base

type function_attributes = { setjmp_called : bool }

type ('instr,'typ) il_function_base = 
    {
     body : 'instr il_basic_block_base array;
     max_variable_number : int;
     variable_environment : 'typ il_variable_attribute_base array;
     arguments : (identifier * c_type) array;
     function_attributes : function_attributes
   }

type ('instr,'typ,'init) il_global_declaration_desc = 
    ILdeclFunction of global_storage_class * c_type 
	* identifier * identifier list * ('instr,'typ) il_function_base
  | ILdeclVariable of 
      global_storage_class * c_type * identifier * 'init option

type ('instr,'typ,'init) il_global_declaration_base = 
    ('instr,'typ,'init) il_global_declaration_desc Locterm.t

