(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

(** IL2: intermediate language with local variable classifications. *)

open Big_int
open Ctt_abstree
open Cttm_abstree
open Set_list

type temp_id = int

type identifier = string

and field = identifier * c_type

type il_switch_label = Il.il_switch_label = CASE of big_int | DEFAULT

type il_if_type = Il.il_if_type = IFNOT | IFTRUE

type il_binop = Il.il_binop = 
    ILbinTimes | ILbinDiv
  | ILbinPlusVV | ILbinMinusVV 
  | ILbinPlusPV | ILbinMinusPP | ILbinMinusPV
  | ILbinModulo | ILbinLshift
  | ILbinRshift | ILbinLogAnd | ILbinLogOr | ILbinIntAnd | ILbinIntOr | ILbinIntXor
  | ILbinLessThan | ILbinLessEqual | ILbinGtrThan | ILbinGtrEqual
  | ILbinEqual | ILbinNotEqual

type il2_vartype = 
    GlobalVar (** statically allocated *)
  | HeapVar   (** address may be escape from the function *)
  | StackVar  (** address taken, or not atomic, but safely deallocatable after exitting *)
  | RegVar    (** no address taken, atomic: can be promoted *)

type il2_lvalue = 
    IL2lvPtr of temp_id
  | IL2lvVar of il2_vartype * identifier * c_type
  | IL2lvTemp of temp_id

type il2_expr =
  | IL2expCoerce of c_type * temp_id
  | IL2expConstant of c_constants
  | IL2expUndefined
  | IL2expBinop of il_binop * temp_id * temp_id
  | IL2expUnaryop of unaryop * temp_id
  | IL2expInvoke of il2_lvalue * temp_id list
  | IL2expAddress of il2_lvalue * field list
  | IL2expArgument of int
  | IL2expIdent of temp_id

type il2_constant_exp_desc = 
    IL2cexpCoerce of c_type * il2_constant_exp
  | IL2cexpConstant of c_constants
  | IL2cexpBinop of il_binop * il2_constant_exp * il2_constant_exp
  | IL2cexpUnaryop of unaryop * il2_constant_exp
  | IL2cexpAddress of il2_lvalue * field list
  | IL2cexpAddFieldOfs of il2_constant_exp * field list

and il2_constant_exp_t =
    { il2_cexp_type : c_type; il2_cexp_t : il2_constant_exp_desc }

and il2_constant_exp = il2_constant_exp_t Locterm.t

type il2_initializer_t =
    IL2initConstantExp of c_type * il2_constant_exp
  | IL2initPostFill
  | IL2initStruct of c_type * (identifier * il2_initializer) list
  | IL2initArray of c_type * il2_initializer list

and il2_initializer = il2_initializer_t Locterm.t

type il2_desc =
(* basically from statement *)
  | IL2stmtDeclAutoScalar of il2_vartype * c_type * identifier * temp_id option
  | IL2stmtDeclBulk of il2_vartype * c_type * identifier * il2_initializer option
  | IL2stmtIf of il_if_type * temp_id * int
  | IL2stmtSwitch of temp_id * ( il_switch_label * int ) list
  | IL2stmtGoto of int
  | IL2stmtReturn of temp_id option
  | IL2stmtAbort of Il.abort_reason
(* basically from expression *)
  | IL2stmtAssign of temp_id * c_type * il2_expr
  | IL2stmtRead of temp_id * c_type * il2_lvalue * field list
  | IL2stmtWrite of il2_lvalue * field list * temp_id
  | IL2stmtSequence of il2 list
  | IL2stmtParallel of il2 list (* Data-flow parallel *)

and il2_t = 
    { il2_t : il2_desc; }

and il2 = il2_t Locterm.t

type il2_basic_block =
    {
     location : Locterm.location;
     predecessor : int list;
     successor : int list;
     code : il2 list
   }

type function_attributes = Il.function_attributes

type variable_attribute = 
    {
     original_name : string option
   }

type il2_function_body =
    {
     body : il2_basic_block array;
(*     max_variable_number : int; *)
     variable_environment : variable_attribute array;
(*     arguments : (identifier * c_type) array;*)
     function_attributes : function_attributes
   }

type il2_global_declaration_desc = 
    IL2declFunction of global_storage_class * c_type 
	* identifier * identifier list * il2_function_body
  | IL2declVariable of 
      global_storage_class * c_type * identifier * il2_initializer option

type il2_global_declaration = 
    il2_global_declaration_desc Locterm.t
