(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2004. *)

open Big_int
open Ctt_abstree

type binop = Ctt_abstree.binop = 
    CTTbinTimes | CTTbinDiv
  | CTTbinPlusVV | CTTbinMinusVV 
  | CTTbinPostPlusVV | CTTbinPostMinusVV 
  | CTTbinPlusPV | CTTbinMinusPP | CTTbinMinusPV (* added *)
  | CTTbinPostPlusPV | CTTbinPostMinusPV 
  | CTTbinModulo | CTTbinLshift
  | CTTbinRshift | CTTbinLogAnd | CTTbinLogOr | CTTbinIntAnd | CTTbinIntOr | CTTbinIntXor
  | CTTbinLessThan | CTTbinLessEqual | CTTbinGtrThan | CTTbinGtrEqual
  | CTTbinEqual | CTTbinNotEqual

type unaryop = Ctt_abstree.unaryop = UnaryPlus | UnaryMinus | LogNot | IntNot

type field = identifier * c_type

type c_constants = Ctt_abstree.c_constants = 
  | CTTconstNull
  | CTTconstInteger of big_int
  | CTTconstFloat of float
  | CTTconstString of string
  | CTTconstTypeInfo of c_type
  | CTTconstAbstract of string

type mexpr_t = { mexpr_type : c_type; mexpr_t : mexpr_desc }

and mexpr = mexpr_t Locterm.t

and mexpr_desc = 
  | CTTMexpComma of mexpr * mexpr
  | CTTMexpAddress of mem_object * field list
  | CTTMexpRead of mem_object * field list
  | CTTMexpWrite of mem_object * field list * (binop * c_type option) option * mexpr
	(* fst: kind of the operataion
	   snd: left hand side experssion
	   3rd: promoted type of LHS expression (before operation)
	   4th: right hand side expression *)
  | CTTMexpConditional of mexpr * mexpr * mexpr
  | CTTMexpBinExpr of binop * mexpr * mexpr
  | CTTMexpCoerce of c_type * mexpr
  | CTTMexpUnaryExpr of unaryop * mexpr
  | CTTMexpInvoke of mem_object * mexpr list
  | CTTMexpConstant of c_constants

and mem_object = 
    CTTMlvPtr of mexpr
  | CTTMlvVar of identifier * c_type
  | CTTMlvRvalue of mexpr

type cttm_initializer_desc = 
    CTTMinitExp of mexpr
  | CTTMinitList of cttm_initializer list

and cttm_initializer = cttm_initializer_desc Locterm.t

type variable_declaration = 
    local_storage_class * c_type * identifier * cttm_initializer option

type statement_desc = 
    CTTMstmtNull
  | CTTMstmtExpr of mexpr
  | CTTMstmtLabeled of identifier * statement
  | CTTMstmtCase_Labeled of big_int * statement
  | CTTMstmtDefault_Labeled of statement
  | CTTMstmtCompound of variable_declaration list * statement list
  | CTTMstmtIf of mexpr * statement * statement option
  | CTTMstmtSwitch of mexpr * statement
  | CTTMstmtWhile of mexpr * statement
  | CTTMstmtDoWhile of statement * mexpr
  | CTTMstmtFor of mexpr option * mexpr option * mexpr option * statement
  | CTTMstmtGoto of identifier
  | CTTMstmtContinue
  | CTTMstmtBreak
  | CTTMstmtReturn of mexpr option

and statement = statement_desc Locterm.t

type global_declaration_desc = 
    CTTMdeclFunction of global_storage_class * c_type * identifier * identifier list * statement
  | CTTMdeclVariable of global_storage_class * c_type * identifier * cttm_initializer option

type global_declaration = global_declaration_desc Locterm.t

type program = global_declaration list
