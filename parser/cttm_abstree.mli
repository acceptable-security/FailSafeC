(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** CTTM: C typed tree with pointer-indirect assignment semantics *)

type binop = Ctt_abstree.binop
type unaryop = Ctt_abstree.unaryop
type field = Ctt_abstree.identifier * Ctt_abstree.c_type

type c_constants = Ctt_abstree.c_constants

type mexpr_t = { mexpr_type : Ctt_abstree.c_type; mexpr_t : mexpr_desc; }
and mexpr = mexpr_t Locterm.t
and mexpr_desc =
    CTTMexpComma of mexpr * mexpr
  | CTTMexpAddress of mem_object * field list
  | CTTMexpRead of mem_object * field list
  | CTTMexpWrite of mem_object * field list *
      (binop * Ctt_abstree.c_type option) option * mexpr
  | CTTMexpConditional of mexpr * mexpr * mexpr
  | CTTMexpBinExpr of binop * mexpr * mexpr
  | CTTMexpCoerce of Ctt_abstree.c_type * mexpr
  | CTTMexpUnaryExpr of unaryop * mexpr
  | CTTMexpInvoke of mem_object * mexpr list
  | CTTMexpConstant of c_constants
and mem_object =
    CTTMlvPtr of mexpr
  | CTTMlvVar of Ctt_abstree.identifier * Ctt_abstree.c_type
  | CTTMlvRvalue of mexpr

type cttm_initializer_desc =
    CTTMinitExp of mexpr
  | CTTMinitList of cttm_initializer list
and cttm_initializer = cttm_initializer_desc Locterm.t

type variable_declaration =
    Ctt_abstree.local_storage_class * Ctt_abstree.c_type *
    Ctt_abstree.identifier * cttm_initializer option
type statement_desc =
    CTTMstmtNull
  | CTTMstmtExpr of mexpr
  | CTTMstmtLabeled of Ctt_abstree.identifier * statement
  | CTTMstmtCase_Labeled of Big_int.big_int * statement
  | CTTMstmtDefault_Labeled of statement
  | CTTMstmtCompound of variable_declaration list * statement list
  | CTTMstmtIf of mexpr * statement * statement option
  | CTTMstmtSwitch of mexpr * statement
  | CTTMstmtWhile of mexpr * statement
  | CTTMstmtDoWhile of statement * mexpr
  | CTTMstmtFor of mexpr option * mexpr option * mexpr option * statement
  | CTTMstmtGoto of Ctt_abstree.identifier
  | CTTMstmtContinue
  | CTTMstmtBreak
  | CTTMstmtReturn of mexpr option
and statement = statement_desc Locterm.t
type global_declaration_desc =
    CTTMdeclFunction of Ctt_abstree.global_storage_class *
      Ctt_abstree.c_type * Ctt_abstree.identifier *
      Ctt_abstree.identifier list * statement
  | CTTMdeclVariable of Ctt_abstree.global_storage_class *
      Ctt_abstree.c_type * Ctt_abstree.identifier * cttm_initializer option
type global_declaration = global_declaration_desc Locterm.t
type program = global_declaration list
