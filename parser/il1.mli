(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2006. *)

(** IL1: intermediate language with basic blocks *)

open Big_int
open Ctt_abstree
open Cttm_abstree
open Set_list

type temp_id = int

type identifier = string

and field = identifier * c_type

(* expression part is now only in module Il *)

type il1_desc =
(* basically from statement *)
  | IL1stmtDeclAutoScalar of local_storage_class * c_type * identifier * temp_id option
  | IL1stmtDeclBulk of local_storage_class * c_type * identifier * Il.il_initializer option
  | IL1stmtIf of Il.il_if_type * temp_id * int
  | IL1stmtSwitch of temp_id * ( Il.il_switch_label * int ) list
  | IL1stmtGoto of int
  | IL1stmtReturn of temp_id option
  | IL1stmtAbort of Il.abort_reason
(* basically from expression *)
  | IL1stmtDefTemp of temp_id * c_type * Il.il_expr
  | IL1stmtReadToTemp of temp_id * c_type * temp_id Il.il_lvalue * field list
  | IL1stmtWrite of temp_id Il.il_lvalue * field list * temp_id
  | IL1stmtSequence of il1 list
  | IL1stmtParallel of il1 list (* Data-flow parallel *)

and il1_t = 
    { il1_depends : Int_set.t;
      il1_defines : Int_set.t;
      il1_t : il1_desc; }

and il1 = il1_t Locterm.t

type il1_basic_block =
    {
     location : Locterm.location;
     predecessor : int list;
     successor : int list;
     code : il1 list
   }

type il1_function_body = il1_basic_block array

type il1_global_declaration_desc = 
    IL1declFunction of global_storage_class * c_type 
	* identifier * identifier list * il1_function_body
  | IL1declVariable of 
      global_storage_class * c_type * identifier * Il.il_initializer option

type il1_global_declaration = 
    il1_global_declaration_desc Locterm.t
