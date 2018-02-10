(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

open Util
open Locterm
open Il

type temp_id = int
type branch_target = int

type pair_types = 
    Value
  | Dvalue
  | PtrValue

type internal_converters = (* others *)
    Value_of_base_ofs
  | Value_of_ptrvalue
  | Ptrvalue_of_value of Ctt_abstree.c_type

  | Value_of_int
  | Dvalue_of_dword 

  | Value_of_dvalue
  | Dvalue_of_value

  | Dvalue_of_double
  | Dword_of_double
  | Double_of_dvalue
  | Double_of_dword
  | Value_of_float
  | Word_of_float
  | Float_of_value
  | Float_of_word

  | Ofs_of_value
  | Vaddr_of_base_ofs
  | Ofs_of_base_vaddr

type internal_reducible_functions = (* others *)
  | Base_remove_castflag
  | Base_put_castflag
  | Set_base_castflag of Ctt_abstree.c_type
  | Add_fat_pointer of Ctt_abstree.c_type
  | Is_cast
  | Get_realoffset_pointer
  | Get_realoffset_funcptr
  | Get_realoffset of Ctt_abstree.c_type

type internal_immobile_functions = (* not freely movable *)
  | Alloc_heapvar
  | Alloc_varargs
  | Alloc_valtempoline
  | Is_offset_ok
  | Get_boundary
  | Is_boundary_offset_ok
  | Invoke_generic_func

type internal_immobile_operations = (* not freely movable, void *)
  | Finish_varargs
  | Dealloc_heapvar
  | Dealloc_varargs_finished
  | Put_varargs
  | Put_varargs_2
  | Enter_stack_unwind_area
  | Exit_stack_unwind_area

type rw_targets = 
    Byte | Hword | Word | Dword
  | RWstruct of int | RWnone

type primitive_reducible = 
  | IL3pr_cons of pair_types
  | IL3pr_car of pair_types
  | IL3pr_cdr of pair_types
  | IL3pr_convert of internal_converters
  | IL3pr_misc of internal_reducible_functions

type 's_tempid il3_reducible_exp = 
  | IL3RexCoerce of Ctt_abstree.c_type * 's_tempid
  | IL3RexConstant of Ctt_abstree.c_constants
  | IL3RexUndefined
  | IL3RexBinop of Il.il_binop * 's_tempid * 's_tempid
  | IL3RexArgument of int
  | IL3RexAddress of 's_tempid Il.il_lvalue * field list
  | IL3RexUnaryop of Ctt_abstree.unaryop * 's_tempid

type 's_tempid il3_irreducible_exp = 
  | IL3IexInvoke of 's_tempid Il.il_lvalue * 's_tempid list
  | IL3IexBinop of Il.il_binop * 's_tempid * 's_tempid (* needed if division should be protected *)

type 's_tempid il3_desc = 
 (* compounds *)
  | IL3stmtSequence of 's_tempid il3 list
  | IL3stmtParallel of 's_tempid il3 list

 (* generated function calls *)
  | IL3stmtCallReducibleFun of temp_id * primitive_reducible * 's_tempid list
  | IL3stmtCallImmobileFun of temp_id * internal_immobile_functions * 's_tempid list
  | IL3stmtCallImmobileOp of internal_immobile_operations * 's_tempid list
  | IL3stmtCallAbort of 's_tempid * 's_tempid (** non-returning *)
  | IL3stmtCallReaderHelper of temp_id * rw_targets * 's_tempid * 's_tempid
  | IL3stmtCallWriterHelper of rw_targets * 's_tempid * 's_tempid * 's_tempid * 's_tempid option (* TODO *)

  | IL3stmtCallReducibleFunOverwriting of temp_id * primitive_reducible * 's_tempid list
	(** intentionally overwriting SSA variable by a "semantically equivalent" value.
	   currently only available for ReducibleFun. *)

 (* operations *)
  | IL3stmtMove of temp_id * Ctt_abstree.c_type * 's_tempid (* <<== direction *)
  | IL3stmtRexp of temp_id * Ctt_abstree.c_type * 's_tempid il3_reducible_exp

  | IL3stmtIexp of temp_id * Ctt_abstree.c_type * 's_tempid il3_irreducible_exp
  | IL3stmtReadRaw of temp_id * Ctt_abstree.c_type * 's_tempid Il.il_lvalue * Il.field list
  | IL3stmtWriteRaw of 's_tempid Il.il_lvalue * Il.field list * 's_tempid

 (* flow controlling *)
  | IL3stmtGoto of branch_target
  | IL3stmtIf of Il.il_if_type * 's_tempid * branch_target
  | IL3stmtSwitch of 's_tempid * ( Il.il_switch_label * branch_target ) list

  | IL3stmtReturn of 's_tempid option
  | IL3stmtConditional of
      temp_id list (* defined variables *) 
	* 's_tempid (* must be unique in function currently *) * 's_tempid il3 * 's_tempid il3
	(** IL3stmtConditional is a structured conditional inside SSA form.
	    For both branches, SSA properties should hold.
	    I.e., 
	      * Any variables should be defined once in each branch,
	        or once in one of the branches only.
	      * In the latter case, 
	        it must be used locally inside the conditional and
	        should not be used outside. *)

and 's_tempid il3_t = { il3_desc: 's_tempid il3_desc }
and 's_tempid il3 = 's_tempid il3_t Locterm.t

let make_il3 ~loc i = locput ~loc { il3_desc = i }

let enclose_il3_sequence ~loc l = make_il3 ~loc (IL3stmtSequence l)
let enclose_il3_parallel ~loc l = make_il3 ~loc (IL3stmtParallel l)

type ('s_tempid, 'add_info) il3_function =
    { body : 's_tempid il3 il_basic_block_base array;
      max_variable_number : int;

      variable_environment : Ctt_abstree.c_type il_variable_attribute_base earray;
      (* only for phi functions and original variables;
         types of temporary variables introduced in translation are
	 described in instruction *)

      prologue : Il0.il0 list; (* additional variable declarations *)

      more_info : 'add_info;
    }

type ('s_tempid, 'add_info) il3_global_declaration_desc = 
    IL3declFunction of Ctt_abstree.global_storage_class * Ctt_abstree.c_type 
	* Ctt_abstree.identifier * Ctt_abstree.identifier list
	* ('s_tempid, 'add_info) il3_function
  | IL3declVariable of 
      Ctt_abstree.global_storage_class * Ctt_abstree.c_type 
	* Ctt_abstree.identifier 
	* Cttm_abstree.cttm_initializer  (* TODO: is this OK? *)
	option

type ('s_tempid, 'add_info) il3_global_declaration = 
    ('s_tempid, 'add_info) il3_global_declaration_desc Locterm.t

(*** il3 with inlined expressions *)
type il3b_rexp = 
    IL3BTemp of temp_id
  | IL3BRfun of (primitive_reducible * il3b_rexp list) Locterm.t
  | IL3BRexp of (Ctt_abstree.c_type * il3b_rexp il3_reducible_exp) Locterm.t

(* copyed from Il2_add_block_to_top: should be unified TODO *)

let scan_for_top_goto f =
  let have_top_goto = ref false in
  let rec scan_inst i = 
    match (locval i).il3_desc with
    | IL3stmtCallReducibleFun _
    | IL3stmtCallImmobileFun _
    | IL3stmtCallImmobileOp _
    | IL3stmtCallAbort _
    | IL3stmtCallReaderHelper _
    | IL3stmtCallWriterHelper _
    | IL3stmtCallReducibleFunOverwriting _
    | IL3stmtMove _
    | IL3stmtRexp _
    | IL3stmtIexp _
    | IL3stmtReadRaw _
    | IL3stmtWriteRaw _
      -> ()

    | IL3stmtIf(_, _, gt)
    | IL3stmtGoto gt -> if gt = 0 then have_top_goto := true
	  
    | IL3stmtSwitch(_, l) ->
	List.iter (fun (_, gt) -> if gt = 0 then have_top_goto := true) l
	  
    | IL3stmtReturn _ -> ()
	  
    | IL3stmtSequence l
    | IL3stmtParallel l -> List.iter scan_inst l
    | IL3stmtConditional (vs, i, i1, i2) ->
	scan_inst i1; scan_inst i2
  in
  let scan_block b = 
    List.iter scan_inst b.Il.code
  in
  Array.iter scan_block f;
  !have_top_goto

let shift_jump_target f = 
  let rec translate_inst i = 
    let tnew = match (locval i).il3_desc with
    | IL3stmtSequence l -> IL3stmtSequence (list_map translate_inst l)
    | IL3stmtParallel l -> IL3stmtParallel (list_map translate_inst l)
    | IL3stmtConditional(vs, i, i1, i2) -> IL3stmtConditional (vs, i, translate_inst i1, translate_inst i2)

    | IL3stmtSwitch(t, l) -> IL3stmtSwitch (t, list_map (function l, gt -> l, succ gt) l)
    | IL3stmtIf(t, i, l) -> IL3stmtIf (t, i, succ l)
    | IL3stmtGoto gt -> IL3stmtGoto (succ gt)

    | IL3stmtMove _
    | IL3stmtRexp _
    | IL3stmtIexp _
    | IL3stmtReadRaw _
    | IL3stmtWriteRaw _

    | IL3stmtCallReducibleFun _
    | IL3stmtCallImmobileFun _
    | IL3stmtCallImmobileOp _
    | IL3stmtCallAbort _
    | IL3stmtCallReaderHelper _
    | IL3stmtCallWriterHelper _
    | IL3stmtCallReducibleFunOverwriting _
    | IL3stmtReturn _
	as t -> t
    in
    loccopy ~orig:i { il3_desc = tnew }
  in
  let translate_block b = 
    { 
      location = b.location;
      predecessor = list_map succ b.predecessor;
      successor = list_map succ b.successor;
      immediate_dominator = succ b.immediate_dominator;
      nest_level = b.nest_level;
      phi_function = b.phi_function;
      code = list_map translate_inst b.code;
    }
  in
  Array.map translate_block f

let add_code_to_top ~function_loc l f = 
  if l = [] then f else
  if scan_for_top_goto f then
    let top_block = { location = function_loc;
		      predecessor = []; successor = [1]; immediate_dominator = 1;
		      nest_level = 0; phi_function = []; code = l } in
    let f = shift_jump_target f in
    let b = Array.of_list (top_block :: Array.to_list f) in
    b.(1) <- { b.(1) with predecessor = 0 :: b.(1).predecessor };
    b
  else
    let f = Array.copy f in
    f.(0) <- { f.(0) with code = l @ f.(0).code };
    f
