(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

open Util
open Locterm
open Il
open Ils
open Ilc

type env_type = 
    { local_environment : (ils,ilr_type) il_function_base;
      base_usetype_info : base_var_usetype_info;
      global_environment : Ctt_abstree.environment }

let typeof_temp ~env id = 
  env.local_environment.variable_environment.(id).variable_type

let sizeof_c_type ~env t = 
  match Ctt_abstree.size_of_type ~genv:env.global_environment t with
    Some b -> Big_int.int_of_big_int b
  | None -> assert false

let sizeof_type ~env typ =
  match typ with
    ILRtypeVal(t,_)
  | ILRtypeBase(t,_)
  | ILRtypeBaseTemp(t,_)
  | ILRtypeOfs(t,_) ->
      sizeof_c_type ~env t

let sizeof_temp ~env id = sizeof_type ~env (typeof_temp ~env id)

let attr_nocheck = 
  { ilc_cattr_null = Never;
    ilc_cattr_cast = Never;
    ilc_cattr_tsize = ILCbaseOk;
  }

let always_safe brange orange = 
  match orange with
    Value_any -> false

(* [0,12], int -> [0,15] (= 12 + sizeof(int) - 1) *)
let extend_by_size range size =
  match range with 
    Value_any -> Value_any

let calculate_pointer_info ~env = function
    ILSlvPtr(base,ofs) ->
      let binfo = match typeof_temp ~env base with
	ILRtypeBase(_,binfo) -> binfo
      | _ -> assert false
      in
      let brange = binfo.ilr_base_size in
      let orange =
	match typeof_temp ~env ofs with
	  ILRtypeOfs(typ,orange) -> extend_by_size orange (sizeof_c_type ~env typ)
	| t ->
	    Format.eprintf "oops: typ=%a\n" Ils_formatter.pp_ilr_type t;
	    flush stderr;
	    assert false
      in
      let base_range_info = 
	if always_safe brange orange then ILCbaseOk
	else match brange with
	  Base_exact s -> ILCbaseConstant s
	| _ -> ILCbaseUnknown
      in
      { ilc_cattr_null = binfo.ilr_base_null;
	ilc_cattr_cast = binfo.ilr_base_cast;
	ilc_cattr_tsize = base_range_info;
      }
  | ILSlvPtrToFunc _ -> failwith "unimp"
  | ILSlvTemp _ -> attr_nocheck
  | ILSlvVar(vt,id,typ) -> attr_nocheck
  | ILSlvSVar _ -> assert false

let calculate_funcpointer_info ~env = function
    ILSlvPtrToFunc(base,ofs) ->
      let binfo = match typeof_temp ~env base with
	ILRtypeBase(_,binfo) -> binfo
      | _ -> assert false
      in
      let orange =
	match typeof_temp ~env ofs with
	  ILRtypeOfs(typ,orange) -> extend_by_size orange (sizeof_c_type ~env typ)
	| _ -> assert false
      in
      let base_range_info = 
	match orange with
	  Value_any -> ILCbaseUnknown
      in
      { ilc_cattr_null = binfo.ilr_base_null;
	ilc_cattr_cast = binfo.ilr_base_cast;
	ilc_cattr_tsize = base_range_info;
      }
  | ILSlvVar _ -> 
      attr_nocheck
  | _ -> assert false

let rec translate_exp ~env ~assigned_var exp = 
  match exp with
  | ILSexpCoerce1(typ, tid) -> ILCexpCoerce1(typ, tid)
  | ILSexpCoerce2(typ, t1, t2) -> ILCexpCoerce2(typ, t1, t2)
  | ILSexpConstant(const) -> ILCexpConstant(const)
  | ILSexpUndefined -> ILCexpUndefined
  | ILSexpBinop(binop, t1, t2) -> ILCexpBinop(binop, t1, t2)
  | ILSexpBinop21(binop, t1s, t2) -> ILCexpBinop21(binop, t1s, t2)
  | ILSexpUnaryop(uop, tid) -> ILCexpUnaryop(uop, tid)
  | ILSexpInvoke(lv, ids) ->
      let checkinfo = calculate_funcpointer_info ~env lv in
      let rettype_hint = 
	assert (assigned_var >= 0);
	Earray.get env.base_usetype_info assigned_var 
      in
      ILCexpInvoke(lv, ids, checkinfo, rettype_hint)
  | ILSexpAddress(lv, fields) -> ILCexpAddress(lv, fields)
  | ILSexpArgument(n) -> ILCexpArgument(n)
  | ILSexpArgumentV(n) -> ILCexpArgumentV(n)
  | ILSexpArgumentB(n) -> ILCexpArgumentB(n)
  | ILSexpIdent(tid) -> ILCexpIdent(tid)

let rec translate_stmt ~env stmt = 
  let nstmt = match locval stmt with
    ILSstmtIf(iftyp,tid,target) -> ILCstmtIf(iftyp,tid,target)
  | ILSstmtSwitch(tid,targets) -> ILCstmtSwitch(tid,targets)
  | ILSstmtGoto target -> ILCstmtGoto target
  | ILSstmtReturn0 -> ILCstmtReturn0
  | ILSstmtAbort err -> ILCstmtAbort err
  | ILSstmtReturn1 tid -> ILCstmtReturn1 tid
  | ILSstmtReturn2 (t1,t2) -> ILCstmtReturn2 (t1,t2)
  | ILSstmtAssign (tid,exp) -> ILCstmtAssign (tid,translate_exp ~env ~assigned_var:tid exp)
  | ILSstmtAssign2 (t1,t2,exp) -> ILCstmtAssign2 (t1,t2,translate_exp ~env ~assigned_var:t1 exp)
  | ILSstmtRead1 (tid,lv,fields) -> 
      let info = calculate_pointer_info ~env lv in
      ILCstmtRead1 (tid,lv,fields,info)
  | ILSstmtRead2 (t1,t2,lv,fields) -> 
      let info = calculate_pointer_info ~env lv in
      ILCstmtRead2 (t1,t2,lv,fields,info)
  | ILSstmtWrite1 (lv,fields,tid) ->
      let info = calculate_pointer_info ~env lv in
      ILCstmtWrite1 (lv,fields,tid,info)
  | ILSstmtWrite2 (lv,fields,t1,t2) -> 
      let info = calculate_pointer_info ~env lv in
      ILCstmtWrite2 (lv,fields,t1,t2,info)
  | ILSstmtDeclScalar(vt,ct,id) -> ILCstmtDeclScalar(vt,ct,id)
  | ILSstmtDeclBulk(vt,ct,id) -> ILCstmtDeclBulk(vt,ct,id)
  | ILSstmtInitialize (vt,ct,id,init) -> ILCstmtInitialize (vt,ct,id,init)
  | ILSstmtSequence instrs ->
      ILCstmtSequence (list_map (translate_stmt ~env) instrs)
  | ILSstmtParallel instrs ->
      ILCstmtParallel (list_map (translate_stmt ~env) instrs)

  | ILSstmtAbortIf(exp,err) -> ILCstmtAbortIf(translate_exp ~env ~assigned_var:(-1) exp,err)
  in
  loccopy ~orig:stmt nstmt

let translate_block ~env { location = location;
			   predecessor = predecessor;
			   successor = successor;
			   immediate_dominator = immediate_dominator;
			   nest_level = nest_level;
			   phi_function = phi_function;
			   code = code } = 
  { location = location;
    predecessor = predecessor;
    successor = successor;
    immediate_dominator = immediate_dominator;
    nest_level = nest_level;
    phi_function = phi_function;
    code = list_map (translate_stmt ~env) code }
    
let translate_body ~env = Array.map (translate_block ~env)

let translate_function ~genv 
    ({ body = body; max_variable_number = max_variable_number;
       variable_environment = variable_environment; arguments = arguments } as f, m) = 
  let env = { local_environment = f; base_usetype_info = m; global_environment = genv } in
  { f with body = translate_body ~env body }

let f ~genv = 
  locmap_list
    (function
	ILRdeclVariable (gsc,t,id,init) -> ILdeclVariable(gsc,t,id,init)
      | ILRdeclFunction (gsc,t,id,arg_id,func) ->
	  ILdeclFunction (gsc,t,id,arg_id,translate_function ~genv func))

let dump_program ~genv = 
  List.iter (Ilc_formatter.pp_ilc_global_declaration Format.err_formatter)
