(*
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

(* Classify local variables for stack/heap allocation *)
(*   additionally, all arguments are mapped to local variables. *)

(* TODO: do escape analysis to detect more stack-allocatable variables *)

open Ctt_abstree
open Locterm
open Il
open Il1

type environment =
    { genv: Ctt_abstree.environment;
      local_variables: (string, unit) Hashtbl.t;
      onmemory_variables: (string, unit) Hashtbl.t;
      longlife_variables: (string, unit) Hashtbl.t;
      mutable max_variable_no : int;
    }

let register_local_variable ~env id =
  Hashtbl.replace env.local_variables id ()

let require_onmemory ~env id =
  Hashtbl.replace env.onmemory_variables id ()

let require_longlife ~env id =
  Hashtbl.replace env.longlife_variables id ()

let rec analyze_cexp ~env c =
  match (locval c).il_cexp_t with
    ILcexpCoerce (_, c1) -> analyze_cexp ~env c1
  | ILcexpConstant _ -> ()
  | ILcexpBinop (_, c1, c2) -> analyze_cexp ~env c1; analyze_cexp ~env c2
  | ILcexpUnaryop (_, c1) -> analyze_cexp ~env c1
  | ILcexpAddress (ILlvPtr _, _) -> assert false
  | ILcexpAddress (ILlvTemp _, _) -> assert false
  | ILcexpAddress (ILlvVar (id, _), _) ->
      require_onmemory ~env id;
      require_longlife ~env id
  | ILcexpAddFieldOfs (c1, _) -> analyze_cexp ~env c1

let rec analyze_initializer ~env i =
  match locval i with
    ILinitConstantExp (_ct, cexp) -> analyze_cexp ~env cexp
  | ILinitPostFill -> ()
  | ILinitStruct (_, l) ->
      List.iter (fun (_, i) -> analyze_initializer ~env i) l
  | ILinitArray (_, l) ->
      List.iter (analyze_initializer ~env) l
  | ILinitAbstractCtt _ -> assert false

let analyze_exp ~env e =
  match e with
    ILexpCoerce _ -> ()
  | ILexpConstant _ -> ()
  | ILexpUndefined -> ()
  | ILexpBinop _ -> ()
  | ILexpUnaryop _ -> ()
  | ILexpInvoke _ -> ()
  | ILexpAddress (ILlvPtr _, _) -> ()
  | ILexpAddress (ILlvTemp _, _) -> ()
  | ILexpAddress (ILlvVar (id, ct), _) ->
      require_onmemory ~env id;
      require_longlife ~env id;
  | ILexpArgument _ -> ()
  | ILexpIdent _ -> ()

let is_atomic_type t =
  match t.ct_ty with
    Tbuiltin _ | Tpointer _ -> true
  | Tarray _ | Tstruct _ -> false
  | Tfunction _ | Tvoid | Tabstract _ -> failwith "is_atomic_type"

let rec analyze_stmt ~env il =
  match (locval il).il1_t with
    IL1stmtDeclAutoScalar(_sc, ct, id, _initopt) ->
      let is_atomic = is_atomic_type ct in
      assert is_atomic;
      register_local_variable ~env id;
      ( (* initializer is temp_id, no need to handle *) );
  | IL1stmtDeclBulk (_sc, ct, id, initopt) ->
      let is_atomic = is_atomic_type ct in
      assert (not is_atomic);
      register_local_variable ~env id;
      require_onmemory ~env id;
      Option.iter (analyze_initializer ~env) initopt;
  | IL1stmtIf _ -> ()
  | IL1stmtSwitch _ -> ()
  | IL1stmtGoto _ -> ()
  | IL1stmtReturn _ -> ()
  | IL1stmtAbort _ -> ()
  | IL1stmtDefTemp(tid, _cty, e) -> 
      env.max_variable_no <- max env.max_variable_no tid; analyze_exp ~env e
  | IL1stmtReadToTemp(tid, _cty, _lv, _flds) ->
      env.max_variable_no <- max env.max_variable_no tid
  | IL1stmtWrite(_lv, _flds, _tid) -> ()
  | IL1stmtSequence l -> List.iter (analyze_stmt ~env) l
  | IL1stmtParallel l -> List.iter (analyze_stmt ~env) l

open Il2

let classify_variable ~env id =
  if not (Hashtbl.mem env.local_variables id) then
    GlobalVar
  else if Hashtbl.mem env.longlife_variables id then
    HeapVar
  else if Hashtbl.mem env.onmemory_variables id then
    StackVar
  else
    RegVar

let translate_lv ~env lv =
  match lv with
    ILlvPtr id -> IL2lvPtr id
  | ILlvVar (id, ct) -> IL2lvVar (classify_variable ~env id, id, ct)
  | ILlvTemp id -> IL2lvTemp id

let rec translate_cexp ~env ce =
  let desc = match (locval ce).il_cexp_t with
    ILcexpCoerce(ct, c1) -> IL2cexpCoerce(ct, translate_cexp ~env c1)
  | ILcexpConstant c -> IL2cexpConstant c
  | ILcexpBinop(b, c1, c2) -> IL2cexpBinop(b, translate_cexp ~env c1, translate_cexp ~env c2)
  | ILcexpUnaryop(u, c1) -> IL2cexpUnaryop(u, translate_cexp ~env c1)
  | ILcexpAddress(lv, fl) -> IL2cexpAddress(translate_lv ~env lv, fl)
  | ILcexpAddFieldOfs(c1, fl) -> IL2cexpAddFieldOfs(translate_cexp ~env c1, fl)
  in
  loccopy ~orig:ce { il2_cexp_t = desc; il2_cexp_type = (locval ce).il_cexp_type }

let translate_exp ~env il1 =
  let e = match il1 with
  | ILexpCoerce(ct, t1)   -> IL2expCoerce(ct, t1)
  | ILexpConstant c	   -> IL2expConstant c
  | ILexpUndefined	   -> IL2expUndefined
  | ILexpBinop(b, t1, t2) -> IL2expBinop(b, t1, t2)
  | ILexpUnaryop(u, t1)   -> IL2expUnaryop(u, t1)
  | ILexpInvoke(lv, tl)   -> IL2expInvoke(translate_lv ~env lv, tl)
  | ILexpAddress(lv, fl)  -> IL2expAddress(translate_lv ~env lv, fl)
  | ILexpArgument a	   -> IL2expArgument a
  | ILexpIdent t1         -> IL2expIdent t1
  in e

let rec translate_initializer ~env i =
  locmap (function
    ILinitConstantExp (ct, ce) -> IL2initConstantExp (ct, translate_cexp ~env ce)
  | ILinitPostFill	        -> IL2initPostFill
  | ILinitStruct (ct, l)
    -> IL2initStruct (ct, Util.list_map (fun (id, i) -> id, translate_initializer ~env i) l)
  | ILinitArray (ct, l)
    -> IL2initArray (ct, Util.list_map (translate_initializer ~env) l)
  | ILinitAbstractCtt _ -> assert false)
    i

let rec translate_stmt ~env i =
  let desc = match (locval i).il1_t with
  | IL1stmtDeclAutoScalar (ls, ct, id, iopt)-> 
      let ls = classify_variable ~env id in
      IL2stmtDeclAutoScalar (ls, ct, id, iopt)
  | IL1stmtDeclBulk (ls, ct, id, initopt)   ->
      let ls = classify_variable ~env id in
      IL2stmtDeclBulk (ls, ct, id, Option.map (translate_initializer ~env) initopt)
  | IL1stmtIf (ift, t, b)		    -> IL2stmtIf (ift, t, b)
  | IL1stmtSwitch (t, sl)		    -> IL2stmtSwitch (t, sl)
  | IL1stmtGoto b			    -> IL2stmtGoto b
  | IL1stmtReturn topt			    -> IL2stmtReturn topt
  | IL1stmtAbort reason			    -> IL2stmtAbort reason
  | IL1stmtDefTemp (t, ct, e)		    -> IL2stmtAssign (t, ct, translate_exp ~env e)
  | IL1stmtReadToTemp (t, ct, lv, fld)	    -> IL2stmtRead (t, ct, translate_lv ~env lv, fld)
  | IL1stmtWrite (lv, fld, t)		    -> IL2stmtWrite (translate_lv ~env lv, fld, t)
  | IL1stmtSequence il			    -> IL2stmtSequence (Util.list_map (translate_stmt ~env) il)
  | IL1stmtParallel il                      -> IL2stmtParallel (Util.list_map (translate_stmt ~env) il)
  in
  loccopy ~orig:i
    { (* il2_depends = i.il1_depends;
	 il2_defines = i.il1_defines; *)
      il2_t = desc }

let translate_basic_block ~env b = 
  { Il2.location = b.Il1.location;
    Il2.code = Util.list_map (translate_stmt ~env) b.Il1.code;
    Il2.predecessor = b.Il1.predecessor;
    Il2.successor = b.Il1.successor }

let translate_function ~genv ~function_loc ct ids f = 
  let env = 
    { genv = genv;
      local_variables = Hashtbl.create 16;
      onmemory_variables = Hashtbl.create 16;
      longlife_variables = Hashtbl.create 16;
      max_variable_no = 0
    }
  in
  let argtypes = match ct.ct_ty with Tfunction(at,vt,rt) -> at | _ -> assert false in
  let args = List.combine ids argtypes in

  List.iter
    (fun (id, ct) ->
      register_local_variable ~env id;
      if not (is_atomic_type ct) then
	require_onmemory ~env id)
    args;

  Array.iter
    (fun b -> List.iter (analyze_stmt ~env) b.Il1.code) f;
  let fb = Array.map (translate_basic_block ~env) f in

  (* add argument -> variable copy *)
  let top_insns = 
    Util.map_flatten_i
      (fun i (id, ct) ->
	let ct = { ct with ct_const_p = false } in
	let vt = classify_variable ~env id in
	let tid = (env.max_variable_no <- succ env.max_variable_no; env.max_variable_no) in
	locput ~loc:function_loc { il2_t = IL2stmtAssign(tid, ct, IL2expArgument i) } ::
	(if not (is_atomic_type ct) then
	  [ locput ~loc:function_loc { il2_t = IL2stmtDeclBulk(vt, ct, id, None) };
	    locput ~loc:function_loc { il2_t = IL2stmtWrite((IL2lvVar(vt, id, ct)), [], tid) };
	 ]
	else
	  [ locput ~loc:function_loc { il2_t = IL2stmtDeclAutoScalar(vt, ct, id, Some tid) }]))
      args
  in
  Il2_add_block_to_top.add_code ~function_loc top_insns 
    { body = fb; 
      variable_environment = Array.create (env.max_variable_no + 1) { Il2.original_name = None }; 
      function_attributes = { Il.setjmp_called = true } }

let translate_global_initializer ~genv g = 
  let env = 
    (* dummy environment: all variables are global *)
    { genv = genv;
      local_variables = Hashtbl.create 1;
      onmemory_variables = Hashtbl.create 1;
      longlife_variables = Hashtbl.create 1;
      max_variable_no = 0;
    }
  in
  translate_initializer ~env g

let translate_gdecl ~genv gd =
  loccopy ~orig:gd
  (match locval gd with
    IL1declFunction(gs, ct, id, ids, f)
    -> IL2declFunction(gs, ct, id, ids, translate_function ~function_loc:(locget gd) ~genv ct ids f)
	(* for a safe bid: setjmp analysis is in next stage *)
  | IL1declVariable(gs, ct, id, initopt)
    ->  IL2declVariable(gs, ct, id, Option.map (translate_global_initializer ~genv) initopt))

let translate_program ~genv p = 
  Util.list_map (translate_gdecl ~genv) p
