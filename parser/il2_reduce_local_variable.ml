(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

(* reduce (or promote) local variables to temporary variables. *)

(* If there is a call to setjmp function, 
   all local variables must be "volatile" and should not be reduced. *)
   
open Util
open Locterm
open Ctt_abstree
open Il
open Il2

include Debug.Install (struct let category = 55 end)

type reduction_parameter = { is_setjmp : identifier -> bool; forced_stack_alloc : bool; }
      
let default_reduction_parameter = 
  { is_setjmp = (fun i -> i = "setjmp" || i = "sigsetjmp"); 
    forced_stack_alloc = false;
  }
    
exception Found
    
let is_function_type t = 
  match t.ct_ty with Tfunction _ -> true | _ -> false

(**************** looking for setjmp ****************)

let scan_for_setjmp ~param f =
  let result = ref false in
  let scan_lv = function
      IL2lvPtr _
    | IL2lvTemp _ -> false
    | IL2lvVar(v, i, t) ->
	v = GlobalVar && is_function_type t && param.is_setjmp i
	  
  in
  let rec scan_exp = function
    | IL2expCoerce _
    | IL2expConstant _
    | IL2expUndefined
    | IL2expBinop _
    | IL2expUnaryop _
    | IL2expArgument _
    | IL2expIdent _ -> ()

    | IL2expInvoke(lv, _args) ->
	if scan_lv lv then result := true

    | IL2expAddress(lv, fld) ->
	if scan_lv lv then
	  failwith "il2_reduce_local_variable: taking address of setjmp function: cannot handle safely"
	else ()
  in
  let rec scan_cexp c = 
    match (locval c).il2_cexp_t with
      IL2cexpCoerce(_, c1) -> scan_cexp c1
    | IL2cexpConstant _ -> ()
    | IL2cexpBinop(_, c1, c2) -> scan_cexp c1; scan_cexp c2
    | IL2cexpUnaryop (_, c1) -> scan_cexp c1
    | IL2cexpAddress(lv, _) ->
	if scan_lv lv then
	  failwith "il2_reduce_local_variable: taking address of setjmp function: cannot handle safely (2)"
    | IL2cexpAddFieldOfs(c1, _) -> scan_cexp c1
  in
  let rec scan_init i = 
    match locval i with
      IL2initConstantExp(_, c) -> scan_cexp c
    | IL2initPostFill -> ()
    | IL2initStruct(_, l) -> List.iter (fun (_,i) -> scan_init i) l
    | IL2initArray(_, l) -> List.iter scan_init l
  in
  let rec scan_inst i = 
    match (locval i).il2_t with
      IL2stmtDeclAutoScalar _
    | IL2stmtIf _
    | IL2stmtSwitch _
    | IL2stmtGoto _
    | IL2stmtReturn _
    | IL2stmtAbort _
    | IL2stmtRead _
    | IL2stmtWrite _ -> ()

    | IL2stmtSequence l
    | IL2stmtParallel l -> List.iter scan_inst l

    | IL2stmtDeclBulk(_, _, _, iopt) ->
	Option.iter scan_init iopt

    | IL2stmtAssign (_tid, _cty, e) ->
	scan_exp e
  in
  let scan_block b = 
    List.iter scan_inst b.code
  in
  Array.iter scan_block f;
  !result

(**************** scanning reducible variables ****************)

type environment = 
    { max_variable_number : int;
      num_reducibles : int;
      name_table : (identifier, (temp_id * c_type)) Hashtbl.t }

let phase1 f = 
  let mvn = ref 0 in
  let num_rval = ref 0 in
  let tbl = Hashtbl.create 16 in

  let scan_used_tid t = 
    if !mvn < t then mvn := t
  in

  let allocate_variable v t = 
    if not (Hashtbl.mem tbl v) then begin
      let new_id = (incr num_rval; !num_rval) in
      dprintf 9 "reduce_local: assigning (+%d) for %s" new_id v;
      Hashtbl.add tbl v (new_id, t)
    end
    else
      ()
  in
  
  let rec scan_inst i = 
    match (locval i).il2_t with
      IL2stmtDeclAutoScalar(vt, ct, id, topt) ->
	if vt = RegVar then allocate_variable id ct
	else
	  dprintf 9 "reduce_local: not assigning for %s" id;
    | IL2stmtDeclBulk(vt, _, _, iopt) ->
	assert (vt <> RegVar); ()

    | IL2stmtRead(tid, _, _, _) -> scan_used_tid tid 
    | IL2stmtAssign (tid, _, _) -> scan_used_tid tid

    | IL2stmtIf _
    | IL2stmtGoto _
    | IL2stmtSwitch _
    | IL2stmtReturn _
    | IL2stmtAbort _
    | IL2stmtWrite _ -> ()

    | IL2stmtSequence l
    | IL2stmtParallel l -> List.iter scan_inst l

  in
  let scan_block b = 
    List.iter scan_inst b.code
  in
  Array.iter scan_block f.body;
  { max_variable_number = !mvn;
    num_reducibles = !num_rval;
    name_table = tbl }

(**************** translate function ****************)

let reduce_function ~function_loc ~env f = 
  let assign_new_id id =
    let idd, t = Hashtbl.find env.name_table id in
    idd + env.max_variable_number, t
  in

  let rec translate_inst i = 
    let tnew = match (locval i).il2_t with
    | IL2stmtIf _
    | IL2stmtSwitch _
    | IL2stmtGoto _
    | IL2stmtReturn _
    | IL2stmtAbort _
    | IL2stmtAssign _ as t -> t

    | IL2stmtDeclAutoScalar(vt, ct, id, iopt) as t -> begin
	if vt <> RegVar then
	  t
	else 
	  let new_tid, _ = assign_new_id id in
	  match iopt with
	    None -> IL2stmtAssign (new_tid, ct, IL2expUndefined)
	  | Some i -> IL2stmtAssign (new_tid, ct, IL2expIdent i)
    end
    | IL2stmtDeclBulk(_vt, _, _, iopt) as t ->
	assert (_vt <> RegVar); t
	  
    | IL2stmtRead(tid, cty, lv, fl) as t -> begin
	match lv, fl with
	  IL2lvPtr _, _ 
	| IL2lvTemp _, _
	| IL2lvVar((StackVar | HeapVar | GlobalVar), _, _), _ -> t

	| IL2lvVar(RegVar, id, ct), [] ->
	    let new_tid, _ = assign_new_id id in
	    IL2stmtAssign(tid, cty, IL2expIdent new_tid)
	      
	| IL2lvVar(RegVar, _, _), _::_ -> assert false
    end
    | IL2stmtWrite(lv, fld, tid) as t -> begin
	match lv, fld with
	  IL2lvPtr _, _ 
	| IL2lvTemp _, _
	| IL2lvVar((StackVar | HeapVar | GlobalVar), _, _), _ -> t

	| IL2lvVar(RegVar, id, ct), [] ->
	    let new_tid, _ = assign_new_id id in
	    IL2stmtAssign(new_tid, ct, IL2expIdent tid)
	| IL2lvVar(RegVar, _, _), _::_ -> assert false
    end
    | IL2stmtSequence l -> IL2stmtSequence (list_map translate_inst l)
    | IL2stmtParallel l -> IL2stmtParallel (list_map translate_inst l)
    in
    loccopy ~orig:i { il2_t = tnew }
  in
  let translate_block b = 
    { location = b.location;
      code = list_map translate_inst b.code;
      predecessor = b.predecessor;
      successor = b.successor
    }
  in

  let new_blocks = Array.map translate_block f.body in

  let top_insns = 
    let g = Glist.empty () in
    Hashtbl.iter
      (fun id _ ->
	let new_tid, ct = assign_new_id id in
	Glist.put g (locput ~loc:function_loc { il2_t = IL2stmtAssign (new_tid, ct, IL2expUndefined) }))
      env.name_table;
    Glist.to_list g
  in

  assert (Array.length f.variable_environment = env.max_variable_number + 1);
  let new_varenv = 
    let addenv = 
      let a = 
	Array.create 
	  env.num_reducibles { original_name = None } 
      in
      Hashtbl.iter
	(fun id (tid, ct) ->
	  dprintf 9 "preparing %d (%d,%d) from %s" (tid + env.max_variable_number) env.max_variable_number tid id;
	  a.(tid - 1) <- { original_name = Some id })
	env.name_table;
      a
    in
    let a = 
      Array.concat
	[ f.variable_environment; addenv ]
    in
    Array.iteri
      (fun i v ->
	dprintf 9 "%d from %s" i (Option.default "-" v.original_name)) a;
    a
  in
  let new_f = { f with body = new_blocks; variable_environment = new_varenv } in
  Il2_add_block_to_top.add_code ~function_loc top_insns new_f

(**************** "demoted" version of translation: prepare for setjmp ****************)

let prepare_function_for_setjmp f = 
  let rec volatilify ct = 
    match ct.ct_ty with
      Tarray(et,l) ->
	{ ct with ct_ty = Tarray(volatilify et,l); ct_volatile_p = true }
    | _ -> { ct with ct_volatile_p = true }
  in

  let translate_lv lv = 
    match lv with
      IL2lvPtr _
    | IL2lvTemp _ -> lv
    | IL2lvVar((StackVar | HeapVar | GlobalVar) as t, lv, ct) ->
	IL2lvVar(t, lv, volatilify ct)
    | IL2lvVar(RegVar, id, ct) -> IL2lvVar (StackVar, id, volatilify ct)
  in

  let rec translate_exp = function
    | IL2expCoerce _
    | IL2expConstant _
    | IL2expUndefined
    | IL2expBinop _
    | IL2expUnaryop _
    | IL2expArgument _
    | IL2expIdent _ as e -> e

    | IL2expInvoke(lv, args) ->
	IL2expInvoke(translate_lv lv, args)

    | IL2expAddress(lv, fld) ->
	IL2expAddress(translate_lv lv, fld)
  in

  let rec translate_inst i = 
    let tnew = match (locval i).il2_t with
    | IL2stmtIf _
    | IL2stmtSwitch _
    | IL2stmtGoto _
    | IL2stmtReturn _
    | IL2stmtAbort _  as t -> t
    | IL2stmtAssign(lv, ct, e) -> IL2stmtAssign (lv, ct, translate_exp e)

    | IL2stmtDeclAutoScalar(vt, ct, id, iopt) ->
	let vt = if vt = RegVar then StackVar else vt in
	let ct = volatilify ct in
	IL2stmtDeclAutoScalar(vt, ct, id, iopt)

    | IL2stmtDeclBulk(vt, ct, id, iopt) ->
	assert (vt <> RegVar); 
	let ct = volatilify ct in
	IL2stmtDeclBulk(vt, ct, id, iopt)
	  
    | IL2stmtRead(tid, cty, lv, fl) -> 
	IL2stmtRead(tid, cty, translate_lv lv, fl)

    | IL2stmtWrite(lv, fld, tid) ->
	IL2stmtWrite(translate_lv lv, fld, tid)

    | IL2stmtSequence l -> IL2stmtSequence (list_map translate_inst l)
    | IL2stmtParallel l -> IL2stmtParallel (list_map translate_inst l)
    in
    loccopy ~orig:i { il2_t = tnew }
  in
  let translate_block b = 
    { location = b.location;
      code = list_map translate_inst b.code;
      predecessor = b.predecessor;
      successor = b.successor
    }
  in

  let new_blocks = Array.map translate_block f.body in
  { f with body = new_blocks }

let translate_function ~function_loc ~param f = 
  let setjmp_found = scan_for_setjmp ~param f.body
      (* scan first, because it may reject some invalid code by raising exception *)
  in
  let attr = if setjmp_found then { Il.setjmp_called = true } else { Il.setjmp_called = false } in
  let f = { f with function_attributes = attr } in
  if setjmp_found || param.forced_stack_alloc then
    (* all variables will be allocated on stack *)
    prepare_function_for_setjmp f
  else
    let env = phase1 f in
    if env.num_reducibles = 0 then { f with function_attributes = attr }
    else reduce_function ~function_loc ~env { f with function_attributes = attr }

let translate_gdecl ~genv ?(param = default_reduction_parameter) gd = 
  loccopy ~orig:gd
  (match locval gd with
    IL2declFunction(gs, ct, id, ids, f) ->
      let f = translate_function ~function_loc:(locget gd) ~param f in
      IL2declFunction(gs, ct, id, ids, f)
  | IL2declVariable(gs, ct, id, initopt)
    ->  IL2declVariable(gs, ct, id, initopt))

let translate_program ~genv ?param p = 
  list_map (translate_gdecl ~genv ?param) p
  
