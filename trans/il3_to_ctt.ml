(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2003-2009. *)

open Util
open Locterm
open Printf
open Big_int
open Il
open Il0
open Il3
open Il3_constants
open Il3_decompose_ssa
open Ctt_abstree
open Cttm_abstree

include Debug.Install (struct let category = 170 end)

type translate_environment = 
    {
     func_name : string;
     argnames : string list;
     argtypes : c_type list;
     additional_decls : (local_storage_class * c_type * identifier * ctt_initializer option) Glist.t;

     variable_environment : c_type il_variable_attribute_base earray;
     genv : Ctt_abstree.environment;

     f : (il3b_rexp, (Il3_optimize.cogen_decisions * block_phi_info array)) il3_function; 
     blocks : il3b_rexp il3 il_basic_block_base array;
     n_blocks : int;
     cogen_hint : Il3_optimize.cogen_decisions;
     phi_info : block_phi_info array;
     tempid_generated : bool earray;
   }

let make_mexpr ~loc ty e = 
  Locterm.locput ~loc { mexpr_type = ty; mexpr_t = e }

let make_expr ~loc e typ = 
  Locterm.locput ~loc { expr_t = e; expr_type = typ }

let make_stmt ~loc d = Locterm.locput ~loc d

let label_direct x = "LB" ^ string_of_int x
let label_trampoline from target = "LT" ^ string_of_int from ^ "_" ^ string_of_int target

let make_branch ~env ~loc ~current_block ?(last = false) target =
  let target = 
    if List.mem_assoc current_block env.phi_info.(target).trampoline_blocks
    then
      label_trampoline current_block target
    else if
      Option.map (fun (t, _) -> t = target)
	(env.phi_info.(current_block).phi_assignments_at_bottom) 
	= Some true && not last
    then
      label_trampoline current_block target
    else
      label_direct target
  in
  make_stmt ~loc (CTTstmtGoto target)

let make_labelled ~loc l = function
    [] -> [make_stmt ~loc (CTTstmtLabeled (l, make_stmt ~loc CTTstmtNull))]
  | [i] -> [make_stmt ~loc (CTTstmtLabeled (l, i))]
  | i1::is -> make_stmt ~loc (CTTstmtLabeled (l, i1))::is

let translate_binop = function
    ILbinTimes     -> CTTbinTimes
  | ILbinDiv       -> CTTbinDiv
  | ILbinPlusVV    -> CTTbinPlusVV
  | ILbinMinusVV   -> CTTbinMinusVV
  | ILbinPlusPV    -> CTTbinPlusPV
  | ILbinMinusPP   -> CTTbinMinusPP
  | ILbinMinusPV   -> CTTbinMinusPV
  | ILbinModulo    -> CTTbinModulo
  | ILbinLshift    -> CTTbinLshift
  | ILbinRshift    -> CTTbinRshift
  | ILbinLogAnd    -> CTTbinLogAnd
  | ILbinLogOr     -> CTTbinLogOr
  | ILbinIntAnd    -> CTTbinIntAnd
  | ILbinIntOr     -> CTTbinIntOr
  | ILbinIntXor    -> CTTbinIntXor
  | ILbinLessThan  -> CTTbinLessThan
  | ILbinLessEqual -> CTTbinLessEqual
  | ILbinGtrThan   -> CTTbinGtrThan
  | ILbinGtrEqual  -> CTTbinGtrEqual
  | ILbinEqual     -> CTTbinEqual
  | ILbinNotEqual  -> CTTbinNotEqual

let translate_arg ~env i = 
  let name = List.nth env.argnames i
  and ty = List.nth env.argtypes i
  in
  CTTMexpRead(CTTMlvVar(name, ty),[])
  
let type_of_id ~env id = (Earray.get env.variable_environment id).variable_type

let get_variable ~env id = 
  let name = 
    match (Earray.get env.variable_environment id).original_name with
      None -> "T" ^ string_of_int id
    | Some n -> "T" ^ string_of_int id ^ "_" ^ n
  in
  name, type_of_id ~env id

let cttm_to_ctt e = 
  Cttm_to_ctt.translate_expr e

let assigned ~env id = 
  Earray.set env.tempid_generated id true

let make_assign ~env ~loc id exp = 
  let v, typ = get_variable ~env id in
  let idv = CTTexpVar(v, typ) in
  let assign_exp = 
    if typ.ct_ty = Tvoid then
      cttm_to_ctt exp
    else if (Earray.get env.cogen_hint id) = Il3_optimize.Omit then
      cttm_to_ctt exp
    else begin
      assigned ~env id;
      (make_expr ~loc
	 (CTTexpAssign(make_expr ~loc idv typ, cttm_to_ctt exp))
	 typ)
    end
  in
  make_stmt ~loc (CTTstmtExpr assign_exp)

let make_voidstmt ~env ~loc exp = 
  make_stmt ~loc (CTTstmtExpr (cttm_to_ctt exp))

let rec translate_lv ~env ~loc = function
    ILlvPtr(v) ->
      CTTMlvPtr (translate_rexp ~env ~loc v)
  | ILlvVar(id, typ) ->
      CTTMlvVar(id, typ)
  | ILlvTemp(v) ->
      CTTMlvRvalue(translate_rexp ~env ~loc v)

and make_call ~env ~loc lv args = 
  let ty = rettype_of_lv lv in
  make_mexpr ~loc ty (CTTMexpInvoke (translate_lv ~env ~loc lv, args))

and translate_fullrexp ~env ~loc t re = 
  let me =
    match re with
    | IL3RexCoerce(ct, t) ->
	CTTMexpCoerce(ct, translate_rexp ~env ~loc t)
    | IL3RexConstant(const) ->
	CTTMexpConstant(const)
    | IL3RexUndefined ->
	CTTMexpConstant(CTTconstInteger zero_big_int)
    | IL3RexBinop(bop, t1, t2) ->
	CTTMexpBinExpr(translate_binop bop, translate_rexp ~env ~loc t1, translate_rexp ~env ~loc t2)
    | IL3RexArgument(i) ->
	translate_arg ~env i
    | IL3RexAddress(lv, fields) ->
	CTTMexpAddress(translate_lv ~env ~loc lv, fields)
    | IL3RexUnaryop(uop, t1) ->
	CTTMexpUnaryExpr(uop, translate_rexp ~env ~loc t1)
  in
  make_mexpr ~loc t me

and translate_iexp ~env ~loc t ie = 
  let me = match ie with
  | IL3IexInvoke(lv, tids) ->
      CTTMexpInvoke(translate_lv ~env ~loc lv, list_map (translate_rexp ~env ~loc) tids)
  | IL3IexBinop(bop, t1, t2) ->
      CTTMexpBinExpr(translate_binop bop, translate_rexp ~env ~loc t1, translate_rexp ~env ~loc t2)
  in
  make_mexpr ~loc t me

and translate_rexp ~env ~loc (i : il3b_rexp) = 
  match i with
    IL3BTemp id -> 
      let id, t = get_variable ~env id in
      make_mexpr ~loc t (CTTMexpRead (CTTMlvVar (id, t), []))

  | IL3BRfun { locterm_loc = loc; locterm_v = (pr, s) } ->
      let lv = lv_reducible_functions ~genv:env.genv pr in
      let args = list_map (translate_rexp ~env ~loc) s in
      make_call ~loc ~env lv args

  | IL3BRexp { locterm_loc = loc; locterm_v = (t, re) } ->
      translate_fullrexp ~env ~loc t re

let rec translate_il3_to_list ~env ~current_block s = 
  dprintf 9 "translating %a." (Il3_formatter.pp_il3 Il3_formatter.pp_il3b_rexp) s;
  let loc = locget s in
  let stmts = 
    match (locval s).il3_desc with
    | IL3stmtIf(iftype,var,target) ->
	let expr = translate_rexp ~env ~loc var in
	let expr = cttm_to_ctt expr in
	let expr = 
	  match iftype with
	    IFTRUE -> expr
	  | IFNOT -> make_expr ~loc (CTTexpUnaryExpr(LogNot, expr)) type_boolean
	in
	[ make_stmt ~loc 
	    (CTTstmtIf(expr, 
		       make_branch ~env ~loc ~current_block target, None)) ]
    | IL3stmtSwitch(var, targets) ->
	let expr = translate_rexp ~env ~loc var in
	let expr = cttm_to_ctt expr in
	let gotos = Util.list_map
	    (fun (l, t) ->
	      let stmt = make_branch ~env ~loc ~current_block t in
	      match l with
		CASE i -> make_stmt ~loc (CTTstmtCase_Labeled(i, stmt))
	      | DEFAULT -> make_stmt ~loc (CTTstmtDefault_Labeled stmt)) targets
	in
	let body = make_stmt ~loc (CTTstmtCompound ([], gotos))
	in
	[ make_stmt ~loc (CTTstmtSwitch (expr, body)) ]
    | IL3stmtGoto t ->
	[ make_branch ~env ~loc ~current_block t ]
    | IL3stmtReturn vopt ->
	[ make_stmt ~loc
	    (CTTstmtReturn 
	       (Option.map
		  (fun e -> cttm_to_ctt (translate_rexp ~env ~loc e)) vopt)) ]
    | IL3stmtConditional(_, c, itrue, ifalse) ->
	let expr = translate_rexp ~env ~loc c in
	let expr = cttm_to_ctt expr in
	let itrue = translate_il3_to_stmt ~env ~current_block itrue in
	let ifalse = translate_il3_to_stmt ~env ~current_block ifalse in
	[ make_stmt ~loc (CTTstmtIf (expr, itrue, Some ifalse)) ]

    | IL3stmtSequence l -> 
	map_flatten (translate_il3_to_list ~env ~current_block) l
    | IL3stmtParallel l ->
	map_flatten (translate_il3_to_list ~env ~current_block) l

    | IL3stmtMove (d, _, s) ->
	let s = translate_rexp ~env ~loc s in
	[ make_assign ~env ~loc d s ]

    | IL3stmtRexp (d, t, s) ->
	let s = translate_fullrexp ~env ~loc t s in
	[ make_assign ~env ~loc d s ]

    | IL3stmtIexp (d, t, s) ->
	let s = translate_iexp ~env ~loc t s in
	[ make_assign ~env ~loc d s ]

    | IL3stmtCallReducibleFun(d, pr, args)
    | IL3stmtCallReducibleFunOverwriting(d, pr, args) ->
	let lv = lv_reducible_functions ~genv:env.genv pr in
	let args = list_map (translate_rexp ~env ~loc) args in
	[ make_assign ~env ~loc d (make_call ~env ~loc lv args) ]
	
    | IL3stmtCallImmobileFun(d, pi, args) ->
	let lv = lv_immobile_functions pi in
	let args = list_map (translate_rexp ~env ~loc) args in
	[ make_assign ~env ~loc d (make_call ~env ~loc lv args) ]

    | IL3stmtCallImmobileOp(io, args) ->
	let lv = lv_immobile_operations io in
	let args = list_map (translate_rexp ~env ~loc) args in
	[ make_voidstmt ~env ~loc (make_call ~env ~loc lv args) ]

    | IL3stmtCallAbort(s1, s2) ->
	let lv = lv_abortfunc in
	let args = list_map (translate_rexp ~env ~loc) [s1; s2] in
	[ make_voidstmt ~env ~loc (make_call ~env ~loc lv args) ]

    | IL3stmtCallReaderHelper (d, rw, s1, s2) ->
	let lv = lv_reader_helper ~genv:env.genv rw in
	let args = list_map (translate_rexp ~env ~loc) [s1; s2] in
	[ make_assign ~env ~loc d (make_call ~env ~loc lv args) ]

    | IL3stmtCallWriterHelper (rw, s1, s2, s3, s4o) ->
	let lv = lv_writer_helper ~genv:env.genv rw in
	let args = list_map (translate_rexp ~env ~loc) ([s1; s2; s3] @ Option.to_list s4o) in
	[ make_voidstmt ~env ~loc (make_call ~env ~loc lv args) ]
	  
    | IL3stmtReadRaw (d, cty, lv, flds) ->
	let exp = make_mexpr ~loc cty
	    (CTTMexpRead (translate_lv ~env ~loc lv, flds))
	in
	[ make_assign ~env ~loc d exp ]
    | IL3stmtWriteRaw (lv, flds, tid) ->
	let rhs = translate_rexp ~env ~loc tid in
	let mexp = CTTMexpWrite (translate_lv ~env ~loc lv, flds, None, rhs) in
	[ make_voidstmt ~env ~loc (make_mexpr ~loc type_void mexp) ] (* TODO: not void actually *)
  in
  stmts

and translate_il3_to_stmt ~env ~current_block ?(need_compound = false) s = 
  let loc = locget s in
  let s : Ctt_abstree.statement list = translate_il3_to_list ~env ~current_block s in
  if need_compound then 
    make_stmt ~loc (CTTstmtCompound ([], s))
  else match s with
    [i] -> i
  | [] -> make_stmt ~loc (CTTstmtNull)
  | _::_ -> 
      make_stmt ~loc (CTTstmtCompound ([], s))

let translate_last_branch ~env ~current_block i =
  dprintf 9 "translating %a as last-branch." (Il3_formatter.pp_il3 Il3_formatter.pp_il3b_rexp) i;
  let loc = locget i in
  match (locval i).il3_desc with
  | IL3stmtGoto t ->
      make_branch ~env ~loc ~current_block ~last:true t
  | IL3stmtReturn vopt ->
      make_stmt ~loc 
	(CTTstmtReturn 
	   (Option.map
	      (fun e -> cttm_to_ctt (translate_rexp ~env ~loc e)) vopt))
  | _ -> assert false

let rec translate_bulkinit_partial ~env init = 
  let loc = locget init in
  match locval init with
    ILinitConstantExp _ -> failwith "i2c unused 20-2-1"
  | ILinitPostFill -> failwith "i2c panic 20-2-2" (* generated code should not have this *)
  | ILinitStruct(_, l) -> 
      locput ~loc (CTTinitList (Util.list_map (function t, s -> translate_bulkinit_partial ~env s) l))
  | ILinitArray(_, l) ->
      locput ~loc (CTTinitList (Util.list_map (translate_bulkinit_partial ~env) l))
  | ILinitAbstractCtt c ->
      locput ~loc (CTTinitExp (Cttm_to_ctt.translate_expr c))

let translate_prologue_il0 ~env s = 
  let translate s = 
    match (locval s).il0_t with
    | IL0stmtDeclAutoScalar(lsc,cty,id,None)
    | IL0stmtDeclBulk(lsc,cty,id,None) ->
	(lsc, cty, id, None)
    | IL0stmtDeclBulk(lsc,cty,id, Some init) -> 
	let init = translate_bulkinit_partial ~env init in
	(lsc, cty, id, Some init)
    | _ -> failwith "unused 232"
  in
  list_map translate s

let make_phi_assignments ~env ~loc phi_chains = 
  let l = Glist.empty () in
  map_flatten
    (list_map
       (fun (d, s) ->
	 let id, t = get_variable ~env s in
	 let mexpr = make_mexpr ~loc t (CTTMexpRead (CTTMlvVar (id, t), [])) in
	 make_assign ~loc ~env d mexpr))
    phi_chains

let translate_block ~env current_block = 
  let b = env.blocks.(current_block) in
  dprintf 5 "block %d of %s." current_block env.func_name;
  let code, last_branch =
    let rec iter = function
	[] -> [], None
      | [i] as l -> begin
	  match (locval i).il3_desc with
	    IL3stmtSwitch(_,[DEFAULT, t]) ->
	      [], Some (make_il3 ~loc:(locget i) (IL3stmtGoto t))
	  | IL3stmtGoto _ | IL3stmtReturn _ -> [], Some i
	  | _ -> l, None
      end
      | h::t -> 
	  let t1, t2 = iter t in
	  h::t1, t2
    in
    iter b.code
  in
  dprintf 9 "last branch: %b." (last_branch <> None);
  let code, last_branch = 
    map_flatten (translate_il3_to_list ~env ~current_block) code,
    Option.map (translate_last_branch ~env ~current_block) last_branch
  in
  dprintf 9 "last branch: %b." (last_branch <> None);
  let last_branch_loc, last_branch = 
    if last_branch = None &&
      current_block <> env.n_blocks - 1 &&
      env.phi_info.(current_block + 1).trampoline_blocks <> []
    then
      let loc = env.blocks.(current_block + 1).location in
      loc, [ make_stmt ~loc (CTTstmtGoto (label_direct (current_block + 1))) ]
    else
      match last_branch with
	Some i -> locget i, [i]
      | None -> 
	  if current_block <> env.n_blocks - 1
	  then env.blocks.(current_block + 1).location, []
	  else dummy_location, []
  in
  let bottom_phi_assigns = 
    match env.phi_info.(current_block).phi_assignments_at_bottom with
      None -> []
    | Some (i, phi) ->
	make_labelled ~loc:last_branch_loc (label_trampoline current_block i)
	  (make_phi_assignments ~env ~loc:last_branch_loc phi)
  in
  let current_loc = env.blocks.(current_block).location in
  let trampolines = 
    let rec iter = function
	[] -> []
      | [i, chain] ->
	  make_labelled ~loc:current_loc (label_trampoline i current_block)
	    (make_phi_assignments ~env ~loc:current_loc chain)
      | (i, chain):: tl ->
	  make_labelled ~loc:current_loc (label_trampoline i current_block)
	    (make_phi_assignments ~env ~loc:current_loc chain) @
	  [ make_stmt ~loc:current_loc (CTTstmtGoto (label_direct current_block)) ] @
	  iter tl
    in
    iter env.phi_info.(current_block).trampoline_blocks
  in
  trampolines @
  make_labelled ~loc:current_loc (label_direct current_block) code @
  bottom_phi_assigns @ last_branch

let generate_tempid_decls ~env = 
  let decls = Glist.empty () in
(* generate declarations for temp ids *)
  Earray.iteri
    (fun id b -> 
      if b then begin
	let name, t = get_variable ~env id in
	(* local_storage_class * c_type * identifier * ctt_initializer option *)
	let decl = Auto, t, name, None in
	Glist.put decls decl
      end
    )
    env.tempid_generated;
  Glist.to_list decls

let translate_function ~genv ~function_loc name ct args f = 
  dprintf 4 "Genelating C code for function %s." name;
  let argtypes =
    match ct.ct_ty with
      Tfunction(at,_,_) -> at
    | _ -> assert false
  in
  let env = {
    func_name = name;
    argnames = args;
    argtypes = argtypes;
    additional_decls = Glist.empty ();
    
    variable_environment = f.Il3.variable_environment;
    genv = genv;
    f = f;
    blocks = f.body;
    n_blocks = Array.length f.body;
    cogen_hint = fst f.more_info;
    phi_info = snd f.more_info;
    tempid_generated = Earray.empty_with_default ~zero:false
  }
  in
  let new_body =
    let b = Glist.empty () in
    for i = 0 to env.n_blocks - 1 do
      Glist.append b (translate_block ~env i)
    done;
    Glist.to_list b
  in
  let new_decls = 
    translate_prologue_il0 ~env f.prologue
    @ generate_tempid_decls ~env
  in
  let new_body = 
    make_stmt ~loc:function_loc (CTTstmtCompound(new_decls, new_body))
  in
  new_body

let translate_initializer init =
  (* Translate generates CTTMinitializer instead of ILinitializer. *)
  Cttm_to_ctt.translate_initializer init

let translate_program ~genv : _ -> Ctt_abstree.program =
  locmapl_list
    (fun ~loc -> function
	IL3declFunction(gs, ct, id, args, f) ->
	  CTTdeclFunction
	    (gs, ct, id, args, translate_function ~genv ~function_loc:loc id ct args f)
      | IL3declVariable (gs,ct,id,init) ->
	  CTTdeclVariable
	    (gs, ct, id, Option.map translate_initializer init))

let translate_from_raw_il3 ~genv t = 
  (* helper for linker *)
  let t = Il3_fixup.translate_program ~genv t in
  let t = Il3_optimize.optimize_program_b t in
  let t = Il3_decompose_ssa.f t in
  translate_program ~genv t
