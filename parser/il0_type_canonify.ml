(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)

open Util
open Big_int_infix
open Ctt_abstree
open Il
open Il0
open Set_list
open Locterm

(* change type of integers on memory to canonical unsigned integer *)

open Util
open Ctt_abstree
open Cttm_abstree

let canonical_types = Earray.empty ()
let canonical_signed_types = Earray.empty ()
let canonical_floating_types = Earray.empty ()

let _ = 
  List.iter
    (fun bt ->
      Earray.set canonical_types (size_of_builtin_type bt) bt)
    [ Tulonglong; Tushort; Tulong; Tuchar; Tuint ]

let _ = 
  List.iter
    (fun bt ->
      Earray.set canonical_signed_types (size_of_builtin_type bt) bt)
    [ Tlonglong; Tshort; Tlong; Tschar; Tint ]
    
let _ = 
  List.iter
    (fun bt ->
      Earray.set canonical_floating_types (size_of_builtin_type bt) bt)
    [ Tlongdouble; Tfloat; Tdouble ]

let rec translate_c_type_mem t = 
  let new_type = 
    match t.ct_ty with
      Tpointer t -> Tpointer (translate_c_type_mem t)
    | Tfunction(at,va,rt) ->
	Tfunction(Util.list_map translate_c_type_mem at, va, translate_c_type_mem rt)
    | Tbuiltin (Tchar | Tschar | Tuchar 
                | Tshort | Tushort 
		| Tint | Tuint
		| Tlong | Tulong
		| Tlonglong | Tulonglong as bt) ->
      let sz = size_of_builtin_type bt in
      Tbuiltin (Earray.get canonical_types sz)

    | Tbuiltin (Tfloat | Tdouble | Tlongdouble as bt) ->
	let sz = size_of_builtin_type bt in
	Tbuiltin (Earray.get canonical_floating_types sz)
    | Tvoid
    | Tstruct _
    | Tabstract _ -> t.ct_ty
    | Tarray(ty,sz) -> Tarray(translate_c_type_mem ty, sz)
  in
  { t with ct_ty = new_type }

let rec is_signed_type t = 
  match t.ct_ty with
  | Tbuiltin (Tchar | Tschar | Tshort | Tint | Tlong | Tlonglong as bt) ->
    is_signed_builtin_type bt (* char may be either signed or unsigned *)
  | _ -> false

let translate_c_type_reg t = 
  match t.ct_ty with
  | Tbuiltin (Tchar | Tschar | Tshort | Tint | Tlong | Tlonglong as bt) 
    when is_signed_builtin_type bt (* char may be either signed or unsigned *) ->
      let sz = size_of_builtin_type bt in
      { t with ct_ty = Tbuiltin (Earray.get canonical_signed_types sz); 
	ct_const_p = false }
  | _ -> translate_c_type_mem t

type environment = 
    {
     regvar_set : identifier set;
     tempid_types : c_type earray;
     genv : Ctt_abstree.environment;
     mutable max_tempid : int
   }

let make_tempid ~env t = 
  let tid = env.max_tempid in
  env.max_tempid <- tid + 1;
  Earray.set env.tempid_types tid t;
  tid

let type_of_tempid ~env id = 
  Earray.get env.tempid_types id

(**************** initializer ****************)

let translate_lvalue = function
    ILlvVar(id,cty) -> ILlvVar(id, translate_c_type_mem cty)
  | (ILlvPtr _ | ILlvTemp _) as lv -> lv

(**************** initializer ****************)

let translate_fields = 
  list_map (fun (id, cty) -> (id, translate_c_type_mem cty))

let rec translate_constant_exp cexp = 
  let desc = match (locval cexp).il_cexp_t with
    ILcexpCoerce(cty,cexp) ->
      ILcexpCoerce(translate_c_type_reg cty, translate_constant_exp cexp)
  | ILcexpConstant c as orig -> orig
  | ILcexpBinop(bop, ce1, ce2) ->
      ILcexpBinop(bop, translate_constant_exp ce1, translate_constant_exp ce2)
  | ILcexpUnaryop(uop, ce1) ->
      ILcexpUnaryop(uop, ce1)
  | ILcexpAddress(lv,fields) ->
      ILcexpAddress(translate_lvalue lv, translate_fields fields)
  | ILcexpAddFieldOfs(e,fields) ->
      ILcexpAddFieldOfs(translate_constant_exp e, translate_fields fields)
  in
  loccopy ~orig:cexp { il_cexp_type = translate_c_type_reg (locval cexp).il_cexp_type; il_cexp_t = desc }

let rec translate_initializer i = 
  locmap (function
    ILinitConstantExp(cty, ce) ->
      let ce = translate_constant_exp ce in
      if is_signed_type cty then
	let memtyp = translate_c_type_mem cty in
	let ce' = loccopy ~orig:ce { il_cexp_type = memtyp; il_cexp_t = ILcexpCoerce(memtyp, ce) } in
	ILinitConstantExp(memtyp, ce')
      else
	ILinitConstantExp(translate_c_type_mem cty, ce)
  | ILinitPostFill as orig -> orig
  | ILinitStruct(cty, inits) ->
      ILinitStruct(translate_c_type_mem cty,
		   list_map (fun (id, init) -> id, translate_initializer init) inits)
  | ILinitArray(cty, inits) ->
      ILinitArray(translate_c_type_mem cty, list_map translate_initializer inits)
  | ILinitAbstractCtt _ -> assert false) i

(**************** expression ****************)

let translate_expr ~env ~loc cty = function
    ILexpCoerce(cty,tid) ->
      [], ILexpCoerce(translate_c_type_reg cty, tid), false
  | ILexpInvoke(lv,tids) -> begin
      let convs = Glist.empty () in
      let tids = Util.list_map
	  (function tid ->
	    let typ = type_of_tempid ~env tid in
	    if is_signed_type (type_of_tempid ~env tid) then
	      let memtyp = translate_c_type_mem typ in
	      let tid0 = make_tempid ~env memtyp in
	      Glist.put convs 
		(make_il0 ~loc (IL0stmtDefTemp(tid0, memtyp, ILexpCoerce(memtyp, tid))));
	      tid0
	    else
	      tid) tids in
      Glist.to_list convs, ILexpInvoke(translate_lvalue lv, tids), true
  end
  | ILexpAddress(lv,fields) ->
      [], ILexpAddress(translate_lvalue lv, translate_fields fields), false
  | ILexpArgument(n) as orig ->
      [], orig, true
  | ( ILexpIdent _ | ILexpConstant _ | ILexpUndefined | ILexpBinop _ | ILexpUnaryop _ ) as orig
    -> [], orig, false

let translate_read ~env ~orig tid cty lv fields = 
  let loc = locget orig in
  match lv, fields with
    ILlvVar(id,cty), [] when mem id env.regvar_set ->
      let regtyp = translate_c_type_reg cty in
      [ make_il0 ~loc (IL0stmtReadToTemp(tid, regtyp, ILlvVar(id, regtyp), [])) ]
  | _ ->
      let memtyp = translate_c_type_mem cty in
      if is_signed_type cty then begin
	let regtyp = translate_c_type_reg cty in
	let tid0 = make_tempid ~env regtyp in
	[ make_il0 ~loc (IL0stmtReadToTemp(tid0, memtyp, translate_lvalue lv, translate_fields fields));
	  make_il0 ~loc (IL0stmtDefTemp(tid, regtyp, ILexpCoerce(regtyp, tid0))) ]
      end else
	[ make_il0 ~loc (IL0stmtReadToTemp(tid, memtyp, translate_lvalue lv, translate_fields fields)) ]

let translate_write ~env ~orig lv fields tid =
  let loc = locget orig in
  match lv, fields with
    ILlvVar(id,cty), [] when mem id env.regvar_set ->
      [ make_il0 ~loc (IL0stmtWrite(ILlvVar(id, translate_c_type_reg cty), [], tid)) ]
  | _ ->
      let tidtyp = type_of_tempid ~env tid in
      if is_signed_type tidtyp then begin
	let unsig_typ = translate_c_type_mem tidtyp in
	let tid0 = make_tempid ~env unsig_typ in
	[ make_il0 ~loc (IL0stmtDefTemp(tid0, unsig_typ, ILexpCoerce(unsig_typ, tid)));
	  make_il0 ~loc (IL0stmtWrite(translate_lvalue lv, translate_fields fields, tid0)) ]
      end else
	[ make_il0 ~loc (IL0stmtWrite(translate_lvalue lv, translate_fields fields, tid)) ]

let translate_return ~env ~orig tid = 
  let loc = locget orig in
  let tidtyp = type_of_tempid ~env tid in
  if is_signed_type tidtyp then begin
    let unsig_typ = translate_c_type_mem tidtyp in
    let tid0 = make_tempid ~env unsig_typ in
    [ make_il0 ~loc (IL0stmtDefTemp(tid0, unsig_typ, ILexpCoerce(unsig_typ, tid)));
      make_il0 ~loc (IL0stmtReturn(Some tid0)) ]
  end else
    [ orig ]

let translate_autodecl ~env ~orig lsc id cty tidopt = 
  let loc = locget orig in
  if mem id env.regvar_set then
    [ make_il0 ~loc (IL0stmtDeclAutoScalar(lsc, translate_c_type_reg cty, id, tidopt)) ]
  else
    match tidopt with
      None ->
	[ make_il0 ~loc (IL0stmtDeclAutoScalar(lsc, translate_c_type_mem cty, id, None)) ]
    | Some tid ->
	let tidtyp = type_of_tempid ~env tid in
	let unsig_typ = translate_c_type_mem tidtyp in
	if is_signed_type tidtyp then begin
	  let tid0 = make_tempid ~env unsig_typ in
	  [ make_il0 ~loc (IL0stmtDefTemp(tid0, unsig_typ, ILexpCoerce(unsig_typ, tid)));
	    make_il0 ~loc (IL0stmtDeclAutoScalar(lsc, unsig_typ, id, Some tid0)) ]
	end else
	  [ make_il0 ~loc (IL0stmtDeclAutoScalar(lsc, unsig_typ, id, Some tid)) ]
	    
let rec translate_il0 ~env orig = 
  let loc = locget orig in
  match (locval orig).il0_t with
  | IL0stmtDeclBulk(lsc, ty, id, initopt) ->
      make_il0 ~loc (IL0stmtDeclBulk(lsc, translate_c_type_mem ty, id, 
				     Option.map translate_initializer initopt))
  | IL0stmtSequence(ils) ->
      enclose_sequence ~loc (list_map (translate_il0 ~env) ils)
  | IL0stmtParallel(ils) ->
      enclose_parallel ~loc (list_map (translate_il0 ~env) ils)
  | IL0stmtDefTemp(tid, typ, expr) ->
      let regtyp = translate_c_type_reg typ in
      let insn, newexp, coerce_to_signed = translate_expr ~env ~loc typ expr in
      if coerce_to_signed && is_signed_type typ then
	let memtyp = translate_c_type_mem typ in
	let tid0 = make_tempid ~env memtyp in
	enclose_parallel ~loc
	  (list_append insn
	     [ make_il0 ~loc (IL0stmtDefTemp(tid0, memtyp, newexp));
	       make_il0 ~loc (IL0stmtDefTemp(tid, regtyp, ILexpCoerce(regtyp, tid0))) ])
      else if insn = [] then
	make_il0 ~loc (IL0stmtDefTemp(tid, regtyp, newexp))
      else
	enclose_parallel ~loc
	  (list_append insn
	     [ make_il0 ~loc (IL0stmtDefTemp(tid, regtyp, newexp)) ])
  | IL0stmtReadToTemp(tid, cty, lv, fields) ->
      enclose_parallel ~loc (translate_read ~env ~orig tid cty lv fields)
  | IL0stmtWrite(lv, fields, tid) ->
      enclose_parallel ~loc (translate_write ~env ~orig lv fields tid)
  | IL0stmtDeclAutoScalar(lsc, cty, id, tidopt) ->
      enclose_parallel ~loc (translate_autodecl ~env ~orig lsc id cty tidopt)
  | IL0stmtReturn (Some id) ->
      enclose_parallel ~loc (translate_return ~env ~orig id)
  | ( IL0stmtLabel _
      | IL0stmtIf _
      | IL0stmtSwitch _
      | IL0stmtGoto _
      | IL0stmtReturn None
      | IL0stmtAbort _) -> orig

let scan_for_regvar fb = 
  let scalars = ref empty in
  let addrs = ref empty in
  let scan_lv = function
      ILlvVar(id, _) -> addrs := add !addrs id
    | ILlvPtr _ | ILlvTemp _ -> ()
  in
  let rec scan_cexp cexp = match (locval cexp).il_cexp_t with
    ILcexpCoerce(cty,ce1) -> scan_cexp ce1
  | ILcexpConstant _ -> ()
  | ILcexpBinop(bop,ce1,ce2) -> scan_cexp ce1; scan_cexp ce2
  | ILcexpUnaryop(uop,ce1) -> scan_cexp ce1
  | ILcexpAddress(lv,fld) -> scan_lv lv
  | ILcexpAddFieldOfs(ce1,fld) -> scan_cexp ce1
  in
  let rec scan_initializer i = match locval i with
      ILinitConstantExp(cty, cexp) -> scan_cexp cexp
    | ILinitPostFill -> ()
    | ILinitStruct(cty,inits) ->
	List.iter (fun (id, init) -> scan_initializer init) inits
    | ILinitArray(cty,inits) ->
	List.iter scan_initializer inits
    | ILinitAbstractCtt _ -> assert false
  in
  let scan_expr = function
    | ILexpAddress(lv,fld) ->
	scan_lv lv
    | ILexpCoerce _ 
    | ILexpConstant _
    | ILexpUndefined
    | ILexpBinop _
    | ILexpUnaryop _
    | ILexpInvoke _
    | ILexpArgument _
    | ILexpIdent _ -> ()
  in 
  let rec scan_instr i =
    match (locval i).il0_t with
      IL0stmtDeclAutoScalar(lsc,cty,id,tidopt) -> begin
	match cty.ct_ty with
	  Tbuiltin _ -> 
	    scalars := add !scalars id
	| _ -> ()
      end
    | IL0stmtDefTemp(tid,cty,expr) ->
	scan_expr expr
    | IL0stmtDeclBulk _
    | IL0stmtIf _
    | IL0stmtSwitch _
    | IL0stmtGoto _
    | IL0stmtReturn _
    | IL0stmtAbort _
    | IL0stmtReadToTemp _
    | IL0stmtLabel _
    | IL0stmtWrite _ -> ()
    | IL0stmtSequence l
    | IL0stmtParallel l ->
	List.iter scan_instr l
  in
  scan_instr fb.il0_funcbody;
  subtract !scalars !addrs

let translate_funcbody ~genv fb = 
  let env = {
    regvar_set = scan_for_regvar fb;
    tempid_types = fb.il0_var_types;
    genv = genv;
    max_tempid = Earray.length fb.il0_var_types
  } in
  let body = translate_il0 ~env fb.il0_funcbody in
  {
   il0_var_types = env.tempid_types;
   il0_funcbody = body
 }

let translate_declaration_desc ~genv = function
    IL0declFunction(gsc,cty,id,args,fbody) ->
      IL0declFunction(gsc, translate_c_type_mem cty,
		      id, args, translate_funcbody ~genv fbody)
  | IL0declVariable(gsc,cty,id,initopt) ->
      IL0declVariable(gsc, translate_c_type_mem cty,
		      id, Option.map (translate_initializer) initopt)

let rec translate_structure_fields ~genv = function
    [] -> []
  | (ofs, NormalField f) :: tl ->
      (ofs, NormalField { f with sf_type = translate_c_type_mem f.sf_type })
      :: translate_structure_fields ~genv tl
  | (_, BitField bf) :: tl -> 
      failwith "il0_type_canonify: PANIC: bit fields must be reduced first."
   
let translate_structure ~genv desc = 
  let desc = 
    { desc with str_fields = translate_structure_fields ~genv desc.str_fields } in
  C_typing.update_fields_cache desc

let translate_environment ~genv = 
  { genv with
    struct_table = Earray.map (translate_structure ~genv) genv.struct_table }

let translate_program ~genv p = 
  let p = locmap_list (translate_declaration_desc ~genv) p in
  translate_environment ~genv, p
  
