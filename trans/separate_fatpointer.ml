(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

open Util
open Locterm
open Ctt_abstree
open Il2
open Il
open Ils
open Big_int_infix

include Debug.Install (struct let category = 60 end)

let make_ils = locput

type translate_environment = 
  {
   function_name : string;
   global_env : Ctt_abstree.environment;
   original_env : Il.il_variable_attribute array;
   varmap : (int * int) array;
   mutable additional_variables : (int * ils_type) list;
   mutable maxvar : int;
   result_type : c_type;
 }

let translate_variable ~env tid = env.varmap.(tid)

let get_temp_type ~env id =
  env.original_env.(id).Il.variable_type

let get_tempid ~env typ = 
  let new_id = env.maxvar in
  env.maxvar <- new_id + 1;
  env.additional_variables <- (new_id, typ) :: env.additional_variables;
  new_id

let constant_zero = CTTconstInteger zero_big_int
let constant_one = CTTconstInteger unit_big_int

let is_toonarrow t = 
  match t.ct_ty with
    Tbuiltin (Tchar | Tuchar | Tschar | Tshort | Tushort | Tfloat | Tdouble) -> true
  | Tvoid -> true (* return value of void function *)
  | Tstruct _ -> true
  | Tbuiltin _ -> false
  | Tpointer _ -> false
  | Tarray _ -> false (* treated as a pointer *)
  | _ -> assert false

let is_pointer t = 
  match t.ct_ty with
    Tpointer _ -> true
  | Tarray _ -> true
  | _ -> false

let is_struct_type t = 
  match t.ct_ty with
    Tstruct _ -> true
  | _ -> false

let is_numeric t = 
  match t.ct_ty with
    Tbuiltin _ -> true
  | _ -> false

let translate_lv ~env lv = 
  match lv with
    IL2lvPtr s ->
      let source_b, source_v = translate_variable ~env s in
      ILSlvPtr (source_b, source_v)
  | IL2lvVar(k,v,ty) ->
      ILSlvVar (k,v,ty)
  | IL2lvTemp(temp) ->
      let source_b, source_v = translate_variable ~env temp in
      assert (source_b == -1);
      ILSlvTemp source_v

let get_boolean ~env t1 = 
  let source_b, source_v = translate_variable ~env t1 in
  let source_type = get_temp_type ~env t1 in
  if is_pointer source_type then begin
    let tempn = get_tempid ~env (ILStypeBase source_type) in
    let tempz = get_tempid ~env (ILStypeOfs source_type)  in
    let tempb = get_tempid ~env (ILStypeVal type_boolean) in
    let tempo = get_tempid ~env (ILStypeVal type_boolean) in
    let tempr = get_tempid ~env (ILStypeVal type_boolean) in
    [
     ILSstmtAssign(tempn,ILSexpConstant(CTTconstNull));
     ILSstmtAssign(tempz,ILSexpConstant(constant_zero));
     ILSstmtAssign(tempb,ILSexpBinop(ILbinNotEqual,source_b,tempn));
     ILSstmtAssign(tempo,ILSexpBinop(ILbinNotEqual,source_v,tempz));
     ILSstmtAssign(tempr,ILSexpBinop(ILbinLogOr,tempb,tempo))
   ], tempr
  end
  else
    [], source_v

let clear_base source_b = 
  if Fsc_config.allow_integer_arith_on_pointer then
    ILSexpIdent(source_b)
  else
    ILSexpConstant(CTTconstNull)

let make_int_constant i = ILSexpConstant(CTTconstInteger i)

let sizeof_genv ~genv t = 
  match Ctt_abstree.size_of_type ~genv t with
    None -> failwith_p "Separate_fatpointer.sizeof: size unknown: %a" Il_formatter.pp_ctt_type t
  | Some s -> s

let sizeof ~env t = sizeof_genv ~genv:env.global_env t

let sizeof_target_genv ~genv t =
  match t.ct_ty with
    Tpointer t2 ->
      sizeof_genv ~genv t2
  | _ -> assert false

let sizeof_target ~env t = sizeof_target_genv ~genv:env.global_env t

let is_power_of_2_big_int bi = 
  (land_big_int bi (pred_big_int bi)) ==! zero_big_int

let rec get_field_offset ~genv t =
  let get t field : struct_ofsinfo =
    match t.ct_ty with
      Tstruct id -> begin
	let sd = Ctt_abstree.get_struct_desc ~genv id in
	try
	  snd (List.assoc field sd.str_fields_byname)
	with
	  Not_found ->
	    failwith_p "panic883: not found .%s in struct #%d" field id
      end
    | _ -> assert false
  in
  function
      [] -> StrOfsNormal zero_big_int
    | [f,_] ->
	get t f
    | (f,ft)::tl -> begin
	match get t f with
	  StrOfsNormal thisofs -> begin
	    match (get_field_offset ~genv ft tl : struct_ofsinfo)
	    with
	      StrOfsNormal ofs -> 
		StrOfsNormal (add_big_int thisofs ofs)
	    | StrOfsBitfield (ofs, width, start) ->
		StrOfsBitfield (add_big_int thisofs ofs, width, start)
	  end
	| StrOfsBitfield _ -> assert false
    end

let rec translate_assign ~env ~loc tid typ exp =
  let target_b, target_v = translate_variable ~env tid in
  let target_is_narrow = is_toonarrow typ in
  let target_is_pointer = is_pointer typ in
  let target_is_numeric = is_numeric typ in
  let typ_base = ILStypeBase typ in
  let typ_ofsval = if target_is_pointer then ILStypeOfs typ else ILStypeVal typ in
  let new_insns = match exp with 
    IL2expCoerce(t,s) -> begin
      let b, v = (translate_variable ~env s) in
      let source_type = get_temp_type ~env s in
      let source_is_narrow = is_toonarrow source_type in
      let source_is_numeric = is_numeric source_type in
      match source_is_numeric, source_is_narrow, target_is_numeric, target_is_narrow with
	true, _, true, true -> (* numeric -> narrow numeric *)
	  [ ILSstmtAssign(target_v, ILSexpCoerce1(t, v)) ]
      | true, true, true, false -> (* narrownum -> fatnum : null_expand *)
	  [ ILSstmtAssign(target_v, ILSexpCoerce1(t, v));
	    ILSstmtAssign(target_b, ILSexpConstant(CTTconstNull))
	  ]
      | true, false, true, false -> (* fatnum -> fatnum : keep base *)
	  [ ILSstmtAssign(target_v, ILSexpCoerce1(t, v));
	    ILSstmtAssign(target_b, ILSexpIdent(b));
	  ]
      | false, false, false, false (* ptr -> ptr *)
      | false, false, true, _      (* ptr -> num *)
      | true, _, false, false      (* num -> ptr *) ->
	  let e =
	    if source_is_narrow then
	      ILSexpCoerce1(t, v)
	    else
	      ILSexpCoerce2(t, b, v)
	  in
	  if target_is_narrow then
	    [
	     ILSstmtAssign(target_v, e)
	   ]
	  else
	    [ ILSstmtAssign2(target_b, target_v, e) ]
      | _ -> assert false
    end
  | IL2expConstant(const) -> begin
      match const with
	CTTconstNull | CTTconstTypeInfo _ | CTTconstString _ ->
	  assert(not target_is_narrow);
	  [ ILSstmtAssign(target_b,ILSexpConstant(const));
	    ILSstmtAssign(target_v,ILSexpConstant(constant_zero)) ]
      | _ ->
	  (ILSstmtAssign(target_v,ILSexpConstant(const)))
	  ::(if target_is_narrow then [] else
	  [ ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull)) ])
  end
  | IL2expUndefined ->
      if is_toonarrow typ then
	[ ILSstmtAssign(target_v,ILSexpUndefined) ]
      else
	[ 
	  ILSstmtAssign(target_b,ILSexpUndefined);
	  ILSstmtAssign(target_v,ILSexpUndefined); ]
  | IL2expBinop(
    (ILbinTimes | ILbinDiv | ILbinPlusVV | ILbinMinusVV
  | ILbinModulo | ILbinLshift | ILbinRshift
  | ILbinIntAnd | ILbinIntOr | ILbinIntXor) as bop,t1,t2) ->
      let t1b,t1v = translate_variable ~env t1 in
      let t2b,t2v = translate_variable ~env t2 in
      ILSstmtAssign(target_v,ILSexpBinop(bop,t1v,t2v))
      ::
	if target_is_narrow then []
	else if is_toonarrow(get_temp_type ~env t1) then      
	  [ ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull)) ]
	else
	  [ ILSstmtAssign(target_b,clear_base t1b) ]
  | IL2expBinop((ILbinPlusPV | ILbinMinusPV) as bop,t1,t2) ->
      let type_t1 = get_temp_type ~env t1 in
      let tempc = get_tempid ~env (ILStypeVal type_size_t) in
      let tempo = get_tempid ~env (ILStypeOfs type_t1) in
      let t1b,t1v = translate_variable ~env t1 in
      let t2b,t2v = translate_variable ~env t2 in
      let newop = match bop with
	ILbinPlusPV -> ILbinPlusVV
      | ILbinMinusPV -> ILbinMinusVV
      | _ -> assert false
      in
      let elemsz = sizeof_target ~env type_t1 in
      if is_power_of_2_big_int elemsz then
	[ 
	  ILSstmtAssign(tempc,make_int_constant(sizeof_target ~env type_t1));
	  ILSstmtAssign(tempo,ILSexpBinop(ILbinTimes,tempc,t2v));
	  ILSstmtAssign(target_b,ILSexpIdent(t1b));
	  ILSstmtAssign(target_v,ILSexpBinop(newop,t1v,tempo))
	]
      else
	[
	 ILSstmtAssign2(target_b, target_v, ILSexpBinop21(bop,(t1b,t1v),t2v))
       ]
  | IL2expBinop(ILbinMinusPP,t1,t2) ->
      let type_t1 = get_temp_type ~env t1 in
      let t1b,t1v = translate_variable ~env t1 in
      let t2b,t2v = translate_variable ~env t2 in
      let tempc = get_tempid ~env (ILStypeVal type_size_t) in
      let tempo = get_tempid ~env (ILStypeVal type_size_t) in
      [
       ILSstmtAbortIf(ILSexpBinop(ILbinNotEqual,t1b,t2b),ILSerrPtrMinus);
       ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull));
       ILSstmtAssign(tempc,make_int_constant(sizeof_target ~env type_t1));
       ILSstmtAssign(tempo,ILSexpBinop(ILbinMinusVV,t1v,t2v));
       ILSstmtAssign(target_v,ILSexpBinop(ILbinDiv,tempo,tempc))
     ]
  | IL2expBinop((ILbinLogAnd | ILbinLogOr) as bop,t1,t2) ->
      let tr1, tv1 = get_boolean ~env t1 in
      let tr2, tv2 = get_boolean ~env t2 in
      tr1 @ tr2 @ 
      [ ILSstmtAssign(target_b, ILSexpConstant(constant_zero));
	ILSstmtAssign(target_v, ILSexpBinop(bop,tv1,tv2)) ]
  | IL2expBinop((ILbinLessThan | ILbinLessEqual | ILbinGtrThan | ILbinGtrEqual) as bop,
	       t1,t2) ->
      let t1b,t1v = translate_variable ~env t1 in
      let t2b,t2v = translate_variable ~env t2 in
      if is_pointer(get_temp_type ~env t1) then
	[
	 ILSstmtAbortIf(ILSexpBinop(ILbinNotEqual,t1b,t2b),ILSerrPtrComp);
	 ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull));
	 ILSstmtAssign(target_v,ILSexpBinop(bop,t1v,t2v))
       ]
      else
	[ ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull));
	  ILSstmtAssign(target_v,ILSexpBinop(bop,t1v,t2v)) ]
  | IL2expBinop((ILbinEqual | ILbinNotEqual) as bop,t1,t2) ->
      let is_neg = (bop = ILbinNotEqual) in
      let mergeop = if is_neg then ILbinLogOr else ILbinLogAnd in
      let t1b,t1v = translate_variable ~env t1 in
      let t2b,t2v = translate_variable ~env t2 in
      if is_pointer(get_temp_type ~env t1) then begin
	let tempb = get_tempid ~env (ILStypeVal type_boolean) in
	let tempo = get_tempid ~env (ILStypeVal type_boolean) in
	[
	 ILSstmtAssign(tempb,ILSexpBinop(bop,t1b,t2b));
	 ILSstmtAssign(tempo,ILSexpBinop(bop,t1v,t2v));
	 ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull));
	 ILSstmtAssign(target_v,ILSexpBinop(mergeop,tempb,tempo))
       ]
      end
      else
	[ ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull));
	  ILSstmtAssign(target_v,ILSexpBinop(bop,t1v,t2v))
	]
  | IL2expUnaryop(LogNot,s) when is_pointer(get_temp_type ~env s) ->
      let source_b, source_v = translate_variable ~env s in
      let tempb = get_tempid ~env (ILStypeVal type_boolean) in
      let tempo = get_tempid ~env (ILStypeVal type_boolean) in
      let tempn = get_tempid ~env (ILStypeBase (get_temp_type ~env s)) in
      let temp0 = get_tempid ~env (ILStypeOfs (get_temp_type ~env s)) in
      [
       ILSstmtAssign(tempn,ILSexpConstant(CTTconstNull));
       ILSstmtAssign(temp0,ILSexpConstant(CTTconstInteger zero_big_int));
       ILSstmtAssign(tempb,ILSexpBinop(ILbinEqual,source_b,tempn));
       ILSstmtAssign(tempo,ILSexpBinop(ILbinEqual,source_v,temp0));
       ILSstmtAssign(target_v,ILSexpBinop(ILbinLogAnd,tempb,tempo)) ]
      @
	if not target_is_narrow then
	  [ ILSstmtAssign(target_b,ILSexpConstant(CTTconstNull)) ]
	else []
  | IL2expUnaryop(uop,s) ->
      let source_b, source_v = translate_variable ~env s in
      ILSstmtAssign(target_v,ILSexpUnaryop(uop,source_v))
      ::
	if not target_is_narrow then
	  [ ILSstmtAssign(target_b,clear_base(source_b)) ]
	else []
  | IL2expInvoke(lv,args) ->
      let lv = 
	match lv with
	  IL2lvPtr(v) ->
	    let b, v' = translate_variable ~env v in
	    ILSlvPtrToFunc(b,v')
	| IL2lvVar(k,t,v) -> ILSlvVar(k,t,v)
	| IL2lvTemp _ -> assert false
      in
      let args = list_map
	  (fun arg ->
	    if is_toonarrow(get_temp_type ~env arg) then
	      ILSFuncArgNarrow(snd (translate_variable ~env arg))
	    else
	      let b, v = (translate_variable ~env arg) in
	      ILSFuncArgWide(b,v)) args in
      if target_is_narrow then
	[ ILSstmtAssign(target_v, ILSexpInvoke(lv,args)) ]
      else
	[ ILSstmtAssign2(target_b, target_v, ILSexpInvoke(lv,args)) ]

  | IL2expAddress(IL2lvPtr s, field) -> 
      assert(not target_is_narrow);
      let t = get_temp_type ~env s in
      let pointedtype = match t.ct_ty with
	Tpointer t -> t
      | _ -> assert false
      in
      let source_b, source_v = translate_variable ~env s in
      let offset = match get_field_offset ~genv:env.global_env pointedtype field with
	StrOfsNormal o -> o
      | StrOfsBitfield _ -> failwith "SF299: taking address of bitfield"
      in
      let new_lv = ILSlvPtr(source_b,source_v) in
      [
	ILSstmtAssign2(target_b, target_v, ILSexpAddress(new_lv,field))
     ]
  | IL2expAddress(IL2lvTemp _, _) -> failwith "SF360: taking address of rvalue temp"
  | IL2expAddress(IL2lvVar(k,vid,ty), field) -> 
      assert(not target_is_narrow);
      let offset =
	match get_field_offset ~genv:env.global_env ty field with
	  StrOfsNormal o -> o
	| StrOfsBitfield _ -> failwith "SF311: taking address of bitfield"
      in
      let temp = get_tempid ~env (ILStypeOfs typ) in
      if field = [] then
	[ ILSstmtAssign(target_b,ILSexpAddress(ILSlvVar(k,vid,ty),[]));
	  ILSstmtAssign(target_v,ILSexpConstant(CTTconstInteger offset)) ]
      else
(*	let temp = get_tempid ~env (ILStypeBase ty) in
	[ ILSstmtAssign(temp,ILSexpAddress(ILSlvVar(vid,ty),[]));
	  ILSstmtAssign(target_b,ILSexpCoerce1(typ,temp)); (* set cast-flag *)
	  ILSstmtAssign(target_v,ILSexpConstant(CTTconstInteger offset)) ]*)
	[ ILSstmtAssign2(target_b, target_v,
			 ILSexpAddress(ILSlvVar(k,vid,ty),field)) ]

  | IL2expArgument(s) ->
      if target_is_narrow then begin
	[ ILSstmtAssign(target_v,ILSexpArgument(s)) ]
      end else begin
	[ ILSstmtAssign(target_b,ILSexpArgumentB(s));
	  ILSstmtAssign(target_v,ILSexpArgumentV(s)) ]
      end
  | IL2expIdent(s) ->
      let source_b, source_v = translate_variable ~env s in
      if is_toonarrow typ then
	[ ILSstmtAssign(target_v,ILSexpIdent(source_v)) ]
      else
	[ ILSstmtAssign(target_b,ILSexpIdent(source_b));
	  ILSstmtAssign(target_v,ILSexpIdent(source_v)) ]
  in
  list_map (make_ils ~loc) new_insns

let make_sequence ~loc stmts = 
  let l = 
    Util.map_flatten
      (function
	 { locterm_v = ILSstmtSequence l } -> l
	| s -> [s])
      stmts
  in
  match l with
    [s] -> s
  | _ -> make_ils ~loc (ILSstmtSequence l)

let make_parallel ~loc stmts = 
  let l = 
    Util.map_flatten
      (function
	  { locterm_v = ILSstmtParallel l } -> l
	| s -> [s])
      stmts
  in
  match l with
    [s] -> s
  | _ -> make_ils ~loc (ILSstmtParallel l)

let round_to_builtintype bt v =
  match bt with
    Tdouble | Tfloat | Tlongdouble -> begin
      match v with
	ILSinitofsFloat _ -> v
      | ILSinitofsInt i -> ILSinitofsFloat (float_of_big_int i)
    end
  | _ -> begin
      let i = 
	match v with
	  ILSinitofsInt i -> i
	| ILSinitofsFloat f -> big_int_of_float f
      in
      let bits = Fsc_config.bits_of_byte * Ctt_abstree.size_of_builtin_type bt in
      let i = 
	if is_signed_builtin_type bt then
	  round_to_signed_binary_big_int i bits
	else
	  round_to_unsigned_binary_big_int i bits
      in
      ILSinitofsInt i
  end

let round_to_type t v = 
  match t.ct_ty with
    Tbuiltin bt -> round_to_builtintype bt v
  | Tpointer bt -> round_to_builtintype builtintype_ptrdiff_t v
  | _ -> assert false

let shift_mask = big_int_of_int (Fsc_config.sizeof_longlong * Fsc_config.bits_of_byte - 1)

let calculate_bop bop v1 v2 = 
  let apply_intonly f v1 v2 = 
    match v1, v2 with
      ILSinitofsInt i1, ILSinitofsInt i2 -> ILSinitofsInt (f i1 i2)
    | _ -> failwith "float cannot appear"
  in
  let apply_shift f v1 v2 = 
    match v1, v2 with
      ILSinitofsInt i1, ILSinitofsInt i2 -> 
	let i2 = int_of_big_int (land_big_int i2 shift_mask) in
	ILSinitofsInt (f i1 i2)
    | _ -> failwith "float cannot appear"
  in
  let apply_promote ff fi v1 v2 =
    match v1, v2 with
      ILSinitofsInt i1, ILSinitofsInt i2 -> ILSinitofsInt (fi i1 i2)
    | ILSinitofsInt i1, ILSinitofsFloat f2 -> ILSinitofsFloat (ff (float_of_big_int i1) f2)
    | ILSinitofsFloat f1, ILSinitofsInt i2 -> ILSinitofsFloat (ff f1 (float_of_big_int i2))
    | ILSinitofsFloat f1, ILSinitofsFloat f2 -> ILSinitofsFloat (ff f1 f2)
  in
  let to_bool = function
      true -> ILSinitofsInt unit_big_int
    | false -> ILSinitofsInt zero_big_int
  in
  let apply_promote_bool ff fi v1 v2 =
    to_bool
      (match v1, v2 with
	ILSinitofsInt i1, ILSinitofsInt i2 -> (fi i1 i2)
      | ILSinitofsInt i1, ILSinitofsFloat f2 -> (ff (float_of_big_int i1) f2)
      | ILSinitofsFloat f1, ILSinitofsInt i2 -> (ff f1 (float_of_big_int i2))
      | ILSinitofsFloat f1, ILSinitofsFloat f2 -> (ff f1 f2)
      )
  in
  let apply_boolfunc f v1 v2 =
    let v = function
	ILSinitofsInt i  when eq_big_int zero_big_int i -> false
      | ILSinitofsFloat f when f = 0.0 -> false
      | _ -> true
    in
    to_bool (f (v v1) (v v2))
  in
  match bop with
    ILbinPlusVV | ILbinPlusPV ->
      apply_promote ( +. ) add_big_int v1 v2
  | ILbinMinusVV | ILbinMinusPP | ILbinMinusPV ->
      apply_promote ( -. ) sub_big_int v1 v2
  | ILbinTimes ->
      apply_promote ( *. ) mult_big_int v1 v2
  | ILbinDiv ->
      apply_promote ( /. ) Native_int_semantics.div_big_int v1 v2
  | ILbinModulo ->
      apply_intonly Native_int_semantics.mod_big_int v1 v2
  | ILbinEqual -> apply_promote_bool ( = ) eq_big_int v1 v2
  | ILbinNotEqual -> apply_promote_bool ( <> ) neq_big_int v1 v2
  | ILbinGtrEqual -> apply_promote_bool ( >= ) ge_big_int v1 v2
  | ILbinLessEqual -> apply_promote_bool ( <= ) le_big_int v1 v2
  | ILbinGtrThan -> apply_promote_bool ( > ) gt_big_int v1 v2
  | ILbinLessThan -> apply_promote_bool ( < ) lt_big_int v1 v2
  | ILbinLshift -> apply_shift shift_left_big_int_positive_int v1 v2
  | ILbinRshift -> apply_shift shift_right_big_int_positive_int v1 v2
  | ILbinLogAnd -> apply_boolfunc ( && ) v1 v2
  | ILbinLogOr -> apply_boolfunc ( || ) v1 v2
  | ILbinIntAnd -> apply_intonly land_big_int v1 v2
  | ILbinIntOr -> apply_intonly lor_big_int v1 v2
  | ILbinIntXor -> apply_intonly lxor_big_int v1 v2
      
let calculate_uop uop v1 =
  match uop, v1 with
    UnaryPlus, v1 -> v1
  | UnaryMinus, ILSinitofsInt v1 -> ILSinitofsInt(minus_big_int v1)
  | UnaryMinus, ILSinitofsFloat v1 -> ILSinitofsFloat(-. v1)
  | LogNot, ILSinitofsInt v1 -> ILSinitofsInt(if v1 ==! zero_big_int then unit_big_int else zero_big_int)
  | LogNot, ILSinitofsFloat v1 -> ILSinitofsInt(if v1 = 0.0 then unit_big_int else zero_big_int)
  | IntNot, ILSinitofsInt v1 -> ILSinitofsInt(lnot_big_int v1)
  | IntNot, ILSinitofsFloat v1 -> assert false

let zero_offset = ILSinitofsInt zero_big_int

let rec calc_offset ~genv typ = function
    [] -> zero_big_int
  | (id,ct)::tl -> begin
      match typ.ct_ty with
	Tstruct sid -> begin
	  let sd = Earray.get genv.struct_table sid in
	  let (fty, fofs) = List.assoc id sd.str_fields_byname in
	  assert (C_typing.equal_type fty ct);
	  match fofs with
	    StrOfsNormal s -> s +! calc_offset ~genv ct tl
	  | _ -> assert false
	end
      | _ -> 
	  failwith_p "panic:separate_fatpointer:1930: bad type passed: %a" Il_formatter.pp_ctt_type typ
  end

let translate_constant_exp ~genv e = 
  (* Note:
     constant expressions are already checked strictly in 
     Separate_side_effect, which should prevent 
     any "assert false" cases to happen. *)
  let rec eval' { il2_cexp_type = ty; il2_cexp_t = e } =
    match e with
      IL2cexpAddress(IL2lvVar (k,v,t),f) ->
	let ofs = calc_offset ~genv t f in
	ty, ILSinitbaseVar (k, v, t), ILSinitofsInt ofs
    | IL2cexpAddress _ ->
	assert false
    | IL2cexpAddFieldOfs(ce1, f) -> begin
	let ty1, base1, ofs1 = eval ce1 in
	let pty = match ty1.ct_ty with Tpointer t -> t | _ -> assert false in
	let ofs2 = calc_offset ~genv pty f in
	match ofs1 with
	  ILSinitofsInt i -> ty, base1, ILSinitofsInt (i +! ofs2)
	| _ -> assert false
    end
    | IL2cexpCoerce({ct_ty = Tbuiltin bt} as ct,cexp) ->
	(* precondition: ct is wide enough *)
	let _, v, ofs = eval cexp in
	ty, v, round_to_builtintype bt ofs
    | IL2cexpCoerce({ct_ty = Tpointer tp} as ct, cexp) ->
	let _, v, ofs = eval cexp in
	ty, v, round_to_builtintype builtintype_ptrdiff_t ofs
    | IL2cexpCoerce _ -> assert false

    | IL2cexpConstant(CTTconstInteger i) ->
	ty, ILSinitbaseNone, round_to_type ty (ILSinitofsInt i)
    | IL2cexpConstant(CTTconstFloat f) ->
	ty, ILSinitbaseNone, round_to_type ty (ILSinitofsFloat f)
    | IL2cexpConstant(CTTconstNull) ->
	ty, ILSinitbaseNone, zero_offset
    | IL2cexpConstant(CTTconstString s) ->
	ty, ILSinitbaseString s, zero_offset
    | IL2cexpConstant(CTTconstAbstract s) ->
	assert false
    | IL2cexpConstant(CTTconstTypeInfo t) ->
	ty, ILSinitbaseTypeInfo t, zero_offset

    | IL2cexpBinop(bop, e1, e2) -> begin
	let _, b1, o1 = eval e1 in
	let _, b2, o2 = eval e2 in

	let ofs_r () = round_to_type ty (calculate_bop bop o1 o2) in
	
	match bop with
	  (ILbinPlusPV | ILbinMinusPV) -> begin
	    assert (b2 = ILSinitbaseNone);
	    match ty.ct_ty, o1, o2 with
	      Tpointer target_t, ILSinitofsInt _, ILSinitofsInt o2 ->
		let s = sizeof_genv ~genv target_t in
		let new_o2 = ILSinitofsInt (mult_big_int s o2) in
		dprintf 9 "eval: PV %s -> %a"
		  (string_of_big_int o2)
		  Ils_formatter.pp_ils_initializer_offset new_o2;
		let ofs_r = round_to_type ty (calculate_bop bop o1 new_o2) in
		ty, b1, ofs_r
	    | _ -> failwith_p "assert509: %a" Il_formatter.pp_ctt_type ty
	  end
	| ILbinMinusPP -> begin
	    begin
	      match b1, b2 with
		ILSinitbaseNone, ILSinitbaseNone -> ()
	      | ILSinitbaseVar(_,i1,_), ILSinitbaseVar(_,i2,_) -> assert (i1 = i2)
	      | _ -> assert false
	    end;
	    match (locval e1).il2_cexp_type.ct_ty, o1, o2 with
	      Tpointer target_t, ILSinitofsInt o1, ILSinitofsInt o2 ->
		let s = sizeof_genv ~genv target_t in
		let new_o = ILSinitofsInt ((o1 -! o2) /! s) in
		dprintf 9 "eval: PP %s - %s -> %a"
		  (string_of_big_int o1) (string_of_big_int o2)
		  Ils_formatter.pp_ils_initializer_offset new_o;
		ty, ILSinitbaseNone, new_o
	    | _ -> failwith_p "assert509_2: %a" Il_formatter.pp_ctt_type ty
	end
	| ILbinPlusVV -> begin
	    (* linear operator *)
	    match b1, b2 with
	      b, ILSinitbaseNone -> ty, b, ofs_r ()
	    | ILSinitbaseNone, b -> ty, b, ofs_r ()
	    | _ -> assert false
	end
	| ILbinMinusVV -> begin
	    match b1, b2 with
	      b, ILSinitbaseNone -> ty, b, ofs_r ()
	    | ILSinitbaseVar(_,i1,_), ILSinitbaseVar(_,i2,_) ->
		assert (i1 = i2);
		ty, ILSinitbaseNone, ofs_r ()
	    | _ -> assert false
	end
	| _ ->
	    assert (b1 = ILSinitbaseNone);
	    assert (b2 = ILSinitbaseNone);
	    ty, ILSinitbaseNone, ofs_r ()
    end
    | IL2cexpUnaryop(uop,e1) ->
	let _, b1, o1 = eval e1 in
	assert (b1 = ILSinitbaseNone);
	ty, ILSinitbaseNone, round_to_type ty (calculate_uop uop o1)
  and eval e = 
    let (ty, b, o) as r = eval' (locval e) in
    dprintf 7 "eval: %a -> %a, %a" Il2_formatter.pp_il2_cexp e
      Ils_formatter.pp_ils_initializer_base b Ils_formatter.pp_ils_initializer_offset o;
    r
  in
  eval e

let rec translate_initializer ~genv i = 
  locmap (function
      IL2initConstantExp(t', e) ->
	let t, b, o = translate_constant_exp ~genv e in
	if not (C_typing.equal_type t t') then
	  failwith_p "assert543: %a %a" Il_formatter.pp_ctt_type t Il_formatter.pp_ctt_type t';
	ILSinitConstant(t, b, o)
    | IL2initPostFill ->
	ILSinitPostFill
    | IL2initArray(t, l) ->
	ILSinitArray(t, list_map (translate_initializer ~genv) l)
    | IL2initStruct(t, l) ->
	ILSinitStruct(t, list_map (fun (t, b) -> t, translate_initializer ~genv b) l))
    i

let rec translate_stmt ~env stmt = 
  let loc = locget stmt in
  match (locval stmt).il2_t with
    IL2stmtSequence(ils) -> make_sequence ~loc (list_map (translate_stmt ~env) ils)
  | IL2stmtParallel(ils) -> make_parallel ~loc (list_map (translate_stmt ~env) ils)
  | IL2stmtAssign(tid,typ,exp) ->
      make_parallel ~loc (translate_assign ~env ~loc tid typ exp)
  | IL2stmtGoto(target) -> make_ils ~loc (ILSstmtGoto target)
  | IL2stmtReturn(None) ->  begin
      (* IL2stmtReturn may be fall-through return from function. check the result type. *)
      let result_type = env.result_type in
      match result_type.ct_ty with
	Tvoid -> make_ils ~loc ILSstmtReturn0
      | Tbuiltin _ | Tpointer _ ->
	  Format.eprintf "Warning: control reaches bottom of non-void function in %s@." env.function_name;
	  if is_toonarrow result_type then begin
	    let tempv = get_tempid ~env (ILStypeVal result_type) in
	    make_parallel ~loc
	      ([ make_ils ~loc (ILSstmtAssign (tempv, ILSexpConstant(constant_zero)));
		 make_ils ~loc (ILSstmtReturn1 tempv)])
	  end else begin
	    let tempb = get_tempid ~env (ILStypeBase result_type) in
	    let typeo = if is_pointer result_type then ILStypeOfs result_type else ILStypeVal result_type in
	    let tempo = get_tempid ~env typeo in
	    make_parallel ~loc
	      ([ make_ils ~loc (ILSstmtAssign (tempb, ILSexpConstant(CTTconstNull)));
		 make_ils ~loc (ILSstmtAssign (tempo, ILSexpConstant(constant_zero)));
		 make_ils ~loc (ILSstmtReturn2 (tempb, tempo))])
	  end
      | Tstruct _ ->
	  failwith_p 
	    "Error 579: control reaches bottom of struct-returning function in %s@." env.function_name;
      | Tarray _ | Tfunction _ | Tabstract _ -> assert false
  end
  | IL2stmtAbort reason ->
      make_ils ~loc (ILSstmtAbort (ILSerrAborted reason))
  | IL2stmtReturn(Some v) -> 
      let target_b, target_v = translate_variable ~env v in
      if is_toonarrow (get_temp_type ~env v) then
	make_ils ~loc (ILSstmtReturn1 target_v)
      else
	make_ils ~loc (ILSstmtReturn2 (target_b,target_v))
  | IL2stmtRead(target,typ,lv,fields) ->
      let lv = translate_lv ~env lv in
      let target_b, target_v = translate_variable ~env target in
      if is_toonarrow typ then
	make_ils ~loc (ILSstmtRead1 (target_v,lv,fields))
      else
	make_ils ~loc (ILSstmtRead2 (target_b,target_v,lv,fields))
  | IL2stmtWrite(lv,fields,source) ->
      let lv = translate_lv ~env lv in
      let source_b, source_v = translate_variable ~env source in
      if is_toonarrow (get_temp_type ~env source) then
	make_ils ~loc (ILSstmtWrite1 (lv,fields,source_v))
      else
	make_ils ~loc (ILSstmtWrite2 (lv,fields,source_b,source_v))
  | IL2stmtSwitch(tid,jump_list) ->
      let target_b, target_v = translate_variable ~env tid in
      make_ils ~loc (ILSstmtSwitch (target_v, jump_list))
  | IL2stmtDeclAutoScalar(vt, ct, id, Some i) ->
      let source_b, source_v = translate_variable ~env i in
      if is_toonarrow (get_temp_type ~env i) then
	make_sequence ~loc
	  [ make_ils ~loc (ILSstmtDeclScalar (vt, ct, id));
	    make_ils ~loc (ILSstmtWrite1 (ILSlvVar(vt, id, ct), [], source_v)) ]
      else
	make_sequence ~loc
	  [ make_ils ~loc (ILSstmtDeclScalar (vt, ct, id));
	    make_ils ~loc (ILSstmtWrite2 (ILSlvVar (vt, id, ct), [], source_b, source_v)) ]
  | IL2stmtDeclAutoScalar(vt, ct, id, None) ->
      make_ils ~loc (ILSstmtDeclScalar (vt, ct, id))
  | IL2stmtDeclBulk(vt, ct, id, None) ->
      make_ils ~loc (ILSstmtDeclBulk (vt, ct, id))
  | IL2stmtDeclBulk(vt, ct, id, Some inits) ->
      let inits = translate_initializer ~genv:env.global_env inits in
      make_sequence ~loc
	[ make_ils ~loc (ILSstmtDeclBulk (vt, ct, id));
	  make_ils ~loc (ILSstmtInitialize (vt, ct, id, inits)) ]
  | IL2stmtIf(iftyp,id,target) ->
      let source_b, source_v = translate_variable ~env id in
      let source_type = get_temp_type ~env id in
      if is_pointer (get_temp_type ~env id) then
	let instrs, id = get_boolean ~env id in
	make_sequence ~loc
	  [ make_parallel ~loc (list_map (make_ils ~loc) instrs);
	    make_ils ~loc (ILSstmtIf (iftyp,id,target)) ]
      else
	make_ils ~loc (ILSstmtIf (iftyp, source_v, target))

let make_varmap vars = 
  let idcntr = ref 0 in
  let get () = let r = !idcntr in incr idcntr; r in
  let varmap = Array.create (Array.length vars) (-1, -1) in
  let vartypes = Array.create (Array.length vars * 2) 
      { original_name = None;
	variable_type = ILStypeVal type_void;
	storage_class = Auto; } (* dummy *)
  in
  Array.iteri
    (fun id v ->
      let t = v.Il.variable_type in
      if is_toonarrow t || is_struct_type t then begin
	let newid = get () in
	dprintf 6 "%d: from %d (%s)" newid id (Option.default "-" v.Il.original_name);
	varmap.(id) <- (-1, newid);
	vartypes.(newid) <- {
	  original_name = Option.map (fun s -> "N" ^ s) v.Il.original_name;
	  variable_type = ILStypeVal t;
	  storage_class = v.Il.storage_class;
	}
      end else begin
	let newidb = get () in
	let newido = get () in
	varmap.(id) <- (newidb, newido);
	dprintf 6 "(%d,%d): from %d (%s)" newidb newido id (Option.default "-" v.Il.original_name);
	vartypes.(newidb) <- {
	  original_name = Option.map (fun s -> "B" ^ s)  v.Il.original_name;
	  variable_type = ILStypeBase t;
	  storage_class = v.Il.storage_class;
	};
	vartypes.(newido) <- {
	  original_name = Option.map (fun s -> if is_pointer t then "O" ^ s else "V" ^ s) v.Il.original_name;
	  variable_type = 
	  (if is_pointer t then ILStypeOfs t else ILStypeVal t);
	  storage_class = v.Il.storage_class;
	}
      end)
    vars;
  let vartypes = Array.sub vartypes 0 (!idcntr) in
  !idcntr, varmap, vartypes
	
let convert_basicblock ~env (b : Il2.il2_basic_block) =
  {
   location = b.Il2.location;
   predecessor = b.Il2.predecessor;
   successor = b.Il2.successor;
   immediate_dominator = -1;
   nest_level = 0;
   phi_function = [];
   code  = list_map (translate_stmt ~env) b.Il2.code
 }


let make_environment env old_venv = 
  let new_vartypes = Array.create env.maxvar
      { original_name = None;
	variable_type = ILStypeVal type_void;
	storage_class = Auto; } (* dummy *)
  in
  Array.blit old_venv 0 new_vartypes 0 (Array.length old_venv);
  for i = (Array.length old_venv) to (env.maxvar - 1) do
    let att = 
      { original_name = None;
	variable_type = List.assoc i env.additional_variables;
	storage_class = Register; }
    in
    new_vartypes.(i) <- att
  done;
  new_vartypes

let scan_variables f = 
  let a = Earray.empty () in
  let rec iter_stmt i = 
    match (locval i).il2_t with
    | IL2stmtDeclAutoScalar _
    | IL2stmtDeclBulk _
    | IL2stmtIf _
    | IL2stmtSwitch _
    | IL2stmtGoto _
    | IL2stmtReturn _ 
    | IL2stmtAbort _ 
    | IL2stmtWrite _ -> ()
    | IL2stmtAssign (t, ct, _) 
    | IL2stmtRead(t, ct, _, _) -> Earray.set a t ct
    | IL2stmtSequence l
    | IL2stmtParallel l -> List.iter iter_stmt l
  in
  Array.iter (fun b -> List.iter iter_stmt b.Il2.code) f.Il2.body;
  let l = Earray.length a in
  Array.init l 
    (fun i -> 
      let t = try
	Earray.get a i
      with
	Not_found -> type_void (* failwith (Printf.sprintf "sfp1306: %d not found" i) *) (* var 0 not exists! *)
      in
      { original_name = f.Il2.variable_environment.(i).Il2.original_name; 
	variable_type = t;
	storage_class = Register})

let scan_args typ args = 
  let tlist = match typ.ct_ty with Tfunction(at,_,_) -> at | _ -> assert false in
  Array.of_list (List.combine args tlist)

let convert_function ~genv ~id ~typ ~args f = 
  let orig_vars = scan_variables f in
  let args = scan_args typ args in
  let maxvar, varmap, vartypes = make_varmap orig_vars in
  let env = {
    function_name = id;
    global_env = genv;
    original_env = orig_vars;
    varmap = varmap;
    additional_variables = [];
    maxvar = maxvar;
    result_type = match typ.ct_ty with Tfunction(at,var,rt) -> rt | _ -> assert false;
  } in
  let new_body = Array.map (convert_basicblock ~env) f.Il2.body in
  let newenv = make_environment env vartypes in
  {
   body = new_body;
   variable_environment = newenv;
   arguments = args;
   max_variable_number = (Array.length newenv);
   function_attributes = f.Il2.function_attributes
 }

let translate_program ~genv p = 
  Record_globalnames.clear ();
  locmap_list
    (function
	IL2declFunction (gs,typ,id,args,body) ->
	  ILdeclFunction(gs,typ,id,args,convert_function ~genv ~id ~typ ~args body)
      |	IL2declVariable (gs,typ,id,init) ->
	  ILdeclVariable(gs,typ,id,Option.map (translate_initializer ~genv) init))
    p
