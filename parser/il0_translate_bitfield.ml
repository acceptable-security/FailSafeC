(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

open Util
open Big_int_infix
open Ctt_abstree
open Il
open Il0
open Locterm

type environment = 
    {
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

let type_of_lvalue ~env = function
    ILlvVar(v, t) -> t
  | ILlvPtr(id) -> begin
      let t = type_of_tempid ~env id in
      match t.ct_ty with
	Tpointer t -> t
      | Tarray(t, _) -> t
      | _ -> assert false
  end
  | ILlvTemp(id) ->
      type_of_tempid ~env id

let get_struct_desc ~env id = Ctt_abstree.get_struct_desc ~genv:env.genv id

let struct_id_of_type t =
  match t.ct_ty with
    Tstruct id -> id
  | _ -> assert false

let two_big_int = big_int_of_int 2
let bits_of_int = Fsc_config.sizeof_int * Fsc_config.bits_of_byte

(* the startbit indicates the highermost bit of the field,
   counted from LSB. *)
let calc_bitmask_unsigned ~wid ~startbit = 
  let shift_width = startbit + 1 - wid in
  let bitmask = 
    pred_power2_big_int wid
  in
  big_int_of_int shift_width, bitmask

let calc_shiftwidth_signed ~wid ~startbit = 
  bits_of_int - 1 - startbit, bits_of_int - wid

let bitfield_name ~ofs ~bytepos = 
  let bytepos = ofs +! (big_int_of_int bytepos) in
  "__bit_" ^ string_of_big_int bytepos

(**************** initializer ****************)

let make_il_cexp_binop ~loc t bin e1 e2 = 
  locput ~loc
    { il_cexp_type = t;
      il_cexp_t = ILcexpBinop(bin, e1, e2) }

let make_il_cexp_of_big_int ~loc t i = 
  locput ~loc
    { il_cexp_type = t;
      il_cexp_t = ILcexpConstant (CTTconstInteger i) }

let rec translate_il_initializer ~genv i = 
  locmap (function
    (ILinitPostFill | ILinitConstantExp _) as init -> init
  | ILinitArray (t, inits) -> begin
      let inits = Util.list_map (translate_il_initializer ~genv) inits in
      ILinitArray (t, inits)
  end
  | ILinitStruct (t, inits) ->
      ILinitStruct(t, translate_il_initializer_struct ~genv t inits)
  | ILinitAbstractCtt _ -> assert false)
    i

and translate_il_initializer_struct ~genv t inits = 
  let str_id = struct_id_of_type t in
  let desc = Ctt_abstree.get_struct_desc ~genv str_id in
  let rec iter f i = 
    match f, i with
      [], [] -> []
    | (_, NormalField nf)::f2 , (id,i1)::i2 ->
	assert (nf.sf_id = id);
	(id, translate_il_initializer ~genv i1) :: iter f2 i2
    | (ofs, BitField bf)::f2, i ->
	let newinits, resti = translate_il_initializer_bitfield ~genv ~ofs ~loc:Locterm.dummy_location bf i in (* TODO LOC *)
	newinits @ iter f2 resti
    | (_::_, []) | ([], _::_) -> assert false
  in
  iter desc.str_fields inits

and translate_il_initializer_bitfield ~genv ~ofs ~loc bf i =
  let size = int_of_big_int bf.s_bf_size in
  assert (size mod Fsc_config.sizeof_int = 0);
  let numwords = size / Fsc_config.sizeof_int in
  let table = Array.create numwords [] in
  let rec iter bf i =
    match bf, i with
      [], i -> i
    | ((None, _, _, _)::tl), i ->
	(* skipping unnamed field *)
	iter tl i
    | ((Some fid, cty, wid, (startbyte, startbit))::tl), (id, i)::itl -> begin
	assert (fid = id);
	match locval i with
	  ILinitPostFill ->
	    iter tl itl
	| ILinitConstantExp(cty, e) ->
	    let loc = locget i in
	    let shift_width, bitmask = calc_bitmask_unsigned ~wid ~startbit
	    in
	    let e = 
	      make_il_cexp_binop ~loc cty
		ILbinLshift
		(make_il_cexp_binop ~loc cty
		   ILbinIntAnd e (make_il_cexp_of_big_int ~loc type_int bitmask))
		(make_il_cexp_of_big_int ~loc type_int shift_width)
	    in
	    assert (startbyte mod Fsc_config.sizeof_int = 0);
	    let wordidx = startbyte / Fsc_config.sizeof_int in
	    table.(wordidx) <- e :: table.(wordidx);
	    iter tl itl
	| ILinitStruct _ | ILinitArray _ -> assert false
	| ILinitAbstractCtt _ -> assert false
    end
    | _::_, [] -> assert false
  in
  let i_rest = iter bf.s_bf_fields i in
  let inits = Array.mapi 
      (fun o i ->
	let exp = match i with
	  [] -> make_il_cexp_of_big_int ~loc type_int zero_big_int
	| h::t ->
	    List.fold_left
	      (fun l r -> make_il_cexp_binop ~loc:(locget l) type_int ILbinIntOr r l)
	      h t
	      (* entries in table are "appended to the head" *)
	in
	let field_name = bitfield_name ~ofs ~bytepos:(o * Fsc_config.sizeof_int) in
	(field_name, locput ~loc (ILinitConstantExp (type_int, exp))))
      table
  in
  Array.to_list inits, i_rest

let translate_il_local_initializer ~env init =
  translate_il_initializer ~genv:env.genv init

let translate_il_global_initializer = translate_il_initializer

(**************** expression ****************)

let parse_field ~env id fields = 
  let rec iter acc id = function
      [] -> None, fields
    | (f, t as bind) :: tl -> begin
	let desc = get_struct_desc ~env id in
	let newt, ofs = List.assoc f desc.str_fields_byname in
	match ofs, tl with
	  StrOfsBitfield(ofs, wid, start), [] ->
	    (* final entry: bit-field *)
	    let t = 
	      match newt.ct_ty with
		Tbuiltin bt -> bt
	      | _ -> assert false
	    in
	    Some (t, ofs, wid, start), List.rev acc
	| StrOfsNormal _, [] ->
	    (* final entry: normal field *)
	    None, fields
	| StrOfsNormal _, _::_ -> begin
	    iter (bind :: acc) (struct_id_of_type t) tl
	end
	| StrOfsBitfield _, _::_ ->
	    (* referencing field of bitfield *)
	    assert false
    end
  in
  iter [] id fields

let translate_expr ~env ~orig = function
    ILexpAddress(lv, []) as expr ->
      expr
  | ILexpAddress(lv, flds) as expr -> begin
      let t = type_of_lvalue ~env lv in
      match parse_field ~env (struct_id_of_type t) flds with
      | Some _, _ -> assert false
      |	None, _ -> expr
  end
  | expr -> expr

let translate_read ~env ~loc ~orig tid cty lv fields = 
  match fields with
    [] -> orig
  | _::_ ->
      let t = type_of_lvalue ~env lv in
      match parse_field ~env (struct_id_of_type t) fields with
	None, _ -> orig
      | Some (t, ofs, wid, (bytepos, startbit)), fields -> begin
	  let fields = fields @ [bitfield_name ~ofs ~bytepos, type_int] in
	  let ils = match t with
	    Tint ->
	      let lshift, rshift = calc_shiftwidth_signed ~wid ~startbit in
	      let tid0 = make_tempid ~env type_int in
	      let tid1 = make_tempid ~env type_int in
	      let tid_lshift = make_tempid ~env type_int in
	      let tid_rshift = make_tempid ~env type_int in
	      let tid2 = make_tempid ~env type_int in
	      [ make_il0 ~loc (IL0stmtReadToTemp(tid0, type_int, lv, fields));
		make_il0 ~loc (IL0stmtDefTemp(tid_lshift, type_int,
					 ILexpConstant (CTTconstInteger (big_int_of_int lshift))));
		make_il0 ~loc (IL0stmtDefTemp(tid1, type_int,
					 ILexpBinop(ILbinLshift, tid0, tid_lshift)));
		make_il0 ~loc (IL0stmtDefTemp(tid_rshift, type_int,
					 ILexpConstant (CTTconstInteger (big_int_of_int rshift))));
		make_il0 ~loc (IL0stmtDefTemp(tid2, type_int,
					 ILexpBinop(ILbinRshift, tid1, tid_rshift)));
		make_il0 ~loc (IL0stmtDefTemp(tid, cty, ILexpCoerce(cty, tid2))) ]
	  | Tuint ->
	      let shift_width, mask = calc_bitmask_unsigned ~wid ~startbit in
	      let tid0 = make_tempid ~env type_int in
	      let tid1 = make_tempid ~env type_int in
	      let tid2 = make_tempid ~env type_int in
	      let tid_shift = make_tempid ~env type_int in
	      let tid_mask = make_tempid ~env type_int in
	      [ make_il0 ~loc (IL0stmtReadToTemp(tid0, type_int, lv, fields));
		make_il0 ~loc (IL0stmtDefTemp(tid_shift, type_int,
					 ILexpConstant (CTTconstInteger shift_width)));
		make_il0 ~loc (IL0stmtDefTemp(tid1, type_int,
					 ILexpBinop(ILbinRshift, tid0, tid_shift)));
		make_il0 ~loc (IL0stmtDefTemp(tid_mask, type_int,
					 ILexpConstant (CTTconstInteger mask)));
		make_il0 ~loc (IL0stmtDefTemp(tid2, type_int,
					 ILexpBinop(ILbinIntAnd, tid1, tid_mask)));
		make_il0 ~loc (IL0stmtDefTemp(tid, cty, ILexpCoerce(cty, tid2))) ]
	  | _ -> assert false
	  in
	  IL0stmtParallel ils
      end
      
let translate_write ~env  ~loc~orig lv fields tid =
  match fields with
    [] -> orig
  | _::_ ->
      let t = type_of_lvalue ~env lv in
      match parse_field ~env (struct_id_of_type t) fields with
	None, _ -> orig
      | Some (t, ofs, wid, (bytepos, startbit)), fields -> begin
	  let fields = fields @ [bitfield_name ~ofs ~bytepos, type_int] in
	  let shift_width, mask = calc_bitmask_unsigned ~wid ~startbit in
	  let tid0 = make_tempid ~env type_unsigned_int in
	  let tid1 = make_tempid ~env type_unsigned_int in
	  let tid2 = make_tempid ~env type_unsigned_int in
	  let tid3 = make_tempid ~env type_int in
	  let tid4 = make_tempid ~env type_unsigned_int in
	  let tid5 = make_tempid ~env type_unsigned_int in
	  let tid_shift = make_tempid ~env type_int in
	  let tid_mask = make_tempid ~env type_int in
	  let tid_mask1 = make_tempid ~env type_int in
	  let tid_shift_mask = make_tempid ~env type_int in
	  IL0stmtParallel
	    [ make_il0 ~loc (IL0stmtDefTemp(tid0, type_unsigned_int, 
				       ILexpCoerce(type_unsigned_int, tid)));
	      make_il0 ~loc (IL0stmtDefTemp(tid_mask, type_unsigned_int,
				       ILexpConstant (CTTconstInteger mask)));
	      make_il0 ~loc (IL0stmtDefTemp(tid1, type_unsigned_int,
				       ILexpBinop(ILbinIntAnd, tid0, tid_mask)));
	      make_il0 ~loc (IL0stmtDefTemp(tid_shift, type_int,
				       ILexpConstant (CTTconstInteger shift_width)));
	      make_il0 ~loc (IL0stmtDefTemp(tid2, type_unsigned_int,
				       ILexpBinop(ILbinLshift, tid1, tid_shift)));
	      make_il0 ~loc (IL0stmtReadToTemp(tid3, type_int, lv, fields));
	      make_il0 ~loc (IL0stmtDefTemp(tid_mask1, type_unsigned_int,
				       ILexpBinop(ILbinLshift, tid_mask, tid_shift)));
	      make_il0 ~loc (IL0stmtDefTemp(tid_shift_mask, type_unsigned_int,
				       ILexpUnaryop(IntNot, tid_mask1)));
	      make_il0 ~loc (IL0stmtDefTemp(tid4, type_unsigned_int,
				       ILexpBinop(ILbinIntAnd, tid3, tid_shift_mask)));
	      make_il0 ~loc (IL0stmtDefTemp(tid5, type_unsigned_int,
				       ILexpBinop(ILbinIntOr, tid4, tid2)));
	      make_il0 ~loc (IL0stmtWrite(lv, fields, tid5)) ]
      end

let rec translate_il0 ~env i = 
  let loc = locget i in
  let desc = match i.locterm_v.il0_t with
  | IL0stmtDeclBulk(lsc, ty, id, initopt) ->
      IL0stmtDeclBulk(lsc, ty, id, Option.map (translate_il_local_initializer ~env) initopt)
  | IL0stmtSequence(ils) ->
      IL0stmtSequence(list_map (translate_il0 ~env) ils)
  | IL0stmtParallel(ils) ->
      IL0stmtParallel(list_map (translate_il0 ~env) ils)
  | IL0stmtDefTemp(tid, typ, expr) as orig ->
      IL0stmtDefTemp(tid, typ, translate_expr ~env ~orig expr)
  | IL0stmtReadToTemp(tid, cty, lv, fields) as orig ->
      translate_read ~env ~loc ~orig tid cty lv fields
  | IL0stmtWrite(lv, fields, tid) as orig ->
      translate_write ~env ~loc ~orig lv fields tid

  | ( IL0stmtLabel _
      | IL0stmtDeclAutoScalar _
      | IL0stmtIf _
      | IL0stmtSwitch _
      | IL0stmtGoto _
      | IL0stmtReturn _
      | IL0stmtAbort _) as orig -> orig
  in
  if i.locterm_v.il0_t == desc then i else make_il0 ~loc desc

let translate_funcbody ~genv fb = 
  let env = {
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
      IL0declFunction(gsc,cty,id,args,translate_funcbody ~genv fbody)
  | IL0declVariable(gsc,cty,id,initopt) ->
      IL0declVariable(gsc,cty,id,
		      Option.map (translate_il_global_initializer ~genv) initopt)

let rec translate_structure_fields ~genv = function
    [] -> []
  | (_, NormalField _) as f :: tl ->
      f :: translate_structure_fields ~genv tl
  | (ofs, BitField bf) :: tl -> begin
      let size = int_of_big_int bf.s_bf_size in
      assert (size mod Fsc_config.sizeof_int = 0);
      let words = size / Fsc_config.sizeof_int in
      let rec iter n = 
	if n = words then translate_structure_fields ~genv tl
	else
	  let name = bitfield_name ~ofs ~bytepos:(n * Fsc_config.sizeof_int) in
	  let f = 
	    NormalField
	      { sf_id = name;
		sf_type = type_int;
		sf_size = big_int_of_int Fsc_config.sizeof_int }
	  in
	  (ofs +! big_int_of_int n *! big_int_of_int Fsc_config.sizeof_int, f) :: iter (n + 1)
      in
      iter 0
  end
   
let translate_structure ~genv desc = 
  let desc = 
    { desc with str_fields = translate_structure_fields ~genv desc.str_fields } in
  C_typing.update_fields_cache desc

let translate_environment ~genv = 
  { genv with
    struct_table = 
    Earray.map (translate_structure ~genv) genv.struct_table }

let translate_program ~genv p = 
  let p = locmap_list (translate_declaration_desc ~genv) p in
  translate_environment ~genv, p
  
