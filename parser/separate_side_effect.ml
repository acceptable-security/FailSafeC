(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)

open Util
open Locterm
open Big_int_infix
open Ctt_abstree
open Ctt_formatter
open Cttm_abstree
open Cttm_expunge_assignment
open Topological_sort
open Il_formatter
open Il1
open ExtList

include Debug.Install (struct let category = 30 end)

open Il
open Il0

open Set_list

let print_list f sep l = String.concat sep (Util.list_map f l)

let cvt_binop = function
    CTTbinTimes       -> ILbinTimes
  | CTTbinDiv         -> ILbinDiv
  | CTTbinPlusVV      -> ILbinPlusVV
  | CTTbinMinusVV     -> ILbinMinusVV
  | CTTbinPostPlusVV  -> ILbinPlusVV
  | CTTbinPostMinusVV -> ILbinMinusVV
  | CTTbinPlusPV      -> ILbinPlusPV
  | CTTbinMinusPP     -> ILbinMinusPP
  | CTTbinMinusPV     -> ILbinMinusPV
  | CTTbinPostPlusPV  -> ILbinPlusPV
  | CTTbinPostMinusPV -> ILbinMinusPV
  | CTTbinModulo      -> ILbinModulo
  | CTTbinLshift      -> ILbinLshift
  | CTTbinRshift      -> ILbinRshift
  | CTTbinLogAnd      -> ILbinLogAnd
  | CTTbinLogOr       -> ILbinLogOr
  | CTTbinIntAnd      -> ILbinIntAnd
  | CTTbinIntOr       -> ILbinIntOr
  | CTTbinIntXor      -> ILbinIntXor
  | CTTbinLessThan    -> ILbinLessThan
  | CTTbinLessEqual   -> ILbinLessEqual
  | CTTbinGtrThan     -> ILbinGtrThan
  | CTTbinGtrEqual    -> ILbinGtrEqual
  | CTTbinEqual       -> ILbinEqual
  | CTTbinNotEqual    -> ILbinNotEqual

(****************************************************************)

type environment = 
{
 genv : Ctt_abstree.environment;
 mutable label_counter : int;
 mutable tempid_counter : int;
 tempid_types : c_type earray;
}

let new_environment ~genv = {
  genv = genv;
  label_counter = 0;
  tempid_counter = 0;
  tempid_types = Earray.empty ();
} 

let make_label ~env =
  let n = env.label_counter in
  let s = "$L"^ (string_of_int n) in
  env.label_counter <- env.label_counter + 1;
  s

let new_tempid ~env t = 
  env.tempid_counter <- env.tempid_counter + 1;
  Earray.set env.tempid_types env.tempid_counter t;
  env.tempid_counter

let gentemp ~loc ~env e t : temp_id * il0 = 
  let i = new_tempid ~env t in
  i, make_il0 ~loc (IL0stmtDefTemp(i, t, e))

let gentemp_read ~loc ~env (e, f) t : temp_id * il0 = 
  let i = new_tempid ~env t in
  i, make_il0 ~loc (IL0stmtReadToTemp(i, t, e, f))

let constant_one = CTTconstInteger unit_big_int
let constant_zero = CTTconstInteger zero_big_int

let is_constant_zero = function
    CTTconstInteger i -> i ==! zero_big_int
  | CTTconstFloat f -> f = 0.0
  | CTTconstString _ -> false
  | CTTconstTypeInfo _ -> false
  | CTTconstNull -> true
  | CTTconstAbstract _ -> assert false (* output only; should not appear in this stage *)

let rec translate_mexpr_jump ~env iftype target mexpr = 
  let loc = locget mexpr in
  let t_mexpr = mexpr.locterm_v.mexpr_type in
  assert (t_mexpr.ct_ty <> Tvoid);
  match iftype, mexpr.locterm_v.mexpr_t with
    IFTRUE, CTTMexpBinExpr(CTTbinLogAnd, e1, e2) ->
      let s1 = make_label ~env in
      enclose_sequence ~loc
	[translate_mexpr_jump ~env IFNOT s1 e1;
	 translate_mexpr_jump ~env IFTRUE target e2;
	 make_il0 ~loc (IL0stmtLabel s1)]
  | IFNOT, CTTMexpBinExpr(CTTbinLogAnd, e1, e2) ->
      enclose_sequence ~loc
	[translate_mexpr_jump ~env IFNOT target e1;
	 translate_mexpr_jump ~env IFNOT target e2]
  | IFTRUE, CTTMexpBinExpr(CTTbinLogOr, e1, e2) ->
      enclose_sequence ~loc
	[translate_mexpr_jump ~env IFTRUE target e1;
	 translate_mexpr_jump ~env IFTRUE target e2]
  | IFNOT, CTTMexpBinExpr(CTTbinLogOr, e1, e2) ->
      let s1 = make_label ~env in
      enclose_sequence ~loc
	[translate_mexpr_jump ~env IFTRUE s1 e1;
	 translate_mexpr_jump ~env IFNOT target e2;
	 make_il0 ~loc (IL0stmtLabel s1)]
  | IFTRUE, CTTMexpUnaryExpr(LogNot, e1) ->
      translate_mexpr_jump ~env IFNOT target e1
  | IFNOT, CTTMexpUnaryExpr(LogNot, e1) ->
      translate_mexpr_jump ~env IFTRUE target e1
  | iftype, CTTMexpComma(e1, e2) ->
      let _, i1 = translate_mexpr ~env ~void_ok:true e1 in
      enclose_sequence ~loc
	[enclose_parallel ~loc:(locget e1) i1; 
	 translate_mexpr_jump ~env iftype target e2]
  | iftype, CTTMexpConditional(e1, e2, e3) ->
      let s1, s2 = make_label ~env, make_label ~env in
      enclose_sequence ~loc
	[translate_mexpr_jump ~env IFNOT s1 e1;
	 translate_mexpr_jump ~env iftype target e2;
	 make_il0 ~loc (IL0stmtGoto s2);
	 make_il0 ~loc (IL0stmtLabel s1);
	 translate_mexpr_jump ~env iftype target e3;
	 make_il0 ~loc (IL0stmtLabel s2)]
  | IFTRUE, CTTMexpConstant c ->
      if is_constant_zero c then
	enclose_sequence ~loc []
      else
	make_il0 ~loc (IL0stmtGoto target)
  | IFNOT, CTTMexpConstant c ->
      if is_constant_zero c then
	make_il0 ~loc (IL0stmtGoto target)
      else
	enclose_sequence ~loc []
  | iftype,
      CTTMexpCoerce
	({ ct_ty = Tpointer { ct_ty = Tbuiltin Tchar }},
	 { locterm_v = { mexpr_t = CTTMexpConstant (CTTconstString s) }})
    ->
      if iftype = IFTRUE then
	make_il0 ~loc (IL0stmtGoto target)
      else
	enclose_sequence ~loc []
  | iftype, _ -> (* default case *)
      let v, st = translate_mexpr ~env mexpr in
      enclose_sequence ~loc
	[ enclose_parallel ~loc:(locget mexpr) st;
	  make_il0 ~loc (IL0stmtIf(iftype, v, target)) ]
     
and translate_mexpr ~env ?(void_ok=false) mexpr =
  let loc = locget mexpr in
  let t_mexpr = mexpr.locterm_v.mexpr_type in
  assert ((t_mexpr.ct_ty <> Tvoid) || void_ok);
  match mexpr.locterm_v.mexpr_t with 
  | CTTMexpWrite(lv, fields, None, e2) ->
      let lv, i1 = translate_lvalue ~env lv in
      let v2, i2 = translate_mexpr ~env e2 in
      v2, make_il0 ~loc (IL0stmtWrite(lv,fields,v2)) :: (i1 @ i2)
  | CTTMexpWrite(lv, fields, Some(binop,tcast), e2) ->
      let lv, i1 = translate_lvalue ~env lv in
      let v2, i2 = translate_mexpr ~env e2 in
      let tv_pre, i3 = gentemp_read ~loc ~env (lv,fields) t_mexpr in
      let (tv_pre_c, i4), tc = 
	match tcast with
	  None -> (tv_pre, []), t_mexpr
	| Some tc ->
	    let tv_pre_c, i4 = gentemp ~loc ~env (ILexpCoerce(tc,tv_pre)) tc
	    in (tv_pre_c, [i4]), tc
      in
      let tv, i5 = gentemp ~loc ~env (ILexpBinop(cvt_binop binop,tv_pre_c,v2)) tc in
      let tv, i6 = match tcast with
	None -> tv, []
      |	Some _ -> let tv, i6 = gentemp ~loc ~env (ILexpCoerce(t_mexpr,tv)) t_mexpr in
	tv, [i6]
      in
      let is = i5 :: i3 :: make_il0 ~loc (IL0stmtWrite(lv,fields,tv))::(i6 @ i4 @ i1 @ i2) in
      (match binop with
      | CTTbinPostPlusVV | CTTbinPostMinusVV 
      | CTTbinPostPlusPV | CTTbinPostMinusPV -> tv_pre
      |	_ -> tv), is
  | CTTMexpComma(e1,e2) ->
      let _, i1 = translate_mexpr ~env ~void_ok:true e1 in
      let v2, i2 = translate_mexpr ~env ~void_ok e2 in
      v2, [enclose_sequence ~loc
	     [enclose_parallel ~loc:(locget e1) i1;
	      enclose_parallel ~loc:(locget e2) i2]]
  | CTTMexpRead(lv, fields) -> begin
      match t_mexpr.ct_ty with
	Tarray _ ->
	  let lv, i1 = translate_lvalue ~env lv in
	  let v, i = gentemp ~loc ~env (ILexpAddress(lv,fields)) t_mexpr in
	  v, i :: i1
      | Tfunction _ ->
	  let lv, i1 = translate_lvalue ~env lv in
	  let v, i = gentemp ~loc ~env (ILexpAddress(lv,fields)) (make_c_type (Tpointer t_mexpr)) in
	  v, i :: i1
      | Tbuiltin _ | Tpointer _ | Tstruct _ ->
	  let lv, i1 = translate_lvalue ~env ~allow_rvalue:true lv in
	  let tv_pre, i3 = gentemp_read ~loc ~env (lv,fields) t_mexpr in
	  tv_pre, i3::i1
      | _ -> Format.eprintf "failed %a@." pp_c_type t_mexpr; assert false
  end
  | CTTMexpConstant(c) ->
    let v, i = gentemp ~loc ~env (ILexpConstant c) t_mexpr in
    v, [i]
  | CTTMexpBinExpr(CTTbinLogAnd, e1, e2) ->
      let vt = new_tempid ~env t_mexpr in
      let s1, s2 = make_label ~env, make_label ~env in
      vt, [enclose_sequence ~loc
	     [translate_mexpr_jump ~env IFNOT s1 e1;
	      translate_mexpr_jump ~env IFNOT s1 e2;
	      make_il0 ~loc (IL0stmtDefTemp (vt, e2.locterm_v.mexpr_type, ILexpConstant constant_one));
	      make_il0 ~loc (IL0stmtGoto(s2));
	      make_il0 ~loc (IL0stmtLabel(s1));
	      make_il0 ~loc (IL0stmtDefTemp (vt, e2.locterm_v.mexpr_type, ILexpConstant constant_zero));
	      make_il0 ~loc (IL0stmtLabel(s2))]]
  | CTTMexpBinExpr(CTTbinLogOr, e1, e2) ->
      let vt = new_tempid ~env t_mexpr in
      let s1, s2 = make_label ~env, make_label ~env in
      vt, [enclose_sequence ~loc
	     [translate_mexpr_jump ~env IFTRUE s1 e1;
	      translate_mexpr_jump ~env IFTRUE s1 e2;
	      make_il0 ~loc (IL0stmtDefTemp (vt, e2.locterm_v.mexpr_type, ILexpConstant constant_zero));
	      make_il0 ~loc (IL0stmtGoto(s2));
	      make_il0 ~loc (IL0stmtLabel(s1));
	      make_il0 ~loc (IL0stmtDefTemp (vt, e2.locterm_v.mexpr_type, ILexpConstant constant_one));
	      make_il0 ~loc (IL0stmtLabel(s2))]]
  | CTTMexpBinExpr(binop, e1, e2) ->
      let v1, i1 = translate_mexpr ~env e1 in
      let v2, i2 = translate_mexpr ~env e2 in
      let v, i = gentemp ~loc ~env (ILexpBinop (cvt_binop binop, v1, v2)) t_mexpr in
      v, (i :: i1) @ i2
  | CTTMexpCoerce (t1, e1) ->
      let v1, i1 = translate_mexpr ~env ~void_ok:(t1.ct_ty = Tvoid) e1 in
      if t1.ct_ty = Tvoid then begin
	assert void_ok;
	(-1), i1
      end else begin
	let v, i = gentemp ~loc ~env (ILexpCoerce (t1, v1)) t_mexpr in
	v, i :: i1
      end
  | CTTMexpUnaryExpr (uop, e1) ->
      let v1, i1 = translate_mexpr ~env e1 in
      let v, i = gentemp ~loc ~env (ILexpUnaryop (uop, v1)) t_mexpr in
      v, i :: i1
  | CTTMexpConditional(e1,e2,e3) ->
      let v2, i2 = translate_mexpr ~env ~void_ok:true e2 in
      let v3, i3 = translate_mexpr ~env ~void_ok:true e3 in
      let s1, s2 = make_label ~env, make_label ~env in
      if v2 = -1 then begin
	assert (v3 = -1);
	-1, [enclose_sequence ~loc
	       [translate_mexpr_jump ~env IFNOT s1 e1;
		enclose_parallel ~loc:(locget e2) i2;
		make_il0 ~loc (IL0stmtGoto s2);
		make_il0 ~loc (IL0stmtLabel s1);
		enclose_parallel ~loc:(locget e3) i3;
		make_il0 ~loc (IL0stmtLabel s2)]]
      end else begin
	let vt = new_tempid ~env t_mexpr in
	vt, [enclose_sequence ~loc
	       [translate_mexpr_jump ~env IFNOT s1 e1;
		enclose_parallel ~loc:(locget e2) i2;
		make_il0 ~loc (IL0stmtDefTemp(vt, e2.locterm_v.mexpr_type, ILexpIdent v2));
		make_il0 ~loc (IL0stmtGoto s2);
		make_il0 ~loc (IL0stmtLabel s1);
		enclose_parallel ~loc:(locget e3) i3;
		make_il0 ~loc (IL0stmtDefTemp(vt, e2.locterm_v.mexpr_type, ILexpIdent v3));
		make_il0 ~loc (IL0stmtLabel s2)]]
      end
  | CTTMexpInvoke(lhs,rhs) -> 
      let ts = List.map (translate_mexpr ~env) rhs in
      let vs, is = List.split ts in
      let is = List.flatten is in
      let lv, ilv = translate_lvalue ~env lhs in
      let is_not_returning = begin
	match lv with
	  ILlvPtr _ | ILlvTemp _ -> false
	| ILlvVar(vf,_tf) -> begin
	    let dcl = List.assoc vf env.genv.global_declarations in
	    let extlist = match dcl.gbind_storage_class with
	      Extern ext | Global ext -> ext
	    | ModuleStatic -> []
	    in
	    List.mem_assoc "noreturn" extlist
	end
      end
      in
      let addi = if is_not_returning then [ make_il0 ~loc (IL0stmtAbort ILabortNotReached) ] else [] in
      let v, i = gentemp ~loc ~env (ILexpInvoke(lv, vs)) t_mexpr in
      v, [enclose_sequence ~loc
	    ([enclose_parallel ~loc (is @ ilv);
	      i] @ addi)]
	(* Invoke needs explicit target even if result is void. *)
  | CTTMexpAddress(lv,fields) ->
      let lv, i1 = translate_lvalue ~env lv in
      let v, i = gentemp ~loc ~env (ILexpAddress(lv,fields)) t_mexpr in
      v, i :: i1

and translate_lvalue ~env ?(allow_rvalue = false) lv = 
  match lv with
    CTTMlvPtr e1 ->
      let v1, i1 = translate_mexpr ~env e1 in
      ILlvPtr(v1), i1
  | CTTMlvVar(v1,t1) ->
      ILlvVar(v1,t1), []
  | CTTMlvRvalue e1 ->
      if allow_rvalue then begin
	let v1, i1 = translate_mexpr ~env e1 in
	ILlvTemp(v1), i1
      end else
	failwith "non-lvalue not allowed"

let translate_mexpr ~env ?void_ok e = 
  let v, i = translate_mexpr ~env ?void_ok e in
  v, enclose_parallel ~loc:(locget e) i

exception Not_a_constant

type const_ptr_info = 
    Val | PtrTo of identifier | PtrToUnknown

let translate_initexp ~genv ~ptr_allowed e = 
  (* (is_pointer, ilinitexp : bool * initexp) *)
  let rec trans (ce : mexpr) = 
    let loc = locget ce in
    let make e = locput ~loc { il_cexp_type = ce.locterm_v.mexpr_type; il_cexp_t = e } in
    dprintf 9 "SSEF:trans_ie: %a" Ctt_formatter.pp_mexpr ce;
    match ce.locterm_v.mexpr_t with
    | CTTMexpComma(e1, e2) -> raise Not_a_constant
    | CTTMexpAddress(CTTMlvVar(id, ct), f) -> 
	if ptr_allowed then
	  PtrTo id, make (ILcexpAddress(ILlvVar(id, ct), f))
	else
	  raise Not_a_constant (* to prevent pointers to local variables: TODO *)
    | CTTMexpAddress(CTTMlvPtr _, []) -> assert false
    | CTTMexpAddress(CTTMlvPtr e1, ((_::_) as f)) -> 
	let p1, e1 = trans e1 in
	p1, make (ILcexpAddFieldOfs(e1, f))
    | CTTMexpAddress(CTTMlvRvalue _, _) -> failwith "taking address of rvalue (in init_exp)"
    | CTTMexpRead _ -> raise Not_a_constant
    | CTTMexpWrite _ -> raise Not_a_constant
    | CTTMexpConditional _ -> raise Not_a_constant
    | CTTMexpBinExpr(bop,e1,e2) -> begin
	let p1, e1 = trans e1 in
	let p2, e2 = trans e2 in
	let e = make (ILcexpBinop(cvt_binop bop, e1, e2)) in
	match bop, p1, p2 with
	  _, Val, Val -> Val, e
	| (CTTbinMinusVV | CTTbinMinusPP), PtrTo i1, PtrTo i2 ->
	    if i1 = i2 then Val, e else raise Not_a_constant
	| (CTTbinMinusVV | CTTbinMinusPV), p, Val
	| (CTTbinPlusPV | CTTbinPlusVV), p, Val
	| CTTbinPlusVV, Val, p ->
	    p, e
	| _ -> raise Not_a_constant
    end
    | CTTMexpCoerce({ct_ty = Tarray(et,_)} as ct, me) -> (* multi-dimensional array handling *)
	trans 
	  (locmap (fun ce -> { ce with mexpr_t = (CTTMexpCoerce({ ct with ct_ty = Tpointer(et) }, me)) })
	     ce)
    | CTTMexpCoerce(ct, me) -> begin
	let p1, e1 = trans me in
	let wide_enough =
	  match ct.ct_ty with
	    Tbuiltin(Tchar | Tschar | Tuchar 
		     | Tshort | Tushort 
		     | Tint | Tuint
		     | Tlong | Tulong
		     | Tlonglong | Tulonglong) -> begin
	      match (Ctt_abstree.size_of_type ~genv ct) with
		None -> false
	      | Some s ->
		  is_int_big_int s && int_of_big_int s >= Fsc_config.sizeof_pointer
	    end
	  | Tbuiltin(Tfloat | Tdouble | Tlongdouble) -> false
	  | Tpointer _ -> true
	  | _ -> assert false
	in
	if (p1 = Val || wide_enough) then
	  p1, make (ILcexpCoerce(ct,e1))
	else raise Not_a_constant
    end
    | CTTMexpUnaryExpr(uop,me) ->
	let p1, e1 = trans me in
	p1, make (ILcexpUnaryop(uop,e1))
    | CTTMexpInvoke _ -> raise Not_a_constant
    | CTTMexpConstant c ->
	let e = make (ILcexpConstant c) in
	match c with
	  CTTconstString _ | CTTconstTypeInfo _ ->
	    PtrToUnknown, e
	| CTTconstNull | CTTconstInteger _ | CTTconstFloat _ ->
	    Val, e
	| CTTconstAbstract _ ->
	    assert false
  in
  snd (trans e)

let rec translate_cttm_initializer ~genv ~envopt prefix_rev t init
:  ((initfield * Ctt_abstree.c_type) list * temp_id * Locterm.location * il0) list * _
    =
  let loc = locget init in
  match locval init with
    (* this function is shared for global and local initializers.
       envopt == None : computation is not allowed
       envopt == Some env : computation is allowed. genv == env.genv must be true.
     *)
    CTTMinitList l -> begin
      match t.ct_ty with
	Tarray(t', _) ->
	  let rec loop sts_rev exps_rev i = function
	      h :: t ->
		let st, exp =
		  translate_cttm_initializer 
		    ~genv ~envopt ((Iindexed i, t') :: prefix_rev) t' h
		in
		loop (List.rev_append st sts_rev) (exp :: exps_rev) (i +! unit_big_int) t
	    | [] ->
		List.rev sts_rev,
		List.rev exps_rev
	  in
	  let sts, exps = loop [] [] zero_big_int l
	  in
	  sts, locput ~loc (ILinitArray (t,exps))
      | Tstruct(id) -> begin
	  let desc = Ctt_abstree.get_struct_desc ~genv id in
	  let fields = desc.str_fields in
	  let rec loop sts_rev exps_rev fields l =
	    match fields, l with
	      [], [] ->
		List.rev sts_rev,
		List.rev exps_rev
	    | (_, NormalField f1)::f2, l1::l2 ->
		let st, exp = 
		  translate_cttm_initializer ~genv ~envopt
		    ((Inamed f1.sf_id, f1.sf_type) :: prefix_rev)
		    f1.sf_type l1
		in
		loop (List.rev_append st sts_rev) ((f1.sf_id, exp) :: exps_rev) f2 l2
	    | (_, BitField bfs)::f2, l ->
		let rec loop_bf sts_rev exps_rev bfs l = 
		  match bfs, l with
		    (None, _, _, _)::tl, init ->
		      loop_bf sts_rev exps_rev tl init
		  | (Some id, cty, _, _)::tl, l1::l2 ->
		      let st, exp = 
			translate_cttm_initializer ~genv ~envopt
			  ((Inamed id, cty) :: prefix_rev)
			  cty l1
		      in
		      loop_bf (List.rev_append st sts_rev) ((id, exp) :: exps_rev) tl l2
		  | (Some _, _, _, _)::_, [] ->
		      assert false
		  | [], l ->
		      loop sts_rev exps_rev f2 l
		in
		loop_bf sts_rev exps_rev bfs.s_bf_fields l
	    | _ -> assert false
	  in
	  let sts, exps = loop [] [] fields l in
	  sts, locput ~loc (ILinitStruct (t,exps))
      end
      | _ -> assert false
    end
  | CTTMinitExp e ->
      try
	(* if envopt <> None then raise Not_a_constant; (* TODO *)*)
	[], 
	locput ~loc
	  (ILinitConstantExp
	     (t, translate_initexp ~genv ~ptr_allowed:(envopt = None) e))
      with
	Not_a_constant ->
	  match envopt with
	    Some env ->
	      let v, st = translate_mexpr ~env e in
	      [prefix_rev, v, locget e, st], 
	      locput ~loc ILinitPostFill
	  | None ->
	      failwith "not a constant initializer"

let translate_cttm_global_initializer ~genv = 
  translate_cttm_initializer ~genv ~envopt:None []

let translate_cttm_local_initializer ~env = 
  translate_cttm_initializer ~genv:env.genv ~envopt:(Some env) []

let translate_initializer_postfill ~env id typ (prefs_rev, v, loc, st) =
  let rec iter inst_acc lv fields typ = function
      [] -> inst_acc, lv, fields
    | (Iindexed x, t') :: rest ->
	let ptrtype = Ctt_abstree.make_c_type (Tpointer t') in
	let tid = Array.map (new_tempid ~env) [| typ; ptrtype; type_int; ptrtype |] in
	iter
	  (inst_acc @ 
	   [ IL0stmtDefTemp(tid.(0), typ, ILexpAddress(lv,fields));
	     IL0stmtDefTemp(tid.(1), ptrtype, ILexpCoerce(ptrtype, tid.(0)));
	     IL0stmtDefTemp(tid.(2), type_int, ILexpConstant(CTTconstInteger x));
	     IL0stmtDefTemp(tid.(3), ptrtype, ILexpBinop(ILbinPlusPV, tid.(1), tid.(2))) ])
	  (ILlvPtr(tid.(3))) [] t' rest
    | (Inamed id, t') :: rest ->
	iter inst_acc lv (fields @ [id, t']) t' rest
  in
  let inst_acc, lv, fields = iter [] (ILlvVar(id, typ)) [] typ (List.rev prefs_rev) in
  st :: List.map (make_il0 ~loc) (inst_acc @ [IL0stmtWrite(lv, fields, v)])
	 
let replace_label_localdecl ~loc ~env (stype, typ, id, init) =
  match stype, typ.ct_ty with
    (Auto | Register), (Tbuiltin _ | Tpointer _) -> begin
      (* auto scalar variable *)
      match init with
	None -> [make_il0 ~loc (IL0stmtDeclAutoScalar(stype, typ, id, None))]
      |	Some i -> begin
	  match locval i with
	    CTTMinitExp e ->
	      let v, st = translate_mexpr ~env e in
	      let st2 = make_il0 ~loc (IL0stmtDeclAutoScalar(stype, typ, id, Some v)) in
	      [st; st2]
	  | CTTMinitList e ->
	      assert false
      end
    end
  | _ ->
      let stmt, init =
	match init with 
	  Some init -> 
	    let st, e = translate_cttm_local_initializer ~env typ init in
	    Util.map_flatten (translate_initializer_postfill ~env id typ) st, Some e
	| None -> [], None
      in
      (make_il0 ~loc (IL0stmtDeclBulk(stype, typ, id, init))) :: stmt

let dynamic_bind r v t = 
  let old_v = !r in
  r := v;
  try
    let result = t () in
    let new_v = !r in
    r := old_v;
    result, new_v
  with
    e -> r := old_v; raise e

let replace_label ~env stmt =
    let current_switch = ref [] in
    let acc = Glist.empty () in
    let emit_il0s l = 
      Glist.append acc l
    in
    let emit_il0_direct i = 
      Glist.put acc i
    in
    let emit_il0 ~loc i = emit_il0_direct (make_il0 ~loc i)
    and emit_il0_bp ~loc i = Glist.put_backpatchable acc (make_il0 ~loc i)
    in
    let rec iter ~lenv stmt = 
      let loc = locget stmt in
    match locval stmt with
      CTTMstmtNull -> ()
    | CTTMstmtExpr e ->
	let _, st = translate_mexpr ~env ~void_ok:true e in
	emit_il0_direct st
    | CTTMstmtLabeled (s, stmt) -> 
	emit_il0 ~loc (IL0stmtLabel s);
	iter ~lenv stmt
    | CTTMstmtCase_Labeled (n, stmt) ->
	let s = make_label ~env in
	current_switch := (CASE n, s)::!current_switch;
	emit_il0 ~loc (IL0stmtLabel s);
	iter ~lenv stmt
    | CTTMstmtDefault_Labeled stmt ->
	let s = make_label ~env in
	current_switch := (DEFAULT, s)::!current_switch;
	emit_il0 ~loc (IL0stmtLabel s);
	iter ~lenv stmt
    | CTTMstmtCompound (declaration_list, stmt_list) ->
	emit_il0s (Util.map_flatten (replace_label_localdecl ~loc ~env) declaration_list);
	List.iter (iter ~lenv) stmt_list
    | CTTMstmtIf (e, stmt1, None) ->
	let s = make_label ~env in
	let st = translate_mexpr_jump ~env IFNOT s e in
	emit_il0_direct st;
	iter ~lenv stmt1;
	emit_il0 ~loc (IL0stmtLabel s)
    | CTTMstmtIf (e, stmt1, Some stmt2) ->
	let s1, s2 = make_label ~env, make_label ~env in
	let st = translate_mexpr_jump ~env IFNOT s1 e in
	emit_il0_direct st;
	iter ~lenv stmt1;
	emit_il0 ~loc (IL0stmtGoto s2);
	emit_il0 ~loc (IL0stmtLabel s1);
	iter ~lenv stmt2;
	emit_il0 ~loc (IL0stmtLabel s2)
    | CTTMstmtSwitch (e, stmt) ->
	let s = make_label ~env in
	let v, st = translate_mexpr ~env e in
	emit_il0_direct st;
	let bp = emit_il0_bp ~loc (IL0stmtSwitch (v, [])) in
	
	let lenv = (BREAK, s)::lenv in

	let jump_table = 
	  let (), switch_labels = 
	    dynamic_bind current_switch []
	      (fun () -> iter ~lenv stmt)
	  in
	  if List.mem_assoc DEFAULT switch_labels then
	    switch_labels
	  else
	    (DEFAULT, s) :: switch_labels
	in
	emit_il0 ~loc (IL0stmtLabel s);
	Glist.backpatch bp (make_il0 ~loc (IL0stmtSwitch (v, List.rev jump_table)));
    | CTTMstmtWhile(e, stmt) ->
	let s1, s2 = make_label ~env, make_label ~env in
	let st = translate_mexpr_jump ~env IFNOT s2 e in
	let lenv = (BREAK, s2)::(CONTINUE, s1)::lenv in
	emit_il0 ~loc (IL0stmtLabel s1);
	emit_il0_direct st;
	iter ~lenv stmt;
	emit_il0 ~loc (IL0stmtGoto s1);
	emit_il0 ~loc (IL0stmtLabel s2)
    | CTTMstmtDoWhile( stmt, e ) ->
	let s1, s2, s3 = make_label ~env, make_label ~env, make_label ~env in
	let st = translate_mexpr_jump ~env IFTRUE s1 e in
	let lenv = (BREAK, s3)::(CONTINUE, s2)::lenv in
	emit_il0 ~loc (IL0stmtLabel s1);
	iter ~lenv stmt;
	emit_il0 ~loc (IL0stmtLabel s2);
	emit_il0_direct st;
	emit_il0 ~loc (IL0stmtLabel s3)
    | CTTMstmtFor( e1, e2, e3, stmt ) ->
	let translate_mexpr_option ~env ?void_ok = function
	    None -> -1, []
	  | Some e ->
	      let e, s = translate_mexpr ~env ?void_ok e in
	      e, [s]
	in
	let s1, s2, s3 = make_label ~env, make_label ~env, make_label ~env in
	let _, st1 = translate_mexpr_option ~env ~void_ok:true e1 in
	let _, st3 = translate_mexpr_option ~env ~void_ok:true e3 in
	let lenv = (BREAK, s3)::(CONTINUE, s2)::lenv in
	emit_il0s st1;
	emit_il0 ~loc (IL0stmtLabel s1);
	(match e2 with
	  Some e ->
	    emit_il0_direct (translate_mexpr_jump ~env IFNOT s3 e)
	| None ->
	    ());
	iter ~lenv stmt;
	emit_il0 ~loc (IL0stmtLabel s2);
	emit_il0s st3;
	emit_il0 ~loc (IL0stmtGoto s1);
	emit_il0 ~loc (IL0stmtLabel s3)
    | CTTMstmtGoto s ->
	emit_il0 ~loc (IL0stmtGoto s)
    | CTTMstmtContinue ->
	let s =
	  try
	    List.assoc CONTINUE lenv
	  with Not_found ->
	    failwith "iter statement not within a loop\n"
	in
	emit_il0 ~loc (IL0stmtGoto s)
    | CTTMstmtBreak ->
	let s =
	  try
	    List.assoc BREAK lenv
	  with Not_found ->
	    failwith "break statement not within a loop nor a switch statement\n"
	in
	emit_il0 ~loc (IL0stmtGoto s)
    | CTTMstmtReturn None -> 
	emit_il0 ~loc (IL0stmtReturn None)
    | CTTMstmtReturn (Some e) ->
	let v, st = translate_mexpr ~env e in
	emit_il0_direct st;
	emit_il0 ~loc (IL0stmtReturn (Some v))
    in
    iter [] stmt;
    enclose_sequence ~loc:(locget stmt) (Glist.to_list acc)

let translate_funcbody ~genv body = 
  let env = new_environment ~genv in
  let newbody = replace_label ~env body in
  {
   il0_var_types = env.tempid_types;
   il0_funcbody = newbody
 }

let translate_program_to_il0 ~genv p = 
  locmap_list
    (function
	CTTMdeclFunction (gs,ct,id,args,body) ->
	  IL0declFunction(gs,ct,id,args,translate_funcbody ~genv body)
      | CTTMdeclVariable (gs,ct,id,init) ->
	  let init =
	    match init with
	      Some init ->
		let st, init = translate_cttm_global_initializer ~genv ct init in
		assert (st = []);
		Some init
	    | None ->
		None
	  in
	  IL0declVariable(gs,ct,id,init))
    p
