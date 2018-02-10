(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2006. *)

(* reverse convert from ctt_tree to parse_tree *)

open C_abstree
open Locterm
open Ctt_abstree
open Big_int_infix
open Util

let encode_c_string = Locterm.encode_c_string

let get_type_qualifiers ty = 
  (if ty.ct_volatile_p then [Volatile] else [])
    @ (if ty.ct_const_p then [Const] else [])

let get_type_qualifiers_speclist ty = 
  (if ty.ct_volatile_p then [TypeQualifier Volatile] else [])
    @ (if ty.ct_const_p then [TypeQualifier Const] else [])

let dummy_loc = Locterm.dummy_location
let make_expr x ~orig = loccopy ~orig x
let make_expr_l x ~loc = locput ~loc x

let constant_exp_of_big_int ~loc i = 
  make_expr_l (PexpConstant(PconstInteger (string_of_big_int i))) ~loc

let constant_exp_of_int ~loc i = 
  make_expr_l (PexpConstant(PconstInteger (string_of_int i))) ~loc

let builtin_to_typespec t =
  List.map (fun x -> TypeSpec (PtypespecBuiltin x))
  (match t with
  | Tlongdouble -> [Long; Double]
  | Tdouble -> [Double]
  | Tfloat -> [Float]
  | Tlonglong -> [Long; Long]
  | Tulonglong -> [Unsigned; Long; Long]
  | Tlong -> [Long]
  | Tulong -> [Unsigned; Long]
  | Tint -> [Int]
  | Tuint -> [Unsigned; Int]
  | Tshort -> [Short]
  | Tushort -> [Unsigned; Short]
  | Tchar -> [Char]
  | Tuchar -> [Unsigned; Char]
  | Tschar -> [Signed; Char])
    
let void_spectreelist = [TypeSpec (PtypespecBuiltin Void)]

let find_struct_name ~genv id = 
  let rec iter e = 
    match e with
      [] -> Printf.sprintf "_anonstruct_%d" id
    | (name,id')::_ when id = id' -> name
    | _::tl -> iter tl
  in
  iter genv.struct_name_table

let is_union ~genv id = 
  (Earray.get genv.struct_table id).str_union_p

let rec convert_declarator_iter ~genv ty ?argnames declbody
    : declaration_specifier list * declarator = 
  match ty.ct_ty with
    Tpointer(t1) ->
      convert_declarator_iter ~genv t1 (PdeclPointer(get_type_qualifiers ty,declbody))
  | Tarray(t1,sz_opt) ->
      convert_declarator_iter ~genv t1 (PdeclArray(declbody, Option.map (constant_exp_of_big_int ~loc:dummy_loc) sz_opt))
  | Tstruct(id) ->
      let quals = get_type_qualifiers_speclist ty in
      let name = find_struct_name ~genv id in
      let is_union = is_union ~genv id in
      let tspec = [TypeSpec (PtypespecStruct(is_union,Some name,None,[],dummy_loc))] in
      (quals @ tspec), declbody
  | Tfunction(args,varargs,rettype) ->
      let argtree = 
	match argnames with
	  None ->
	    List.map
	      (fun t -> 
		let ds, dc = convert_declarator_anonymous ~genv t in
		PpdeclAbstract(ds,dc))
	      args
	| Some argnames ->
	    List.map2
	      (fun t n -> 
		let ds, dc = convert_declarator ~genv t n in
		PpdeclAbstract(ds,dc))
	      args argnames
      in
      let argtree = 
	if varargs then
	  if argtree = [] then
	    [] (* K&R style empty decl *)
	  else 
	    argtree @ [PpdeclVariant]
	else
	  if argtree = [] then
	    [PpdeclAbstract(void_spectreelist, PdeclAnonymous)]
	  else
	    argtree
      in
      convert_declarator_iter ~genv rettype (PdeclFuncType(declbody, argtree))
  | Tbuiltin t ->
      let quals = get_type_qualifiers_speclist ty in
      let tspec = builtin_to_typespec t in
      (quals @ tspec), declbody
  | Tvoid ->
      let quals = get_type_qualifiers_speclist ty in
      quals @ void_spectreelist, declbody
  | Tabstract(s) ->
      let quals = get_type_qualifiers_speclist ty in
      let tspec = [TypeSpec (PtypespecAlias s)] in
      (quals @ tspec), declbody

and convert_declarator ~genv ty ?argnames ident = 
  convert_declarator_iter ~genv ty ?argnames (PdeclIdent ident)

and convert_declarator_anonymous ~genv ty = 
  convert_declarator_iter ~genv ty (PdeclAnonymous)

let typeinfo_converter_hook =
  ref (fun ~genv ~(make : C_abstree.expr_desc -> C_abstree.expr) _ -> 
    failwith "Internal error: __typeinfo appeared without proper configuration")

let use_gnu_hex_float_constant = ref true

(* workaround for ARM OCaml bugs (bug #lepidum-85). *)
       (** workaround for OCaml bug PR#4358, fixed in OCaml 3.10.1. *)
       let is_bits_of_float_broken = 
         Int64.logxor (Int64.bits_of_float 1.0) (Int64.bits_of_float (-1.0)) = 2147483648L

       (** workaround for OCaml bug PR#4503, fixed in OCaml 3.11.0. *)
	let is_classify_float_broken =
	  classify_float (1.0 /. 0.0) <> FP_infinite

        let classify_float = 
	  if is_classify_float_broken then
	    (fun x ->
	      match x >= 0.0, x <= 0.0 with
		true, false | false, true ->
		  if (x /. (1.0 /. 0.0)) = 0.0 then 
		    FP_normal (* may be FP_subnormal; not checked here *)
		  else FP_infinite
	      | true, true -> FP_zero
	      | false, false -> FP_nan)
	  else
	    classify_float
         
let significant_bits_of_float = 
  if is_bits_of_float_broken then 53 else (* epsilon_float is also broken #PR4358 *)
  let r2d v = Int64.float_of_bits (Int64.bits_of_float v) in
  let s_e, e_e = frexp epsilon_float in
  let s_e, e_e = r2d (s_e *. 2.0), e_e - 1 in
  assert (s_e = 1.0);
  let e = -e_e + 1 in
  let two_e = r2d (ldexp 1. e) in
  assert (r2d (two_e +. 1.0) = two_e || r2d(two_e +. 1.0) = r2d(two_e +. 2.0));
  assert (r2d (two_e +. 2.0) <> two_e);
  e

let fsc_string_of_float ee ~postfix = 
    match classify_float ee with
      FP_nan -> "(0.0" ^ postfix ^ " / 0.0" ^ postfix ^ ") /* nan */"
    | FP_infinite ->
	if ee > 0.0 
	then "(1.0" ^ postfix ^ " / 0.0" ^ postfix ^ ") /* +inf */" 
	else "(-1.0" ^ postfix ^ " / 0.0" ^ postfix ^ ") /* -inf */"
    | FP_zero -> if 1.0 /. ee > 0.0 then "+0.0" ^ postfix else "-0.0" ^ postfix
    | FP_normal | FP_subnormal ->
	if !use_gnu_hex_float_constant then begin
	  if (float_of_string (string_of_float ee)) = ee then string_of_float ee ^ postfix else
	  let sign, e = if ee < 0.0 then "-", -.ee else "+", ee in
	  let f, i = modf e in
	  if (f = 0.0) then begin
	    (* integer *)
	    let s, exp = frexp e in (* [0.5, 1.0) *)
	    let s, exp = s *. 2.0, exp - 1 in (* [1, 2) *)
	    if exp > significant_bits_of_float then begin
	      (*
		if integer bigger than 2^53: use hexadecimal with exponent
	       *)
	      let s, exp = ldexp s significant_bits_of_float, exp - significant_bits_of_float in
	      assert (fst (modf s) = 0.0);
	      let s_bits = Int64.of_float s in
	      assert (Int64.to_float s_bits = s);
	      Printf.sprintf "%s0x%Lx.p%d" sign s_bits exp ^ postfix ^
	      Printf.sprintf " /* %g */" ee
	    end else begin
	      (* if integer smaller than 2^53: use decimal. *)
	      Printf.sprintf "%s%F" sign i ^ postfix
	    end
	  end else begin
	    (* mixed or fraction only. *)
	    (*let rec iter s oexp =
	      if (fst (modf s) = 0.0) then s, oexp
	      else
	      iter (s *. 2.0) (oexp - 1)
	      in
	      let s, oexp = iter e 0 in
	      let s_bits = Int64.of_float s in
	      assert (Int64.to_float s_bits = s);*)
	    let s_bits, oexp = Util.frexp_int e in
	    Printf.sprintf "%s0x%Lx.p%d" sign s_bits oexp ^ postfix ^
	    Printf.sprintf " /* %g */" ee
	  end
      end
	else
	  string_of_float ee ^ postfix

let convert_constant ~genv ~loc exp typ =
  match exp with
    CTTconstNull -> PexpConstant(PconstInteger "0")
  | CTTconstInteger e -> begin
      let postfix, intmask_size = 
	match typ.ct_ty with
	  Tbuiltin Tchar when not Fsc_config.char_is_signed -> "U", None
	| Tbuiltin ((Tchar | Tschar | Tshort | Tint) as t) -> "", Some (size_of_builtin_type t)
	| Tbuiltin (Tuchar | Tushort | Tuint) -> "U", None
	| Tbuiltin Tlong -> "L", Some Fsc_config.sizeof_long
	| Tbuiltin Tulong -> "UL", None
	| Tbuiltin Tlonglong -> "LL", Some Fsc_config.sizeof_longlong
	| Tbuiltin Tulonglong -> "ULL", None
	| _ -> "", None
      in
      let d = PexpConstant(PconstInteger (string_of_big_int e ^ postfix)) in
      match intmask_size with
	None -> d
      | Some s ->
	  let min_signed_int = minus_big_int (power2_big_int (s * Fsc_config.bits_of_byte - 1)) in
	  Debug.dprintf ~category:120 6 "const_int->Ptree: %d %s %s" s (string_of_big_int min_signed_int) (string_of_big_int e);
	  if min_signed_int ==! e then
	    let e' = e +! unit_big_int in
	    PexpConstant(PconstInteger ("(" ^ string_of_big_int e' ^ postfix ^ "-1)"))
	  else d
  end
  | CTTconstFloat e -> 
      let postfix = match typ.ct_ty with
      | Tbuiltin Tfloat -> "F"
      | Tbuiltin Tdouble -> ""
      | Tbuiltin Tlongdouble -> "L"
      | _ -> assert false
      in
      PexpConstant(PconstFloat (fsc_string_of_float e ~postfix))
  | CTTconstString s -> PexpConstant(PconstString ([encode_c_string s]))
  | CTTconstTypeInfo t -> (!typeinfo_converter_hook) ~genv ~make:(make_expr_l ~loc) t
  | CTTconstAbstract s -> PexpVar s

let convert_binop = function
    CTTbinTimes        -> PbinTimes       
  | CTTbinDiv          -> PbinDiv         
  | CTTbinPlusVV       -> PbinPlus
  | CTTbinMinusVV      -> PbinMinus
  | CTTbinPostPlusVV   -> assert false
  | CTTbinPostMinusVV  -> assert false
  | CTTbinPlusPV       -> PbinPlus 
  | CTTbinMinusPP      -> PbinMinus
  | CTTbinMinusPV      -> PbinMinus
  | CTTbinPostPlusPV   -> assert false
  | CTTbinPostMinusPV  -> assert false
  | CTTbinModulo       -> PbinModulo      
  | CTTbinLshift       -> PbinLshift      
  | CTTbinRshift       -> PbinRshift      
  | CTTbinLogAnd       -> PbinLogAnd
  | CTTbinLogOr        -> PbinLogOr
  | CTTbinIntAnd       -> PbinIntAnd      
  | CTTbinIntOr        -> PbinIntOr       
  | CTTbinIntXor       -> PbinIntXor      
  | CTTbinLessThan     -> PbinLessThan    
  | CTTbinLessEqual    -> PbinLessEqual   
  | CTTbinGtrThan      -> PbinGtrThan     
  | CTTbinGtrEqual     -> PbinGtrEqual    
  | CTTbinEqual        -> PbinEqual       
  | CTTbinNotEqual     -> PbinNotEqual    

let rec convert_expr ~genv e = 
  locmap_l (fun ~loc exp ->
    match exp.expr_t with
    | CTTexpComma(e1,e2) -> 
	PexpComma(convert_expr ~genv e1, convert_expr ~genv e2)
    | CTTexpAssign (e1,e2) ->
	PexpAssign(convert_expr ~genv e1, convert_expr ~genv e2)
    | CTTexpBinAssign((CTTbinPostPlusVV | CTTbinPostPlusPV), e1, _, e2) -> begin
	match e2.locterm_v.expr_t with
	  CTTexpConstant(CTTconstInteger p) when eq_big_int p unit_big_int ->
	    PexpPostInc (convert_expr ~genv e1)
	| _ -> failwith "argument of post-plus must be 1"
    end
    | CTTexpBinAssign((CTTbinPostMinusVV | CTTbinPostMinusPV), e1, _, e2) -> begin
	match e2.locterm_v.expr_t with
	  CTTexpConstant(CTTconstInteger p) when eq_big_int p unit_big_int ->
	    PexpPostDec (convert_expr ~genv e1)
	| _ -> failwith "argument of post-minus must be 1"
    end
    | CTTexpBinAssign (bop, e1, _, e2) ->
	PexpBinAssign (convert_binop bop, convert_expr ~genv e1, convert_expr ~genv e2)
    | CTTexpConditional(e1, e2, e3) ->
	PexpConditional (convert_expr ~genv e1, convert_expr ~genv e2, convert_expr ~genv e3)
    | CTTexpBinExpr((CTTbinPostPlusVV | CTTbinPostPlusPV | CTTbinPostMinusVV | CTTbinPostMinusPV),_,_)
	-> failwith "post+/- cannot appear in usual binary operation expression"
    | CTTexpBinExpr(binop, e1, e2) ->
	PexpBinExpr(convert_binop binop, convert_expr ~genv e1, convert_expr ~genv e2)
    | CTTexpCoerce(ty, e1) ->
	let sq, dc = convert_declarator_anonymous ~genv ty in
	PexpCast(Ptypename(sq,dc), convert_expr ~genv e1)
    | CTTexpUnaryExpr(uop, e1) ->
	PexpUnaryExpr(uop, convert_expr ~genv e1)
    | CTTexpAddress(e1) ->
	PexpAddress(convert_expr ~genv e1)
    | CTTexpPtrDeref(e1) ->
	PexpPtrDeref(convert_expr ~genv e1)
    | CTTexpInvoke(e1,eargs) ->
	PexpInvoke(convert_expr ~genv e1, List.map (convert_expr ~genv) eargs)
    | CTTexpField (e1,id) ->
	PexpField(convert_expr ~genv e1, id)
    | CTTexpConstant(const) ->
	convert_constant ~genv ~loc const (exp.expr_type)
    | CTTexpVar(id, typ) ->
	PexpVar(id)) e

let rec convert_initializer ~genv i =
  locmap
    (function
	CTTinitExp e -> (PinitExp(convert_expr ~genv e))
      | CTTinitList l -> (PinitList(Util.list_map (convert_initializer ~genv) l)))
    i

let make_decl = locput

let make_stmt s ~orig = 
  loccopy ~orig s

let convert_local_storage_class = function
    LocalStatic -> [StorageClass C_abstree.Static]
  | Auto -> [StorageClass C_abstree.Auto]
  | Register -> [StorageClass C_abstree.Register]
  | FuncArgs -> []

let rec convert_statement ~genv stmt = 
  locmap_l (fun ~loc -> function
      CTTstmtNull -> PstmtExpr None
    | CTTstmtExpr(e) -> PstmtExpr (Some (convert_expr ~genv e))
    | CTTstmtLabeled(l,s) -> PstmtLabeled(l, convert_statement ~genv s)
    | CTTstmtCase_Labeled(l,s) -> 
	PstmtCase_Labeled(constant_exp_of_big_int l ~loc, convert_statement ~genv s)
    | CTTstmtDefault_Labeled(s) ->
	PstmtDefault_Labeled(convert_statement ~genv s)
    | CTTstmtCompound(d,s) ->
      let ld = 
	Util.list_map
	  (fun (lsclass, ty, id, init) ->
	    let sq, dc = convert_declarator ~genv ty id in
	    let lsclasses = convert_local_storage_class lsclass in
	    make_decl ~loc
	      (PdeclVariable
		 (lsclasses @ sq,
		  [PinitDecl(dc, Option.map (convert_initializer ~genv) init)])))
	  d
      in
      let ls = Util.list_map (convert_statement ~genv) s in
      PstmtCompound(ld, ls)
    | CTTstmtIf(e1,s2,s3) ->
      PstmtIf(convert_expr ~genv e1,
	      convert_statement ~genv s2,
	      Option.map (convert_statement ~genv) s3)
    | CTTstmtSwitch(e1,s) ->
	PstmtSwitch(convert_expr ~genv e1, convert_statement ~genv s)
    | CTTstmtWhile(e1,s) ->
	PstmtWhile(convert_expr ~genv e1, convert_statement ~genv s)
    | CTTstmtDoWhile(s,e1) ->
	PstmtDoWhile(convert_statement ~genv s, convert_expr ~genv e1)
    | CTTstmtFor(e1,e2,e3,s) ->
	PstmtFor(Option.map (convert_expr ~genv) e1,
		 Option.map (convert_expr ~genv) e2,
		 Option.map (convert_expr ~genv) e3,
		 convert_statement ~genv s)
    | CTTstmtGoto(id) ->
	PstmtGoto(id)
    | CTTstmtContinue -> PstmtContinue
    | CTTstmtBreak -> PstmtBreak
    | CTTstmtReturn(e1) ->
	PstmtReturn(Option.map (convert_expr ~genv) e1)) stmt

let convert_global_storage_class = function
    Extern ext | Global ((_::_) as ext) -> 
      StorageClass C_abstree.Extern ::
      List.map (fun (id, elist) -> ExtendedDeclSpec(id, elist)) ext
  | ModuleStatic -> [StorageClass C_abstree.Static]
  (*| Inline -> [StorageClass C_abstree.Inline]*)
  | Global [] -> []

let convert_global_declaration ~genv (gdecl : Ctt_abstree.global_declaration) = 
  let dbody = match locval gdecl with
    CTTdeclFunction(gsc, ty, id, argnames, stmt) ->
      let gsc = convert_global_storage_class gsc in
      let sq, dc = 
	convert_declarator ~genv ty ~argnames id in
      let s = convert_statement ~genv stmt in
      PdeclFunction(gsc @ sq, dc, [], s)
  | CTTdeclVariable(gsc, ty, id, init) ->
      let gsc = convert_global_storage_class gsc in
      let sq, dc = 
	convert_declarator ~genv ty id in
      let init = Option.map (convert_initializer ~genv) init in
      PdeclVariable(gsc @ sq, [PinitDecl(dc, init)])
  in
  make_decl ~loc:(locget gdecl) dbody

let convert_struct_field ~genv (size, field) =
  match field with
    NormalField { sf_id = id; sf_type = ty } ->
      let sq, dc = convert_declarator ~genv ty id in
      [ PstructDecl (sq, [PstructDeclNormal dc]) ]
  | BitField { s_bf_fields = bfs } ->
      Util.list_map
	(fun (id_opt, ty, width, _) ->
	  match id_opt with
	    None ->
	      let sq, dc = convert_declarator_anonymous ~genv ty in
	      PstructDecl
		(sq, [PstructDeclBitfield (None, constant_exp_of_int ~loc:dummy_loc width)])
	  | Some n ->
	      let sq, dc = convert_declarator ~genv ty n in
	      PstructDecl
		(sq, [PstructDeclBitfield (Some dc, constant_exp_of_int ~loc:dummy_loc width)])
	)
	bfs
		
let make_struct_declarations ~genv fields =
  Util.map_flatten (convert_struct_field ~genv) fields

let convert_struct_declaration ~genv id s =
  make_decl ~loc:s.str_loc
    (PdeclVariable
       ([TypeSpec 
	   (PtypespecStruct
	      (s.str_union_p,
	       Some (find_struct_name ~genv id),
	       Some (make_struct_declarations ~genv s.str_fields),
	       [],
	       s.str_loc
	      ))], []))

let convert_program ~genv ?(emit_structs=true) (p : program) : C_abstree.program = 
  let struct_defs =
    if emit_structs then begin
      let r = Glist.empty () in
      Earray.iteri (fun id decl ->
	Glist.put r (convert_struct_declaration ~genv id decl))
	genv.struct_table;
      Glist.to_list r
    end else [] in
  struct_defs @ Util.list_map (convert_global_declaration ~genv) p
