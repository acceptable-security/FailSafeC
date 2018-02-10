(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

open Locterm
open Util
open Big_int
open Ctt_abstree
open C_typing

open Ctt_visitor

let dprintf l f = Debug.dprintf ~category:22 l f
let get_debug_flags () = Debug.get_debug_flags ~category:22
open Ctt_formatter

let fold_constants e = 
  try
    Reduce.reduce_expression e
  with
    Reduce.NotConstant -> e

let rec convert_type t =
  let new_ty = 
    match t.ct_ty with
      Tarray(t1,sz1) -> begin
	match convert_type t1 with
	  { ct_ty = Tarray(t2,sz2) } ->
	    let new_size = 
	      match sz1, sz2 with
		Some x, Some y -> Some (mult_big_int x y)
	      | _ -> None
	    in
	    Tarray(t2,new_size)
	| t2 ->
	    Tarray(t2,sz1)
      end
    | Tbuiltin _ | Tvoid | Tstruct _ -> t.ct_ty
    | Tfunction(at,f,rt) ->
	Tfunction(list_map convert_type at, f, convert_type rt)
    | Tpointer(t) ->
	let t = convert_type t in
	Tpointer(
	match t.ct_ty with
	  Tarray(t1,sz1) -> t1
	| _ -> t)
    | Tabstract(_) -> assert false
  in
  { t with ct_ty = new_ty }

let calculate_magnitude t ~loc = 
  match t.ct_ty with
    Tpointer (et) -> begin
      match convert_type et with
	{ct_ty = Tarray(_,Some sz)} ->
	  Some (make_expr (CTTexpConstant (CTTconstInteger sz)) type_int ~loc)
      |	{ct_ty = Tarray(_,None)} ->
	  failwith "unknown size: why?"
      |	_ -> None
    end
  | _ -> None

let rec reduce_expr exp = 
  let v = visit_expr ~v_type:convert_type ~self:reduce_expr in
  let loc = locget exp in
  let make_expr = make_expr ~loc in
  let body = 
    match (locval exp).expr_t with
      CTTexpBinAssign(
      (CTTbinPlusPV | CTTbinMinusPV | 
       CTTbinPostPlusPV | CTTbinPostMinusPV) as binop,
      e_ptr,None,
      e_off) -> begin
	match calculate_magnitude exp.locterm_v.expr_type ~loc with
	  Some multiply ->
	    let e_ptr = reduce_expr e_ptr in
	    let e_off = reduce_expr e_off in
	    let e2 =
	      make_expr
		(CTTexpBinExpr (CTTbinTimes, e_off, multiply))
		(type_of e_off)
	    in
	    let e2 = fold_constants e2 in
	    CTTexpBinAssign(binop,e_ptr,None,e2)
	| None -> (v exp).locterm_v.expr_t
      end
    | CTTexpBinExpr(
      (CTTbinPlusPV | CTTbinMinusPV) as binop,
	e_ptr, e_off) -> begin
	  match calculate_magnitude exp.locterm_v.expr_type ~loc with
	    Some multiply ->
	      let e_ptr = reduce_expr e_ptr in
	      let e_off = reduce_expr e_off in
	      let e2 =
		make_expr
		  (CTTexpBinExpr (CTTbinTimes, e_off, multiply))
		  (type_of e_off)
	      in
	      let e2 = fold_constants e2 in
	      CTTexpBinExpr(binop,e_ptr,e2)
	  | None ->
	      (v exp).locterm_v.expr_t
	end
    | CTTexpCoerce(t,e1) ->
	let e1 = reduce_expr e1 in
	let new_t = convert_type t in
	if equal_type ~check_qual:qual_eq ~check_iqual:qual_eq new_t e1.locterm_v.expr_type then
	  e1.locterm_v.expr_t 
	else
	  CTTexpCoerce(convert_type t, e1)
    | _ -> (v exp).locterm_v.expr_t
  in
  let t = convert_type exp.locterm_v.expr_type in
  make_expr body t

let list_map_f f l = 
  let g = Glist.empty () in
  let rec iter = function
      [] -> ()
    | [e] -> Glist.put g (f ~final:true e)
    | h::t -> Glist.put g (f ~final:false h); iter t
  in
  iter l;
  Glist.to_list g

let rec dummy_initializer ~genv ~loc num t = 
  let rec dummy_initializer_elem t = 
    match t.ct_ty with
      Tpointer _ -> 
	[ locput ~loc
	    (CTTinitExp(make_expr ~loc (CTTexpConstant CTTconstNull) t)) ]
    | Tbuiltin (Tfloat | Tdouble | Tlonglong) ->
	[ locput ~loc
	    (CTTinitExp(make_expr ~loc (CTTexpConstant (CTTconstFloat 0.0)) t)) ]
    | Tbuiltin _ ->
	[ locput ~loc
	    (CTTinitExp(make_expr ~loc (CTTexpConstant (CTTconstInteger zero_big_int)) t)) ]
    | Tarray(et,None) -> assert false
(*    | Tarray({ ct_ty = Tarray _ } as et, Some sz) -> begin
	(* emit expanded pattern *)
	dprintf 6 "  [[array-array]]";
	match dummy_initializer_elem et with
	  l ->
	    List.flatten (Util.list_repeat l (int_of_big_int sz))
    end *)
    | Tarray(et,Some sz) ->
	dummy_initializer ~genv ~loc (int_of_big_int sz) et
    | Tstruct id -> begin
	let desc = get_struct_desc ~genv id in
	let field_dummy = function
	    NormalField f -> begin
	      match f.sf_type.ct_ty with
		Tarray(et,_) -> [ locput ~loc (CTTinitList (dummy_initializer_elem et)) ]
	      | _ -> dummy_initializer_elem f.sf_type
	    end
	  | BitField { s_bf_fields = bf } ->
	      Util.map_flatten
		(function 
		    (None, _, _, _) -> []
		  | (Some _, t, _, _) -> dummy_initializer_elem t) bf
	in
	match desc.str_union_p with
	  Struct -> [ locput ~loc (CTTinitList (map_flatten (fun (_,f) -> field_dummy f) desc.str_fields)) ]
	| Union -> begin
	    match desc.str_fields with
	      [] -> []
	    | (_,f)::_ -> [ locput ~loc (CTTinitList (field_dummy f))]
	end
    end
    | Tabstract _|Tfunction (_, _, _)|Tvoid -> assert false
  in
  dprintf 6 "generating %d dummies for %a" num pp_c_type t;
  if num = 0 then [] else 
  let r = List.flatten (Util.list_repeat (dummy_initializer_elem t) num) in
  dprintf 7 "result of %d dummies for %a is [%a]" num pp_c_type t 
    (pp_list ~elem_pp:pp_ctt_initalizer ~sep_pp:C_pp.pp_print_sep_comma) r;
  r

let rec reduce_initializer ~final ~genv typ init =
  (* put dummy initializer appropriately *)
  dprintf 6 "reducing %a (type %a, final=%b)" pp_ctt_initalizer init pp_c_type typ final;
  let loc = locget init in
  let flatten_initializers = 
    Util.map_flatten
      (function
	  { locterm_v = CTTinitList l } -> l
	| _ -> assert false)
  in
  let r = match typ.ct_ty, locval init with
  | (Tpointer _ | Tbuiltin _ | Tstruct _), CTTinitExp(i) -> 
      locput ~loc (CTTinitExp (reduce_expr i))
  | (Tstruct id, CTTinitList(l)) ->
      locput ~loc
	(CTTinitList
	   (visit_struct_initializer ~loc ~v_init:(reduce_initializer ~final:true ~genv) ~genv id l))
  | (Tarray ({ ct_ty = Tarray(_) } as et, sz), CTTinitList(l)) ->
      if final then
	let nl = list_map_f (reduce_initializer ~genv et) l in
	let nl = flatten_initializers nl in
	locput ~loc (CTTinitList nl)
      else begin
	assert (sz <> None);
	let nl = Util.list_map (reduce_initializer ~final:false ~genv et) l in
	let num_dummy = int_of_big_int (Option.get sz) - List.length nl in
	if num_dummy = 0 then 
	  let nl = flatten_initializers nl in
	  locput ~loc (CTTinitList nl)
	else
	  let l = dummy_initializer ~genv ~loc num_dummy et in
	  let nl = flatten_initializers nl in
	  locput ~loc (CTTinitList (nl @ l))
      end
  | (Tarray (t,sz), CTTinitList(l)) -> 
      if final then
	locput ~loc (CTTinitList (list_map (reduce_initializer ~genv ~final:true t) l))
      else
	let nl = list_map (reduce_initializer ~genv t ~final:false) l in
	let num_dummy = int_of_big_int (Option.get sz) - List.length nl in
	if num_dummy = 0 then 
	  locput ~loc (CTTinitList nl)
	else
	  let l = dummy_initializer ~genv ~loc num_dummy t in
	  locput ~loc (CTTinitList (nl @ l))
  | (Tfunction _ | Tvoid), _ -> assert false
  | _ -> assert false
  in
  dprintf 6 "  result is %a" pp_ctt_initalizer r;
  r

let rec reduce_ldecl ~genv (lsclass, typ, id, initopt) =
  let initopt = Option.map (reduce_initializer ~genv ~final:true typ) initopt in
  let typ = convert_type typ in
  (lsclass, typ, id, initopt)
    
let rec reduce_statement ~genv statement =
  visit_statement ~v_type:convert_type ~v_expr:reduce_expr ~v_ldecl:(reduce_ldecl ~genv) ~self:(reduce_statement ~genv) statement

let rec reduce_gdecl ~genv decl =
  locmap 
    (function
	CTTdeclFunction(gsclass, typ, ident, argnames, body) ->
	  let new_type = convert_type typ in
	  let new_body = reduce_statement ~genv body in
	  CTTdeclFunction(gsclass, new_type, ident, argnames, new_body)
      | CTTdeclVariable(gsclass, typ, ident, initopt) ->
	  CTTdeclVariable(gsclass, convert_type typ, ident, Option.map (reduce_initializer ~genv ~final:true typ) initopt))
    decl

let reduce_struct_field = function
    NormalField n ->
      NormalField { n with sf_type = convert_type n.sf_type }
  | BitField b ->
      BitField { b with 
		 s_bf_fields =
		 list_map
		   (fun (i,ty,s,p) -> (i, convert_type ty, s, p))
		   b.s_bf_fields }

let reduce_struct_desc s = 
  let fields = 
    list_map (fun (i,f) -> i, reduce_struct_field f)
      s.str_fields 
  in
  let fields_byname = 
    list_map (fun (i, (t, o)) -> i, (convert_type t, o))
      s.str_fields_byname
  in
  { s with str_fields = fields; str_fields_byname = fields_byname }

let reduce_global_declarations ~genv gdecls = 
  list_map
    (fun (id, gs) -> id, { gs with gbind_type = convert_type gs.gbind_type }) gdecls

let reduce_environment genv = 
  let str_table = Earray.map reduce_struct_desc genv.struct_table in
  let gdecls = reduce_global_declarations ~genv genv.global_declarations in
  { genv with struct_table = str_table; global_declarations = gdecls }

let rec reduce_program ~genv prog = 
  let prog = list_map (reduce_gdecl ~genv) prog
  in
  let genv = reduce_environment genv in
  genv, prog
