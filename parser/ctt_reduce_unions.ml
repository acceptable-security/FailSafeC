(* Part of Fail-Safe C Compiler. (c) 2005 Yutaka Oiwa. *)

open Ctt_abstree
open Locterm
open C_typing
open Ctt_visitor
open Util
open Big_int_infix

let dprintf l v = Debug.dprintf ~category:23 l v

let pad_field_name = "__pad_union"

let t_is_union ~genv t = 
  match t.ct_ty with
    Tstruct sid ->
      (get_struct_desc ~genv sid).str_union_p = Union
  | _ -> false

let translate_type t = t

let rec translate_expr ~genv exp = 
  let v = visit_expr ~v_type:translate_type ~self:(translate_expr ~genv) in
  let t = exp.locterm_v.expr_type in
  let make_expr = make_expr ~loc:(locget exp) in
  dprintf 5 "visit: %a" Ctt_formatter.pp_expr exp;
  match exp.locterm_v.expr_t with
    CTTexpField(e1,id) ->
      let msg = sfprintf "e1=[%a](%a) fld=%s(%a)" 
	  Ctt_formatter.pp_expr e1 Ctt_formatter.pp_c_type e1.locterm_v.expr_type
	  id Ctt_formatter.pp_c_type t in
      dprintf 5 "hit:  %s" msg;
      let body = 
	let e1 = translate_expr ~genv e1 in
	let t1 = e1.locterm_v.expr_type in
	if t_is_union ~genv t1 then begin
	  let addr = make_expr (CTTexpAddress e1) (make_c_type (Tpointer t1)) in
	  match t.ct_ty with
	    Tarray(ct,_) ->
	      dprintf 5 "trans:1  %s" msg;
	      let new_ptr_t = make_c_type (Tpointer ct) in
	      CTTexpCoerce(new_ptr_t,addr)
	  | _ ->
	      dprintf 5 "trans:2  %s" msg;
	      let new_ptr_t = make_c_type (Tpointer t) in
	      let newaddr = make_expr (CTTexpCoerce(new_ptr_t,addr)) new_ptr_t in
	      CTTexpPtrDeref(newaddr)
	end
	else
	  CTTexpField(e1,id)
      in
      dprintf 5  "done: %s" msg;
      if exp.locterm_v.expr_t == body then exp else
      make_expr body t
  | e ->
      v exp

let rec translate_initializer ~genv typ init = 
  dprintf 5 "cru-ti: %a %a" Ctt_formatter.pp_c_type typ Ctt_formatter.pp_ctt_initalizer init;
  visit_initializer 
    ~v_expr:(translate_expr ~genv)
    ~v_strinit:(translate_struct_initializer ~genv)
    ~self:(translate_initializer ~genv)
    typ init

and translate_struct_initializer ~genv ~loc id inits = 
  visit_struct_initializer ~genv ~loc ~v_init:(translate_initializer ~genv) id inits

let translate_ldecl ~genv = function
    ls, ct, id, None as ld -> ld
  | ls, ct, id, Some init ->
      ls, ct, id, Some (translate_initializer ~genv ct init)

let rec translate_statement ~genv st = 
  visit_statement
    ~v_type:translate_type
    ~v_expr:(translate_expr ~genv)
    ~v_ldecl:(translate_ldecl ~genv)
    ~self:(translate_statement ~genv)
    st

let translate_struct_desc s = 
  if s.str_union_p <> Union then zero_big_int, s else
  if s.str_size = None then zero_big_int, { s with str_union_p = Struct } else
  match s.str_fields with
    [] -> zero_big_int, { s with str_union_p = Struct }
  | ((_ofs,NormalField fld)::_tl) -> begin
      let pad_size, new_fields = 
	let union_size = Option.get s.str_size in
	let elem_size = fld.sf_size in
	let diff = union_size -! elem_size in
	if diff ==! zero_big_int then
	  zero_big_int, [ zero_big_int, NormalField fld ]
	else
	  diff,
	  [ zero_big_int, NormalField fld;
	    elem_size, 
	    NormalField
	      { sf_id = pad_field_name;
		sf_type = make_c_type (Tarray(type_unsigned_char, Some diff));
		sf_size = diff } ]
      in
      pad_size,
      update_fields_cache
	{ s with
	  str_union_p = Struct;
	  str_fields = new_fields }
  end
  | ((_, BitField _)::_) -> assert false

let rec translate_initializer ~pad_map ~genv typ init = 
  dprintf 5 "cru-ti: %a %a" Ctt_formatter.pp_c_type typ Ctt_formatter.pp_ctt_initalizer init;
  visit_initializer ~v_expr:(translate_expr ~genv) ~v_strinit:(translate_struct_initializer ~pad_map ~genv)
    ~self:(translate_initializer ~pad_map ~genv) typ init

and translate_struct_initializer ~pad_map ~genv ~loc id inits = 
  let desc = get_struct_desc ~genv id in
  match desc.str_union_p with
    Struct ->
      visit_struct_initializer ~genv ~loc ~v_init:(translate_initializer ~pad_map ~genv) id inits
  | Union ->
      match desc.str_fields, inits with
	[], [] -> []
      | (_,NormalField f)::_, [init] ->
	  let new_init = translate_initializer ~pad_map ~genv f.sf_type init in
	  let loc = locget new_init in
	  let pad_size = Earray.get pad_map id in
	  if pad_size ==! zero_big_int then [new_init] else begin
	    dprintf 4 "ctt_reduce_unions add extra %d bytes for struct #%d\n" (int_of_big_int pad_size) id;
	    new_init ::
	    [ locput ~loc
		(CTTinitList
		   (list_repeat
		      (locput ~loc
			 (CTTinitExp 
			    (make_expr 
			       (CTTexpConstant (CTTconstInteger zero_big_int))
			       type_unsigned_char
			       ~loc)))
		      (int_of_big_int pad_size))) ]
	  end
      | (_,BitField _)::_, _ -> assert false
      | _::_,[] | [], _::[] | _,_::_::_ -> assert false

let translate_gdecl ~pad_map ~genv decl = 
  dprintf 5 "cru-tgd: %a" Ctt_formatter.pp_global_declaration decl;
  visit_gdecl ~v_stmt:(translate_statement ~genv) 
    ~v_init:(translate_initializer ~pad_map ~genv) ~v_type:translate_type decl

let translate_global_declarations ~genv = 
  list_map
    (fun (id, gs) -> id, { gs with gbind_type = translate_type gs.gbind_type })

let translate_environment genv = 
  let pad_map = Earray.empty () in
  let str_table = 
    Earray.mapi
      (fun id desc -> 
	let pad_size, new_desc = translate_struct_desc desc in
	Earray.set pad_map id pad_size;
	new_desc)
      genv.struct_table
  in
  let gdecls = translate_global_declarations ~genv genv.global_declarations in
  pad_map,
  { genv with struct_table = str_table; global_declarations = gdecls }

let rec translate_program ~genv prog = 
  let pad_map, new_genv = translate_environment genv in
  let prog = list_map (translate_gdecl ~pad_map ~genv) prog in
  new_genv, prog

