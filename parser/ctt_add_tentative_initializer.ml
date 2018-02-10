(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2004. *)

open Big_int
open Ctt_abstree
open C_typing
open Locterm

let fill_bss_initializers = ref true

let initializer_zero ~loc typ = 
  let d = (CTTexpConstant (CTTconstInteger zero_big_int)) in
  locput ~loc (CTTinitExp (make_expr d typ loc))

let initializer_float_zero ~loc typ = 
  let d = (CTTexpConstant (CTTconstFloat 0.0)) in
  locput ~loc (CTTinitExp(make_expr d typ loc))

let initializer_null ~loc typ = 
  let d = (CTTexpConstant CTTconstNull) in
  locput ~loc (CTTinitExp(make_expr d typ loc))

let rec fill_initializer ~genv ~is_static ~loc typ = 
  match typ.ct_ty with
    Tvoid -> assert false
  | Tbuiltin (Tfloat | Tdouble | Tlongdouble) ->
      typ, initializer_float_zero ~loc typ
  | Tbuiltin _ ->
      typ, initializer_zero ~loc typ
  | Tpointer _ ->
      typ, initializer_null ~loc typ
  | Tarray(et, Some sz) ->
      let et, i = fill_initializer ~genv ~is_static ~loc et in
      let szi = int_of_big_int sz in
      let init =
	if !C_typing.fill_zero_initializer 
	then
	  locput ~loc (CTTinitList (Util.list_repeat i szi))
	else
	  locput ~loc (CTTinitList [i])
      in
      Ctt_abstree.make_c_type (Tarray(et, Some sz)), init
  | Tarray(et, None) ->
      if is_static then
	failwith "incomplete static declaration"
      else
	let et, init = fill_initializer ~genv ~is_static ~loc et in
	Ctt_abstree.make_c_type (Tarray(et, Some unit_big_int)),
	locput ~loc (CTTinitList [init])
  | Tstruct(id) ->
      let sdesc = Ctt_abstree.get_struct_desc ~genv id in
      if sdesc.str_size = None then
	failwith "initializing incomplete struct"
      else
	typ,
	locput ~loc 
	  (CTTinitList
	     (fill_struct_initializer ~genv ~is_static ~loc sdesc.str_fields))
  | Tabstract(_) -> assert false
  | Tfunction _ -> assert false (* see reduce_gdecl *)

and fill_struct_initializer ~genv ~is_static ~loc = function
  | [] -> []
  | (_, NormalField { sf_type = t }) :: tl ->
      snd (fill_initializer ~genv ~is_static ~loc t) :: fill_struct_initializer ~genv ~is_static ~loc tl
  | (_, BitField { s_bf_fields = bfs }) :: tl ->
      let rec iter = function
	  [] -> fill_struct_initializer ~genv ~is_static ~loc tl
	| (None, _, _, _)::l -> iter l
	| (Some _, t, _, _)::l ->
	    snd (fill_initializer ~genv ~is_static ~loc t) :: iter l
      in
      iter bfs

let reduce_gdecl ~genv gd =
  let loc = locget gd in
  match locval gd with
    CTTdeclFunction(_, typ, ident, _, _)
  | CTTdeclVariable(_, typ, ident, Some _)
  | CTTdeclVariable(Extern _, typ, ident, None) ->
      gd
  | CTTdeclVariable((Global _ | ModuleStatic) as sclass, typ, ident, None) -> begin
      let is_static = sclass = ModuleStatic in
      match typ.ct_ty with
	Tfunction _ -> 
	  locput ~loc
	    (CTTdeclVariable
	       ((if is_static then ModuleStatic else Extern []),
		typ, ident, None))
      | _ ->
	  if is_static || !fill_bss_initializers then
	    let typ, init = fill_initializer ~genv ~is_static ~loc typ in
	    locput ~loc (CTTdeclVariable (sclass, typ, ident, Some init))
	  else
	    locput ~loc (CTTdeclVariable (sclass, typ, ident, None))
  end

let rec reduce_program ~genv prog = 
  let prog = Util.list_map (reduce_gdecl ~genv) prog in
  genv, prog
