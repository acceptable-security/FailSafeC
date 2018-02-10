(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2006. *)

open Big_int
open Locterm
open Ctt_abstree
open Cttm_abstree
open Util

let dprintf l f = Debug.dprintf ~category:111 l f
let get_debug_flags () = Debug.get_debug_flags ~category:111

let make_expr ~loc ~typ e = 
  Locterm.locput ~loc { expr_t = e; expr_type = typ }

let is_arraytype t =
  match t.ct_ty with
    Tarray _ -> true
  | _ -> false
	
let rec translate_expr e = 
  let loc, e = locget e, locval e in
  let desc = 
    match e.mexpr_t with
    | CTTMexpComma(e1,e2) ->
	CTTexpComma(translate_expr e1, translate_expr e2)

    | CTTMexpAddress(CTTMlvVar(id,typ),[]) ->
	if is_arraytype e.mexpr_type then
	  CTTexpVar(id, typ)
	else
	  CTTexpAddress(make_expr ~loc ~typ (CTTexpVar (id, typ)))
    | CTTMexpAddress(CTTMlvPtr(e),[]) ->
	(* meaningful, but should not happen *)
	assert false (* ; (translate_expr e).expr_t *)
    | CTTMexpAddress(CTTMlvRvalue _,[]) ->
	assert false
    | CTTMexpAddress(e1,fs) ->
	CTTexpAddress(convert_field ~loc e1 fs)

    | CTTMexpRead(e1,fs) ->
	(convert_field ~loc e1 fs).locterm_v.expr_t
    | CTTMexpWrite(memobj,fs,None,e2) -> 
	CTTexpAssign(convert_field ~loc memobj fs, translate_expr e2)
    | CTTMexpWrite(memobj,fs,Some (binop,tcast),e2) -> 
	CTTexpBinAssign(binop, convert_field ~loc memobj fs, tcast, translate_expr e2)
    | CTTMexpConditional(e1,e2,e3) ->
	CTTexpConditional(translate_expr e1, translate_expr e2, translate_expr e3)
    | CTTMexpBinExpr(binop,e1,e2) ->
	CTTexpBinExpr(binop, translate_expr e1, translate_expr e2)
    | CTTMexpCoerce(ty,e1) ->
	CTTexpCoerce(ty,translate_expr e1)
    | CTTMexpUnaryExpr(uop,e1) ->
	CTTexpUnaryExpr(uop, translate_expr e1)
    | CTTMexpInvoke(lhs,elist) ->
	let lhs = translate_invoke_lhs ~loc lhs in
	CTTexpInvoke(lhs, list_map translate_expr elist)
    | CTTMexpConstant(c) ->
	CTTexpConstant(c)
  in
  make_expr ~loc ~typ:(e.mexpr_type) desc

and translate_invoke_lhs ~loc = function
    CTTMlvPtr(e) ->
      deref_exp e
  | CTTMlvVar(v,typ) ->
      make_expr ~loc ~typ (CTTexpVar (v, typ))
  | CTTMlvRvalue _ ->
      assert false

and deref_exp e = 
  let t = CTTexpPtrDeref(translate_expr e) in
  let typ = match e.locterm_v.mexpr_type.ct_ty with
    Tpointer t -> t
  | _ -> (dprintf (-1) "deref_exp: panic: %a" Ctt_formatter.pp_mexpr e); assert false
  in
  make_expr ~loc:(locget e) ~typ t

and convert_field ~loc lhs fs = 
  let lhs = match lhs with
    CTTMlvPtr(e) -> deref_exp e
  | CTTMlvVar(v,typ) -> make_expr ~loc ~typ (CTTexpVar (v, typ))
  | CTTMlvRvalue(e) ->
      translate_expr e
  in
  let rec iter lhs = function
      [] -> lhs
    | (f,typ)::tl -> 
	iter (make_expr ~loc ~typ (CTTexpField(lhs,f))) tl
  in
  iter lhs fs

let rec translate_initializer i =
  locmap
  (function
      CTTMinitExp e -> CTTinitExp (translate_expr e)
    | CTTMinitList l -> CTTinitList (list_map translate_initializer l))
    i

let rec translate_declaration (sclass, typ, ident, init) = 
  sclass, typ, ident, Option.map translate_initializer init
