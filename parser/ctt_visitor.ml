(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2006. *)

open Util
open Locterm
open Ctt_abstree

let visit_type ~self typ = 
  let ct_ty =
    match typ.ct_ty with
    | Tpointer t -> Tpointer (self t)
    | Tfunction(at, v, rt) -> Tfunction(list_map_ordered self at, v, self rt)
    | Tarray(at, sz) -> Tarray(self at, sz)
    | Tbuiltin _
    | Tstruct _
    | Tvoid
    | Tabstract _ -> typ.ct_ty
  in
  { typ with ct_ty = ct_ty }

let visit_expr ~v_type ~self expr = 
  locmap
    (fun expr ->
      let body = match expr.expr_t with
	CTTexpComma(e1,e2) -> 
	  let e1 = self e1 in
	  let e2 = self e2 in
	  CTTexpComma(e1, e2)
      | CTTexpAssign(e1,e2) ->
	  let e1 = self e1 in
	  let e2 = self e2 in
	  CTTexpAssign(e1, e2)
      | CTTexpBinAssign(binop,e1,t_cast,e2) ->
	  let e1 = self e1 in
	  let e2 = self e2 in
	  let t_cast = Option.map v_type t_cast in
	  CTTexpBinAssign(binop, e1, t_cast, e2)
      | CTTexpConditional(e1,e2,e3) ->
	  let e1 = self e1 in
	  let e2 = self e2 in
	  let e3 = self e3 in
	  CTTexpConditional(e1, e2, e3)
      | CTTexpBinExpr(binop,e1,e2) ->
	  let e1 = self e1 in
	  let e2 = self e2 in
	  CTTexpBinExpr(binop, e1, e2)
      | CTTexpCoerce(t,e1) -> CTTexpCoerce(v_type t,self e1)
      | CTTexpUnaryExpr(uop,e1) -> CTTexpUnaryExpr(uop,self e1)
      | CTTexpAddress(e1) -> CTTexpAddress(self e1)
      | CTTexpPtrDeref(e1) -> CTTexpPtrDeref(self e1)
      | CTTexpInvoke(e1,es) ->
	  let e1 = self e1 in
	  let es = list_map_ordered self es in
	  CTTexpInvoke(e1, es)
      | CTTexpField(e1,id) -> CTTexpField(self e1,id)
      | CTTexpConstant(_)  -> expr.expr_t
      | CTTexpVar(i,t) -> CTTexpVar(i, v_type t)
      in
      { expr_t = body; expr_type = v_type expr.expr_type })
    expr

let visit_statement ~v_type ~v_expr ~v_ldecl ~self stmt = 
  locmap
    (function 
	CTTstmtNull -> CTTstmtNull
      | CTTstmtExpr(e) -> CTTstmtExpr(v_expr e)
      | CTTstmtLabeled(l,s) -> CTTstmtLabeled(l,self s)
      | CTTstmtCase_Labeled(l,s) -> CTTstmtCase_Labeled(l,self s)
      | CTTstmtDefault_Labeled(s) -> CTTstmtDefault_Labeled(self s)
      | CTTstmtCompound(dcls, stmts) -> 
	  let dcls = list_map_ordered v_ldecl dcls in
	  let stmts = list_map_ordered self stmts in
	  CTTstmtCompound(dcls, stmts)
      | CTTstmtIf(e,s_th,s_el) -> 
	  let e = v_expr e in
	  let s_th = self s_th in
	  let s_el = Option.map self s_el in
	  CTTstmtIf(e, s_th, s_el)
      | CTTstmtSwitch(e,s) ->
	  let e = v_expr e in
	  CTTstmtSwitch(e, self s)
      | CTTstmtWhile(e,s) -> 
	  let e = v_expr e in
	  CTTstmtWhile(e, self s)
      | CTTstmtDoWhile(s,e) ->
	  let s = self s in
	  let e = v_expr e in
      CTTstmtDoWhile(s, e)
      | CTTstmtFor(e1,e2,e3,s) ->
	  let e1 = Option.map v_expr e1 in
	  let e2 = Option.map v_expr e2 in
	  let e3 = Option.map v_expr e3 in
	  CTTstmtFor(e1, e2, e3, self s)
      | CTTstmtGoto(_) | CTTstmtContinue | CTTstmtBreak as st -> st
      | CTTstmtReturn(e) -> CTTstmtReturn(Option.map v_expr e))
    stmt

let visit_initializer ~v_expr ~v_strinit ~self typ init = 
  match typ.ct_ty, locval init with
  | Tarray (et, _), CTTinitList l ->
      let nl = list_map_ordered (self et) l in
      loccopy ~orig:init (CTTinitList nl)
  | Tstruct id, CTTinitList l ->
      loccopy ~orig:init (CTTinitList (v_strinit ~loc:(locget init) id l))
  | Tstruct id, CTTinitExp i ->
      loccopy ~orig:init (CTTinitExp (v_expr i)) (* struct copy initialization *)
  | (Tarray _ | Tpointer _ | Tbuiltin _), CTTinitExp i ->
      loccopy ~orig:init (CTTinitExp (v_expr i))
  | (Tfunction _ | Tvoid), _ -> assert false
  | _ -> assert false

let visit_struct_initializer ~genv ~loc ~v_init id inits =
  let desc = get_struct_desc ~genv id in
  match desc.str_union_p with
    Struct -> begin
      let rec iter fields inits = 
	match fields, inits with
	  [], [] -> []
	| (_, NormalField { sf_type = t }) :: tl, ini::rest ->
	    (v_init t ini) ::
	    iter tl rest
	| (_, BitField { s_bf_fields = bf }) :: tl, inits ->
	    iter_bfields tl bf inits
	| [], _::_ | _::_, [] -> assert false
      and iter_bfields rest_fields bfields inits = 
	match bfields, inits with
	  [], rest_inits -> iter rest_fields inits 
	| (_,t,_,_)::tl, (init :: rest) ->
	    (v_init t init)
	    :: iter_bfields rest_fields tl rest
	| _::_, [] -> assert false
      in
      iter desc.str_fields inits
  end
  | Union -> begin
      match desc.str_fields, inits with
	[], [] -> []
      | (_, NormalField { sf_type = t })::_, [init] -> [v_init t init]
      | _::_, [] -> failwith "panic: visitor 130"
      | [], _::_ -> failwith "panic: visitor 131"
      | _ -> assert false
  end

let visit_ldecl ~v_init ~v_type (lsc, ty, id, initopt) = 
  lsc, v_type ty, id, Option.map (v_init ty) initopt

let visit_gdecl ~v_stmt ~v_init ~v_type gd =
  loccopy ~orig:gd
    (match locval gd with
      CTTdeclFunction(gsclass, typ, ident, argnames, body) ->
	CTTdeclFunction(gsclass, v_type typ, ident, argnames, v_stmt body)
    | CTTdeclVariable(gsclass, typ, ident, initopt) ->
	CTTdeclVariable(gsclass, v_type typ, ident, 
			Option.map (v_init typ) initopt))

