(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2006. *)

open Big_int
open Locterm
open Ctt_abstree
open Cttm_abstree

exception TypeError_untyped of C_abstree.expr * string
exception TypeError_typed of expr * string
exception TypeError_typed_t of expr * string * c_type
exception Type_Error_local of string
exception NotConstant

let rec translate_field e1 ?(rvalue = false) id typ =
  match e1.locterm_v.expr_t with
    CTTexpPtrDeref(e2) ->
      CTTMlvPtr(translate e2), [id, typ]
  | CTTexpVar(id2, vty) ->
      CTTMlvVar(id2, vty), [id, typ]
  | CTTexpField(e2, id2) ->
      let l, r = translate_field ~rvalue e2 id2 e1.locterm_v.expr_type in
      l, (r @ [id, typ])
  | CTTexpInvoke _
  | CTTexpConditional _ when rvalue ->
      CTTMlvRvalue(translate e1), [id, typ]
  | _ -> failwith "invalid LHS for . operator"

and translate (e : expr) : mexpr = 
  let body : mexpr_desc = match e.locterm_v.expr_t with
    CTTexpComma(e1,e2) -> CTTMexpComma(translate e1, translate e2)
  | CTTexpConditional(e1,e2,e3) -> CTTMexpConditional(translate e1, translate e2, translate e3)
  | CTTexpBinExpr(binop,e1,e2) -> CTTMexpBinExpr(binop, translate e1, translate e2)
  | CTTexpCoerce(t,e1) -> CTTMexpCoerce(t, translate e1)
  | CTTexpUnaryExpr(u,e1) -> CTTMexpUnaryExpr(u, translate e1)
  | CTTexpInvoke(e1,args) -> CTTMexpInvoke(translate_invoke_lhs e1, Util.list_map translate args)
  | CTTexpConstant(c) -> CTTMexpConstant(c)
	
  | CTTexpAddress(e) -> translate_address e
  | CTTexpPtrDeref(e) -> CTTMexpRead(CTTMlvPtr (translate e),[])
  | CTTexpAssign(e1,e2) -> translate_assign e1 (translate e2) None
  | CTTexpBinAssign(binop,e1,tcast,e2) -> translate_assign e1 (translate e2) (Some (binop, tcast))
  | CTTexpField(e1,id) -> 
      let l,r = translate_field ~rvalue:true e1 id e.locterm_v.expr_type in
      CTTMexpRead(l,r)
  | CTTexpVar(id, vty) -> begin
      match e.locterm_v.expr_type.ct_ty with
	Tarray(_) ->
	  CTTMexpAddress(CTTMlvVar(id, vty), [])
      | _ ->
	  CTTMexpRead(CTTMlvVar(id, vty), [])
  end
  in
  loccopy ~orig:e { mexpr_type = e.locterm_v.expr_type; mexpr_t = body }
    
and translate_address e1 =
  match e1.locterm_v.expr_t with
    CTTexpPtrDeref(e2) -> (translate e2).locterm_v.mexpr_t
  | CTTexpVar(id, vty) -> CTTMexpAddress(CTTMlvVar(id, vty), [])
  | CTTexpField(e2,id) ->
      let l, r = translate_field e2 id e1.locterm_v.expr_type in
      CTTMexpAddress(l,r)
  | _ -> raise (TypeError_typed(e1,"not an lvalue"))

and translate_invoke_lhs e1 =
  match e1.locterm_v.expr_t with
    CTTexpPtrDeref(e) -> CTTMlvPtr(translate e)
  | CTTexpVar(id, vty) -> CTTMlvVar(id, vty)
  | CTTexpField _ -> assert false
  | _ -> assert false

and translate_assign e1 rhs binop =
  match e1.locterm_v.expr_t with
    CTTexpPtrDeref(e) -> CTTMexpWrite(CTTMlvPtr(translate e), [], binop, rhs)
  | CTTexpVar(id, vty) -> CTTMexpWrite(CTTMlvVar(id, vty), [], binop, rhs)
  | CTTexpField(e2,id) ->
      let l, r = translate_field e2 id e1.locterm_v.expr_type in
      CTTMexpWrite(l, r, binop, rhs)
  | _ -> raise (TypeError_typed(e1,"not an lvalue"))
  
let rec translate_initializer init =
  match locpair init with
    loc, CTTinitExp e -> locput ~loc (CTTMinitExp (translate e))
  | loc, CTTinitList l -> locput ~loc (CTTMinitList (Util.list_map translate_initializer l))

let rec translate_declaration (sclass, typ, ident, init) = 
  sclass, typ, ident, Option.map translate_initializer init

let rec translate_statement stmt = 
  let ndesc = match locval stmt with
    CTTstmtNull -> CTTMstmtNull
  | CTTstmtExpr e1 -> CTTMstmtExpr(translate e1)
  | CTTstmtLabeled(id, s1) -> CTTMstmtLabeled(id, translate_statement s1)
  | CTTstmtCase_Labeled(l, s1) -> CTTMstmtCase_Labeled(l, translate_statement s1)
  | CTTstmtDefault_Labeled s1 -> CTTMstmtDefault_Labeled(translate_statement s1)
  | CTTstmtCompound(dcls,stmts) ->
      CTTMstmtCompound(Util.list_map translate_declaration dcls, Util.list_map translate_statement stmts)
  | CTTstmtIf(e1, s1, s2) ->
      CTTMstmtIf(translate e1, translate_statement s1, Option.map translate_statement s2)
  | CTTstmtSwitch(e1, s1) ->
      CTTMstmtSwitch(translate e1, translate_statement s1)
  | CTTstmtWhile(e1, s1) ->
      CTTMstmtWhile(translate e1, translate_statement s1)
  | CTTstmtDoWhile(s1, e1) ->
      CTTMstmtDoWhile(translate_statement s1, translate e1)
  | CTTstmtFor(e1,e2,e3,s1) ->
      CTTMstmtFor(Option.map translate e1,
		  Option.map translate e2,
		  Option.map translate e3,
		  translate_statement s1)
  | CTTstmtGoto(id) -> CTTMstmtGoto(id)
  | CTTstmtContinue -> CTTMstmtContinue
  | CTTstmtBreak -> CTTMstmtBreak
  | CTTstmtReturn e1 ->
      CTTMstmtReturn(Option.map translate e1)
  in
  loccopy ~orig:stmt ndesc

let translate_global_declaration = 
  locmap
    (function
	CTTdeclFunction (gs,ct,id,args,body) ->
	  CTTMdeclFunction(gs, ct, id, args, translate_statement body)
      | CTTdeclVariable (gs,ct,id,init) ->
	  CTTMdeclVariable(gs, ct, id, Option.map translate_initializer init))

let translate_program = Util.list_map translate_global_declaration
