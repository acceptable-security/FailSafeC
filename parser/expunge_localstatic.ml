(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2005. *)

(* move function-local static variable to toplevel *)

open Util
open Locterm
open Ctt_abstree
open C_typing
open Ctt_visitor

let id x = x

let rec reduce_ldecls ~acc ~loc =
  function
      [] -> []
    | (LocalStatic, typ, id, initopt) :: tl ->
	(* TODO : check reference to local auto varialble *)
	acc := (typ, id, initopt, loc) :: !acc;
	reduce_ldecls ~acc ~loc tl
    | dc :: tl ->
	dc :: reduce_ldecls ~acc ~loc tl


let rec reduce_stmt ~acc stmt =
  let self = (reduce_stmt ~acc) in
  match locval stmt with
  | CTTstmtCompound(dcls, stmts) -> 
      let dcls = reduce_ldecls ~loc:(locget stmt) ~acc dcls in
      let stmts = list_map_ordered self stmts in
      loccopy ~orig:stmt (CTTstmtCompound(dcls, stmts))
  | s ->
      visit_statement
	~v_type:id
	~v_ldecl:id (* dummy *)
	~v_expr:id ~self stmt

let rec reduce_gdecl ~genv decl =
  let loc = locget decl in
  match locval decl with
    CTTdeclFunction(gsclass, typ, ident, argnames, body) ->
      let acc = ref [] in
      let b = locput ~loc (CTTdeclFunction(gsclass, typ, ident, argnames, reduce_stmt ~acc body)) in
      let a = List.rev (!acc) in
      let additional_decls = Util.list_map
	  (fun (typ,id,init,loc) -> 
	    locput ~loc (CTTdeclVariable(ModuleStatic,typ,id,init))) a in
      (additional_decls @ [b])
  | CTTdeclVariable(gsclass, typ, ident, initopt) ->
      [decl]

let rec reduce_program ~genv prog = 
  Util.map_flatten (reduce_gdecl ~genv) prog
