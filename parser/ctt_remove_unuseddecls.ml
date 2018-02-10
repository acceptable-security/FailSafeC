(* Part of Fail-Safe C Compiler.
   Produced by Yutaka Oiwa.
   (c) 2005- AIST. *)

open Util
open Locterm
open Ctt_abstree
open Ctt_visitor

type environment = 
  { genv: Ctt_abstree.environment;
    structs_used: bool earray;
    values_declared: (identifier,unit) Hashtbl.t;
    values_used: (identifier,unit) Hashtbl.t;
  }

let external_definition_filter_hook = ref (fun ~ext ty id x -> (x : bool))

(* phase 1: make a list of declarations. *)

let phase1 ~genv t = 
  let structs_used = Earray.empty_with_default ~zero:false in
  let values_declared = Hashtbl.create 16 in
  let values_used = Hashtbl.create 16 in
  List.iter 
    (fun gdecl -> match locval gdecl with
	CTTdeclFunction(_gs,_ct,id,_argids,_stmt) ->
	  Hashtbl.add values_declared id ()
      | CTTdeclVariable(gs,_ct,id,initopt) -> begin
	  match gs with
	    Global _ | ModuleStatic -> Hashtbl.add values_declared id ()
	  | Extern _ ->
	      if initopt <> None then
		Hashtbl.add values_declared id ()
      end
    ) t;
  { genv = genv;
    structs_used = structs_used;
    values_declared = values_declared;
    values_used = values_used }

let rec walk_type ~env typ = 
  let v = visit_type ~self:(walk_type ~env) in
  match typ.ct_ty with
    Tstruct sid ->
      let visited = 
	Earray.get env.structs_used sid in
      Earray.set env.structs_used sid true; 
      if not visited then begin
	let s = get_struct_desc ~genv:env.genv sid in
	List.iter 
	  (function
	      _, NormalField {sf_type = t} -> ignore (walk_type ~env t)
	    | _, BitField { s_bf_fields = l } ->
		List.iter 
		  (fun (ido, ct, _, _) -> ignore (walk_type ~env ct)) l)
	  s.str_fields;
      end;
      v typ
  | _ -> v typ

let phase2 ~env t = 
  let walk_type = walk_type ~env in
  let rec walk_expr expr = 
    let v = visit_expr ~self:walk_expr ~v_type:walk_type in
    match expr.locterm_v.expr_t with
      CTTexpVar(id, _t) ->
	Hashtbl.add env.values_used id (); v expr
    | CTTexpConstant(CTTconstTypeInfo t) -> ignore (walk_type t); v expr
    | _ -> v expr
  in
  let rec walk_init typ init = 
    visit_initializer ~v_expr:walk_expr ~v_strinit:walk_struct_initializer
      ~self:walk_init typ init
  and walk_struct_initializer ~loc id inits = 
    visit_struct_initializer ~loc ~genv:env.genv ~v_init:walk_init id inits
  in
  let rec walk_ldecl ldecl = 
    visit_ldecl ~v_init:walk_init ~v_type:walk_type ldecl
  in
  let rec walk_statement stmt = 
    visit_statement
      ~v_type:walk_type ~v_expr:walk_expr ~v_ldecl:walk_ldecl ~self:walk_statement 
      stmt
  in
  let walk_gdecl gdecl = 
    let v = visit_gdecl ~v_type:walk_type ~v_init:walk_init ~v_stmt:walk_statement in
    match locval gdecl with
      CTTdeclFunction _
    | CTTdeclVariable((Global _ | ModuleStatic),_,_,_) 
    | CTTdeclVariable(Extern _, _, _, Some _) -> ignore (v gdecl)
    | CTTdeclVariable(Extern _, _, _, None) -> ()
  in
  List.iter walk_gdecl t

let do_extfilter ~env ~ext ty id x = 
  if ext = [] then x else
  let new_x = !external_definition_filter_hook ~ext ty id x in
  if new_x && not x then
    ignore (walk_type ~env ty);
  new_x

let phase3 ~env t = 
  let t = List.filter
      (fun gdecl -> match locval gdecl with
	  CTTdeclFunction _
	| CTTdeclVariable((Global _ | ModuleStatic),_,_,_)  -> true
	| CTTdeclVariable(Extern ext, ty, id, Some _) ->
	    Debug.dprintf ~category:26 6 "filter 1: %s %b" id (ext <> []);
	    do_extfilter ~env ~ext ty id true
	| CTTdeclVariable(Extern ext, ty, id, None) -> 
	    Debug.dprintf ~category:26 6 "filter 2: %s %b" id (ext <> []);
	    do_extfilter ~env ~ext ty id
	      (Hashtbl.mem env.values_used id &&
	       not (Hashtbl.mem env.values_declared id)))
      t
  in
  let new_struct_table = 
    Earray.filter_i
      (fun id s -> Earray.get env.structs_used id) 
      env.genv.struct_table
  in
  let new_struct_name_table = 
    List.filter
      (function _, id -> Earray.get env.structs_used id)
      env.genv.struct_name_table 
  in
  { env.genv with
    struct_table = new_struct_table;
    struct_name_table = new_struct_name_table }, t

let f ~genv t = 
  let env = phase1 ~genv t in
  phase2 ~env t;
  let genv, t = phase3 ~env t in
  Ctt_abstree.reconstruct_gdecl_table ~genv t, t
