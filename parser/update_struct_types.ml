(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

open Util
open Ctt_abstree
open Ctt_visitor

let id x = x

let rec reduce_type ~genv t = 
  let t' = reset_struct_size ~genv t in
  if (t.ct_size == None && t'.ct_size != None) then
    Debug.dprintf ~category:15 5 "Info: size of %a updated.@." Ctt_formatter.pp_c_type t;
  visit_type ~self:(reduce_type ~genv) t'

let rec reduce_expr ~genv t = 
  visit_expr 
    ~v_type:(reduce_type ~genv)
    ~self:(reduce_expr ~genv)
    t

let rec reduce_initializer ~genv t = 
  visit_initializer
    ~v_expr:(reduce_expr ~genv)
    ~v_strinit:(reduce_struct_initializer ~genv)
    ~self:(reduce_initializer ~genv)
    t

and reduce_struct_initializer ~genv = 
  visit_struct_initializer ~genv
    ~v_init:(reduce_initializer ~genv)

let reduce_ldecl ~genv = 
  visit_ldecl ~v_init:(reduce_initializer ~genv)
    ~v_type:(reduce_type ~genv)

let rec reduce_stmt ~genv t = 
  visit_statement
    ~v_type:(reduce_type ~genv)
    ~v_expr:(reduce_expr ~genv)
    ~v_ldecl:(reduce_ldecl ~genv)
    ~self:(reduce_stmt ~genv)
    t

let reduce_gdecl ~genv decl = 
  Locterm.locmap
  (function 
    CTTdeclFunction(gsclass, typ, ident, argnames, body) ->
      let typ = reduce_type ~genv typ in
      let body = reduce_stmt ~genv body in
      CTTdeclFunction(gsclass, typ, ident, argnames, body)
    | CTTdeclVariable(gsclass, typ, ident, initopt) ->
	let typ = reduce_type ~genv typ in
	let initopt = Option.map (reduce_initializer ~genv typ) initopt in
	CTTdeclVariable(gsclass, typ, ident, initopt))
    decl

let reduce_struct_field ~genv = function
    ofs, NormalField sf -> ofs, NormalField { sf with sf_type = reduce_type ~genv sf.sf_type }
  | ofs, BitField bfs -> ofs, BitField bfs (* bitfield should contain only primitive types *)

let reduce_struct_desc ~genv sd = 
  { sd with 
    str_fields = list_map (reduce_struct_field ~genv) sd.str_fields;
    str_fields_byname = 
    list_map
      (fun (id, (typ, ofs)) -> 
	id, ((reduce_type ~genv typ), ofs)) sd.str_fields_byname }

let reduce_struct_table ~genv = 
  Earray.map (fun sd -> reduce_struct_desc ~genv sd)

let reduce_global_declarations ~genv = 
  list_map
    (fun (id, gs) -> id, { gs with gbind_type = reduce_type ~genv gs.gbind_type })

let reduce_genv genv = 
  { genv with
    struct_table = reduce_struct_table ~genv genv.struct_table;
    global_declarations = reduce_global_declarations ~genv genv.global_declarations
  }

let reduce_program ~genv prog = 
  reduce_genv genv,
  list_map_ordered (reduce_gdecl ~genv) prog
