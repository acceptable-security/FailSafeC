(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Generic visitor for IL3 program *)

open Locterm
open Util
open Il
open Il3

let id x = x

(** same type *)

let visit_lv ~v_src = function
    ILlvVar _ as l -> l
  | ILlvPtr v -> ILlvPtr (v_src v)
  | ILlvTemp v -> ILlvTemp (v_src v)

let visit_rexp ~v_src = function
  | IL3RexCoerce(ct, tid) -> IL3RexCoerce(ct, v_src tid)
  | IL3RexConstant _
  | IL3RexArgument _
  | IL3RexUndefined
      as e -> e
  | IL3RexBinop(bop, t1, t2) -> IL3RexBinop(bop, v_src t1, v_src t2)
  | IL3RexAddress(lv, flds) -> 
      IL3RexAddress(visit_lv ~v_src lv, flds)
  | IL3RexUnaryop(u, t) -> IL3RexUnaryop(u, v_src t)

let visit_iexp ~v_src = function
  | IL3IexInvoke(lv, args) ->
      IL3IexInvoke(visit_lv ~v_src lv, list_map_ordered v_src args)
  | IL3IexBinop(bop, t1, t2) -> IL3IexBinop(bop, v_src t1, v_src t2)

let visit_instr ~self ~v_src ~v_dest i = 
  let r = 
    match (locval i).il3_desc with
    | IL3stmtCallReducibleFun(d, rf, srcs) ->
	IL3stmtCallReducibleFun(v_dest d, rf, list_map_ordered v_src srcs)
    | IL3stmtCallImmobileFun(d, imf, srcs) ->
	IL3stmtCallImmobileFun(v_dest d, imf, list_map_ordered v_src srcs)
    | IL3stmtCallReaderHelper(d, sz, s1, s2) ->
	IL3stmtCallReaderHelper(v_dest d, sz, v_src s1, v_src s2)
    | IL3stmtMove(d, t, s) -> IL3stmtMove(v_dest d, t, v_src s)
    | IL3stmtRexp(d, t, e) -> IL3stmtRexp(v_dest d, t, visit_rexp ~v_src e)
    | IL3stmtIexp(d, t, e) -> IL3stmtIexp(v_dest d, t, visit_iexp ~v_src e)
    | IL3stmtReadRaw(d, ty, lv, flds) -> IL3stmtReadRaw(v_dest d, ty, visit_lv ~v_src lv, flds)
    | IL3stmtCallReducibleFunOverwriting(d, rf, srcs) ->
	IL3stmtCallReducibleFunOverwriting(v_dest d, rf, list_map_ordered v_src srcs)
    | IL3stmtCallImmobileOp(imo, srcs) -> IL3stmtCallImmobileOp(imo, list_map_ordered v_src srcs)
    | IL3stmtCallAbort(t1, t2) -> IL3stmtCallAbort(v_src t1, v_src t2)      
    | IL3stmtCallWriterHelper(sz, t1, t2, t3, t4o) ->
	IL3stmtCallWriterHelper(sz, v_src t1, v_src t2, v_src t3, Option.map v_src t4o)
    | IL3stmtWriteRaw(lv, fld, tid) -> IL3stmtWriteRaw(visit_lv ~v_src lv, fld, v_src tid)
    | IL3stmtIf(ift, t1, bt) -> IL3stmtIf(ift, v_src t1, bt)
    | IL3stmtGoto bt as s -> s
    | IL3stmtSwitch(t1, bts) -> IL3stmtSwitch(v_src t1, bts)
    | IL3stmtReturn(t) -> IL3stmtReturn(Option.map v_src t)
	
    | IL3stmtSequence l
      -> IL3stmtSequence(list_map_ordered self l)
    | IL3stmtParallel l
      -> IL3stmtParallel(list_map_ordered self l)
    | IL3stmtConditional (vs, i, i1, i2) ->
	IL3stmtConditional(vs, v_src i, self i1, self i2)
  in
  loccopy ~orig:i { il3_desc = r }

let visit_block ~v_instr ~v_phi b = 
  { b with
    code = list_map_ordered (v_instr : 'a il3 -> 'b il3) b.code;
    phi_function = list_map_ordered v_phi b.phi_function
  }

let visit_function ~v_block ~v_prologue f = 
  { f with
    body = Array.map v_block f.body;
    prologue = v_prologue f.prologue }

let visit_gdecl_for_function ~v_func = 
  locmap_list
    (function
	IL3declFunction(gs, ct, id, args, f) ->
	  IL3declFunction(gs, ct, id, args, v_func ~fname:id f)
      | IL3declVariable _ as vdecl ->
	  vdecl)

(** unit type *)

let scan_lv ~v_src = function
    ILlvVar _ -> ()
  | ILlvPtr v -> v_src v
  | ILlvTemp v -> v_src v

let scan_rexp ~v_src = function
  | IL3RexCoerce(ct, tid) -> v_src tid
  | IL3RexConstant _
  | IL3RexArgument _
  | IL3RexUndefined -> ()
  | IL3RexBinop(bop, t1, t2) ->v_src t1; v_src t2
  | IL3RexAddress(lv, flds) -> 
      scan_lv ~v_src lv
  | IL3RexUnaryop(u, t) -> v_src t

let scan_iexp ~v_src = function
  | IL3IexInvoke(lv, args) ->
      scan_lv ~v_src lv; List.iter v_src args
  | IL3IexBinop(bop, t1, t2) -> v_src t1; v_src t2

let scan_instr ~self ~v_src ~v_dest i = 
  match (locval i).il3_desc with
  | IL3stmtCallReducibleFun(d, rf, srcs)
  | IL3stmtCallReducibleFunOverwriting(d, rf, srcs) ->
      v_dest d; List.iter v_src srcs
  | IL3stmtCallImmobileFun(d, imf, srcs) ->
      v_dest d; List.iter v_src srcs
  | IL3stmtCallReaderHelper(d, sz, s1, s2) ->
      v_dest d; v_src s1; v_src s2
  | IL3stmtMove(d, t, s) -> v_dest d; v_src s
  | IL3stmtRexp(d, t, e) -> v_dest d; scan_rexp ~v_src e
  | IL3stmtIexp(d, t, e) -> v_dest d; scan_iexp ~v_src e
  | IL3stmtReadRaw(d, ty, lv, flds) -> v_dest d; scan_lv ~v_src lv
  | IL3stmtCallImmobileOp(imo, srcs) -> List.iter v_src srcs
  | IL3stmtCallAbort(t1, t2) -> v_src t1; v_src t2
  | IL3stmtCallWriterHelper(sz, t1, t2, t3, t4o) ->
      v_src t1; v_src t2; v_src t3; Option.iter v_src t4o
  | IL3stmtWriteRaw(lv, fld, tid) -> scan_lv ~v_src lv; v_src tid
    | IL3stmtIf(ift, t1, bt) -> v_src t1
    | IL3stmtGoto bt -> ()
    | IL3stmtSwitch(t1, bts) -> v_src t1
    | IL3stmtReturn(t) -> Option.iter v_src t
	
    | IL3stmtSequence l
      -> List.iter self l
    | IL3stmtParallel l
      -> List.iter self l
    | IL3stmtConditional (vs, i, i1, i2) ->
	v_src i; self i1; self i2

let scan_block ~v_instr ~v_phi b = 
  List.iter (v_instr : 'a il3 -> unit) b.code;
  List.iter v_phi b.phi_function

let scan_function ~v_block ~v_prologue f = 
  Array.iter v_block f.body;
  (v_prologue : _ -> unit) f;
  ()

