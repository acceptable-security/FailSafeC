(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(* fixup IL3 program: fill any untyped declarations *)

open Util
open Locterm
open Ctt_abstree
open Il
open Il3
open Il3_constants

type environment = 
  { vt : Ctt_abstree.c_type il_variable_attribute_base earray;
    genv : Ctt_abstree.environment
  }

let set ~env i t = 
  if Earray.mem env.vt i then
    ()
  else
    Earray.set env.vt i 
      { original_name = None; variable_type = t; storage_class = Register }

let rec scan_inst ~env i = 
  match (locval i).il3_desc with
  | IL3stmtCallReducibleFun (tid, rf, _)
    -> set ~env tid (rettype_reducible_functions ~genv:env.genv rf)
  | IL3stmtCallImmobileFun (tid, imf, _)
    -> set ~env tid (rettype_immobile_functions ~genv:env.genv imf)
  | IL3stmtCallReaderHelper (tid, rf, _, _)
    -> set ~env tid (rettype_reader_helper ~genv:env.genv rf)
  | IL3stmtMove (tid, ty, _)
  | IL3stmtRexp (tid, ty, _)
  | IL3stmtIexp (tid, ty, _)
  | IL3stmtReadRaw (tid, ty, _, _) ->
      set ~env tid ty

  | IL3stmtCallReducibleFunOverwriting _ -> () (* there should be another RF call *)

  | IL3stmtCallImmobileOp _
  | IL3stmtCallAbort _
  | IL3stmtCallWriterHelper _
  | IL3stmtWriteRaw _
    -> ()
	
  | IL3stmtIf _
  | IL3stmtGoto _
  | IL3stmtSwitch _ 
  | IL3stmtReturn _
    -> ()
	
  | IL3stmtSequence l
  | IL3stmtParallel l -> List.iter (scan_inst ~env) l
  | IL3stmtConditional (vs, i, i1, i2) ->
      scan_inst ~env i1; scan_inst ~env i2

let scan_function ~genv f = 
  let env = 
    { vt = f.variable_environment;
      genv = genv }
  in
  Array.iter 
    (fun b ->
      List.iter (scan_inst ~env) b.code;
      List.iter 
	(fun (d, sl) ->
	  assert (Earray.mem env.vt d))
	b.phi_function
    ) f.body;
  () (* f.variable_environment is inplace-modified *)

let translate_program ~genv p = 
  lociter_list
    (function
	IL3declFunction (gs, ct, id, args, f) ->
	  scan_function ~genv f; ()
      | IL3declVariable _ ->
	  ())
    p;
  p
