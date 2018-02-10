(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007. *)

(* type analysis for polymorphic pointer values *)

(* phase 1: collect use types and predecessors *)

open Util
open Locterm
open Ctt_abstree
open Il
open Ils
open Ils_formatter

module VarSet = Set.Make (struct type t = int let compare (x:int) (y:int) = compare x y end)

include Debug.Install (struct let category = 81 end)

type environment = 
    { f : (ils,ilr_type) il_function_base;
      predecessors : VarSet.t array;
      usetypes : base_var_usetype_info; }

let update_predecessor ~env v vp = 
  env.predecessors.(v) <-
    VarSet.add vp env.predecessors.(v)

let merge t1 t2 = 
  match t1, t2 with
  | Bunknown, _ -> Bunknown
  | _, Bunknown -> Bunknown

  | Bunanalyzed, t -> t
  | t, Bunanalyzed -> t

  | Brettype, t -> Brettype
  | t, Brettype -> Brettype (* Brettype is stronger *)

  | Bused ct1, Bused ct2 when C_typing.equal_type ct1 ct2 -> t1 (* return left side *)
  | Bused _, Bused _ -> Bunknown

  | Bcast _, Bused _ -> t2 (* accessed is stronger than just cast *)
  | Bused _, Bcast _ -> t1

  | Bcast ct1, Bcast ct2 when C_typing.equal_type ct1 ct2 -> t1 (* return left side *)

  | Bcast ct1, Bcast ct2 -> begin
      match ct1.ct_ty, ct2.ct_ty with
	Tpointer { ct_size = l }, Tpointer { ct_size = r } -> begin
	  match l, r with
	    Some l, Some r ->
	      if Big_int.ge_big_int l r then t1 else t2
	  | Some _, None -> t1
	  | None, Some _ -> t2
	  | None, None -> t1
	end
      | _ -> Bunknown
  end
  | Bscalar, Bscalar -> Bscalar

  | _ -> Bunknown

let update ~env v t = 
  dprintf 6 "  initial %d: %a" v pp_ilr_base_usetype_info t;
  Earray.set env.usetypes v (merge t (Earray.get env.usetypes v))

let update_by_vartype ~env v =
  match env.f.variable_environment.(v).variable_type with
    ILRtypeBase(t, _)
  | ILRtypeBaseTemp(t, _) -> update ~env v (Bused t)
  | ILRtypeOfs(t, _)
  | ILRtypeVal(t, _) -> update ~env v Bscalar

let rec phase1_lv ~env lv = 
  match lv with
    ILSlvPtr(b, _)
  | ILSlvPtrToFunc(b, _) -> update_by_vartype ~env b
  | ILSlvVar _
  | ILSlvSVar _
  | ILSlvTemp _ -> ()

let phase1_assign1 ~env v e = 
  match e with
  | ILSexpCoerce1(ct, t1) -> update_predecessor ~env v t1; update ~env v (Bcast ct)
  | ILSexpCoerce2(ct, t1, _t2) -> update_predecessor ~env v t1; update ~env v (Bcast ct)
  | ILSexpConstant _ -> update ~env v Bscalar
  | ILSexpUndefined
  | ILSexpBinop _
  | ILSexpBinop21 _
  | ILSexpUnaryop _
  | ILSexpInvoke _ -> ()
  | ILSexpAddress(lv, _) -> begin
      phase1_lv ~env lv; 
      match lv with
	ILSlvPtr(b, _o) -> update_predecessor ~env v b
      | _ -> ()
  end
  | ILSexpArgument _
  | ILSexpArgumentB _
  | ILSexpArgumentV _ -> ()
  | ILSexpIdent(t1) -> update_predecessor ~env v t1

let phase1_assign2 ~env v1 v2 e = phase1_assign1 ~env v1 e

let rec phase1_stmt ~env s = 
  match locval s with
    ILSstmtIf _ -> ()
  | ILSstmtSwitch _ -> ()
  | ILSstmtGoto _ -> ()
  | ILSstmtReturn0 -> ()
  | ILSstmtAbort _ -> ()
  | ILSstmtReturn1 _ -> ()
  | ILSstmtReturn2(v, _) -> update ~env v Brettype
  | ILSstmtAssign(v, e) -> phase1_assign1 ~env v e
  | ILSstmtAssign2(v1, v2, e) -> phase1_assign2 ~env v1 v2 e
  | ILSstmtRead1(_, lv, _) -> phase1_lv ~env lv
  | ILSstmtRead2(_, _, lv, _) -> phase1_lv ~env lv
  | ILSstmtWrite1(lv, _, _) -> phase1_lv ~env lv
  | ILSstmtWrite2(lv, _, _, _) -> phase1_lv ~env lv
  | ILSstmtDeclScalar _ -> ()
  | ILSstmtDeclBulk _ -> ()
  | ILSstmtInitialize _ -> ()
  | ILSstmtSequence l
  | ILSstmtParallel l  -> List.iter (phase1_stmt ~env) l
  | ILSstmtAbortIf _ -> ()

let phase1_block ~env b = 
  (* process phi predecessors *)
  List.iter
    (fun (v, preds) ->
      Array.iter (update_predecessor ~env v) preds)
    b.phi_function;
  List.iter (phase1_stmt ~env) b.code

let phase1 f = 
  let predecessors = Array.create (Array.length f.variable_environment) VarSet.empty in
  let env = 
    { f = f;
      predecessors = predecessors;
      usetypes = Earray.empty_with_default ~zero:Bunanalyzed }
  in
  Array.iteri
    (fun i b ->
      dprintf 6 "phase 1: block %d" i;
      phase1_block ~env b) f.body;
  env

let phase2 ~env = 
  let rec iter ~env n = 
    dprintf 5 "phase 2: iter %d" n;
    let processed = Array.create (Array.length env.predecessors) false in
    let updated = ref false in
    let rec do_var ~env v = 
      VarSet.iter
	(fun p ->
	  if processed.(p) then () else begin
	    processed.(p) <- true;
	    let previous_ut = Earray.get env.usetypes p in
	    let vt = Earray.get env.usetypes v in
	    let new_ut = merge previous_ut vt in
	    (* previous_ut is passed to left-side of merge: ==-comparison is OK *)
	    if previous_ut == new_ut then () else begin
	      updated := true;
	      dprintf 6 "  updated %d: new %a <- old %a, succ(%d):%a" 
		p pp_ilr_base_usetype_info new_ut pp_ilr_base_usetype_info previous_ut
		v pp_ilr_base_usetype_info vt;
	      Earray.set env.usetypes p new_ut;
	      do_var ~env p
	    end
	  end)
	env.predecessors.(v)
    in
    for i = 0 to Array.length env.predecessors - 1 do do_var ~env i done;
    if !updated then iter ~env (n + 1) else ()
  in
  iter ~env 0

let do_function ~id (f, utinfo) = 
  dprintf 5 "polymorphism analysis for %s" id;
  let ut = 
    let env = phase1 f in
    phase2 ~env;
    env.usetypes
  in
  dprintf 5 "polymorphism analysis for %s done." id;
  f, ut 

let f ~genv = 
  Locterm.locmap_list
    (function
	ILRdeclVariable (gsc,t,id,init) -> ILRdeclVariable(gsc,t,id,init)
      | ILRdeclFunction (gsc,t,id,arg_id,func) ->
	  ILRdeclFunction (gsc,t,id,arg_id,do_function ~id func))
