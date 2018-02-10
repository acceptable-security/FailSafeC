(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2003-2009. *)

open Format
open Util
open Locterm
open Il
open Il3
open Il3_formatter

include Debug.Install (struct let category = 160 end)

type car_cdr = CAR of pair_types | CDR of pair_types

let incr_earray e i = 
  Earray.set e i (1 + Earray.get e i)

let optimize1 ~fname f = 
  dprintf 6 "optimize 1 for %s" fname;
  (* map cons of car and cdr *)
  let env = Earray.empty () in
  let rec scan i = 
    match (locval i).il3_desc with
      IL3stmtCallReducibleFun(id, IL3pr_car t, [s]) ->
	dprintf 8 "%d = car %d" id s;
	Earray.set env id (CAR t, s)
    | IL3stmtCallReducibleFun(id, IL3pr_cdr t, [s]) ->
	dprintf 8 "%d = cdr %d" id s;
	Earray.set env id (CDR t, s)
    | IL3stmtConditional _ ->
	  () (* do not search inside conditionals *)
    | _ -> Il3_visitor.scan_instr ~self:scan ~v_src:ignore ~v_dest:ignore i
  in
  Il3_visitor.scan_function 
    ~v_block:(Il3_visitor.scan_block ~v_instr:scan ~v_phi:ignore)
    ~v_prologue:ignore
    f;

  let rec replace i =
    let loc = locget i in
    match (locval i).il3_desc with
      IL3stmtCallReducibleFun(id, IL3pr_cons t, [s1; s2]) -> begin
	dprintf 8 "searching match for %d = cons (%d, %d)" id s1 s2;
	if Earray.mem env s1 && Earray.mem env s2 then
	  match Earray.get env s1, Earray.get env s2 with
	    (CAR t1, s'1), (CDR t2, s'2) when
	      t1 = t && t2 = t && s'1 = s'2
	    ->
	      dprintf 6 "found pair (%d, %d) replaced by %d for %d" s1 s2 s'1 id;
	      make_il3 ~loc
		(IL3stmtMove(id,
			     (Earray.get f.variable_environment id).variable_type,
			     s'1))
	  | _ -> i
	else
	  i
      end
    | _ ->
	Il3_visitor.visit_instr ~self:replace ~v_src:identity ~v_dest:identity i
  in
  Il3_visitor.visit_function
    ~v_block:(Il3_visitor.visit_block ~v_instr:replace ~v_phi:identity)
    ~v_prologue:identity
    f

let optimize02 n ~fname f = 
  (* move propargation *)
  dprintf 6 "optimize %d for %s" n fname;
  let env = Earray.empty () in
  let bl = Earray.empty_with_default ~zero:false in
  let rec scan i = 
    match (locval i).il3_desc with
      IL3stmtMove(d, _, s) ->
	dprintf 8 "found assignment %d = %d" d s;
	Earray.set env d s
    | IL3stmtConditional(ds, _, _, _) ->
	List.iter 
	  (fun d -> 
	    dprintf 8 "target %d blacklisted" d;
	    Earray.set bl d true) 
	  ds;
	Il3_visitor.scan_instr ~self:scan ~v_src:ignore ~v_dest:ignore i
    | _ -> Il3_visitor.scan_instr ~self:scan ~v_src:ignore ~v_dest:ignore i
  in
  Il3_visitor.scan_function 
    ~v_block:(Il3_visitor.scan_block ~v_instr:scan ~v_phi:ignore)
    ~v_prologue:ignore
    f;

  let srcmap i = 
    if Earray.get bl i then begin
      dprintf 7 "found use of %d: blacklisted" i;
      i
    end else if Earray.mem env i then
      let n = Earray.get env i in
      dprintf 7 "found use of %d: replacing with %d" i n;
      n
    else
      i
  in
  let rec replace i =
    Il3_visitor.visit_instr ~self:replace ~v_src:srcmap ~v_dest:identity i
  in
  let replace_phi (d, srcs) =
    d, Array.map srcmap srcs in

  Il3_visitor.visit_function
    ~v_block:(Il3_visitor.visit_block ~v_instr:replace ~v_phi:replace_phi)
    ~v_prologue:identity
    f

type cogen_decision = 
    Omit | Inline of temp_id il3 | Constant of temp_id il3 | 
    GenTempVar1 | GenTempVarM
      (* GenVoid disappered (caused a bug): 
	 GenVoid is now represented as "Omit with irreducible statement".
       *)

type cogen_decisions = cogen_decision earray

let strof_cogen_decisions = function
    Omit -> "omit"
  | Inline i -> "inline " ^ sfprintf "%a" (Il3_formatter.pp_il3 Il3_formatter.pp_temp_id) i
  | Constant i -> "inline " ^ sfprintf "%a" (Il3_formatter.pp_il3 Il3_formatter.pp_temp_id) i
  | GenTempVar1 -> "temp_var_once"
  | GenTempVarM -> "temp_var_multi"
let short_strof_bool = function
    true -> "T" | false -> "-"

let analyze_for_optimize3 ~fname f = 
  (* remove unused declarations *)
  let defined = Earray.empty_with_default ~zero:false in
  let r_definition = Earray.empty () in
  let is_constant = Earray.empty_with_default ~zero:false in
  let is_phi_source = Earray.empty_with_default ~zero:false in
  let uses_from_IR = Earray.empty_with_default ~zero:0 in
  let target_of_multiwrites = Earray.empty_with_default ~zero:false in
  let target_of_IRresult = Earray.empty_with_default ~zero:false in
  let val_sources = Earray.empty_with_default ~zero:[] in
  
  let scan_dependency () = 
    let add_use_from_IR i = 
      dprintf 8 "val %d used from irreducible instructions" i;
      incr_earray uses_from_IR i
    in
    let add_depends i deps =
      dprintf 8 "val %d depends on %d" i deps;
      Earray.set val_sources i (deps :: Earray.get val_sources i)
    in
    let add_phi_depends i deps =
      dprintf 8 "phi val %d depends on %d" i deps;
      Earray.set val_sources i (deps :: Earray.get val_sources i);
      Earray.set is_phi_source deps true;
    in
    let add_multiwrite_target i =
      dprintf 8 "val %d is multiply defined" i;
      Earray.set target_of_multiwrites i true;
      Earray.set defined i true;
      Earray.unset r_definition i (* incomplete; for paranoia *)
    in
    let add_Rresult i instr = 
      Earray.set defined i true;
      Earray.set r_definition i instr;
    in
    let add_constant i = 
      Earray.set is_constant i true
    in
    let add_IRresult i =
      dprintf 8 "val %d is a result of irreducible" i;
      Earray.set defined i true;
      Earray.set target_of_IRresult i true
    in
    let rec scan_instr i =
      match (locval i).il3_desc with
	(* reducibles *)
      | IL3stmtMove(d, t, s) -> add_Rresult d i; add_depends d s
      | IL3stmtRexp(d, t, e) -> begin
	  add_Rresult d i;
	  Il3_visitor.scan_rexp ~v_src:(add_depends d) e;
	  match e with
	    IL3RexArgument _ 
	  | IL3RexAddress(ILlvVar _, _)
	  | IL3RexUndefined
	  | IL3RexConstant _ -> add_constant d
	  | _ -> ()
      end;
      | IL3stmtCallReducibleFun(d, t, e)
	-> add_Rresult d i; List.iter (add_depends d) e
      | IL3stmtCallReducibleFunOverwriting (d, t, e)
	-> add_multiwrite_target d;
	  List.iter (add_depends d) e
	    
        (* irreducibles *)
      | IL3stmtCallImmobileFun(d, _, srcs) ->
	  add_IRresult d;
	  List.iter add_use_from_IR srcs
      | IL3stmtCallImmobileOp(_, srcs) ->
	  List.iter add_use_from_IR srcs
	    
      | IL3stmtCallReaderHelper(d, sz, s1, s2) ->
	  add_IRresult d;
	  add_use_from_IR s1; add_use_from_IR s2
      | IL3stmtCallWriterHelper(sz, t1, t2, t3, t4o) ->
	  add_use_from_IR t1; add_use_from_IR t2;
	  add_use_from_IR t3; Option.iter add_use_from_IR t4o
      | IL3stmtCallAbort(t1, t2) ->
	  add_use_from_IR t1; add_use_from_IR t2
      | IL3stmtIexp(d, t, e) ->
	  add_IRresult d;
	  Il3_visitor.scan_iexp ~v_src:add_use_from_IR e
      | IL3stmtReadRaw(d, ty, lv, flds) -> 
	  add_IRresult d;
	  Il3_visitor.scan_lv ~v_src:add_use_from_IR lv
      | IL3stmtWriteRaw(lv, fld, tid) ->
	  Il3_visitor.scan_lv ~v_src:add_use_from_IR lv;
	  add_use_from_IR tid
	    
        (* controls *)
      | IL3stmtIf(ift, t1, bt) -> add_use_from_IR t1
      | IL3stmtGoto bt -> ()
      | IL3stmtSwitch(t1, bts) -> add_use_from_IR t1
      | IL3stmtReturn(t) -> Option.iter add_use_from_IR t
	    
      | IL3stmtSequence l
      | IL3stmtParallel l
	-> List.iter scan_instr l
      | IL3stmtConditional (vs, i, i1, i2) ->
	  List.iter add_multiwrite_target vs;
	  add_use_from_IR i;
	  scan_instr i1; scan_instr i2
    in
    let scan_phi (i, srcs) = 
      add_multiwrite_target i;
      Array.iter (add_phi_depends i) srcs
    in
    Il3_visitor.scan_function 
      ~v_block:(Il3_visitor.scan_block ~v_instr:scan_instr ~v_phi:scan_phi)
      ~v_prologue:ignore
      f
  in
  scan_dependency ();
  let compute_usages () = 
    let dependency_walked = Earray.empty_with_default ~zero:false in
    let usages = Earray.empty_with_default ~zero:0 in

    let rec walk i = 
      incr_earray usages i;
      if Earray.get dependency_walked i then () else begin
	Earray.set dependency_walked i true;
	List.iter walk (List.rev (Earray.get val_sources i))
      end
    in
    Earray.iteri
      (fun i n ->
	if n <> 0 then
	  for x = 1 to n do
	    walk i
	  done)
      uses_from_IR;
    usages
  in
  let usages = compute_usages () in
  let decisions = Earray.empty_with_default ~zero:Omit in

  dprintf 7 "      use# IRu#  MW? IRW? Psrc?  decision";
  Earray.iteri
    (fun i n ->
      if n then begin
	let u, uIR, tM, tIR, srcP = 
	  Earray.get usages i, Earray.get uses_from_IR i,
	  Earray.get target_of_multiwrites i, Earray.get target_of_IRresult i,
	  Earray.get is_phi_source i
	in
	assert (uIR <= u);
	let decision = 
	  match u with
	    0 ->
	      Omit
	  | 1 ->
	      if tM or tIR or srcP then GenTempVar1
	      else
		if Earray.get is_constant i then 
		  Constant (Earray.get r_definition i)
		else Inline (Earray.get r_definition i)
	  | _ -> 
	      if Earray.get is_constant i then 
		if tM or tIR or srcP then GenTempVarM
		else Constant (Earray.get r_definition i)
	      else
		GenTempVarM
	in
	Earray.set decisions i decision;

	let cogen_decision = strof_cogen_decisions decision in
	dprintf 7 "%4d %4d %4d    %s    %s    %s    %s"
	  i u uIR (short_strof_bool tM) (short_strof_bool tIR) (short_strof_bool srcP) cogen_decision
      end)
    defined;
  dprintf 7 "";

  decisions

let optimize3a ~fname f = 
  dprintf 6 "optimize 3a for %s" fname;
  let decisions = analyze_for_optimize3 ~fname f in
  
  (* remove unused statements *)
  let rec translate_instr_list is = 
    map_flatten
      (fun i ->
	let loc = locget i in
	match (locval i).il3_desc with
	| IL3stmtCallReducibleFun(d, _, _)
	| IL3stmtCallReducibleFunOverwriting(d, _, _)
	| IL3stmtMove(d, _, _)
	| IL3stmtRexp(d, _, _)
	| IL3stmtReadRaw(d, _, _, _) ->
	  if Earray.get decisions d <> Omit then [i] else 
	  (dprintf 7 "omitting %d (%a)" d (pp_il3 pp_temp_id) i; [])
	      
	| IL3stmtCallImmobileFun _
	| IL3stmtCallReaderHelper _
	| IL3stmtIexp _
	| IL3stmtCallImmobileOp _
	| IL3stmtCallAbort _
	| IL3stmtCallWriterHelper _
	| IL3stmtWriteRaw _
	| IL3stmtIf _
	| IL3stmtGoto _
	| IL3stmtSwitch _
	| IL3stmtReturn _
	  -> [i]

	| IL3stmtSequence l -> begin
	    let l = translate_instr_list l in
	    match l with
	      [] | [_] -> l
	    | _::_ ->
		[ make_il3 ~loc (IL3stmtSequence l) ]
	end
	| IL3stmtParallel l -> begin
	    let l' = translate_instr_list l in
	    match l' with
	      [] | [_] -> l'
	    | _::_ ->
		[ make_il3 ~loc (IL3stmtParallel l') ]
	end
	| IL3stmtConditional(targets, cond, i1, i2) -> begin
	    let i1 = match translate_instr_list [i1] with
	      [i] -> i
	    | [] | _::_ as l -> make_il3 ~loc:(locget i1) (IL3stmtSequence l)
	    in
	    let i2 = match translate_instr_list [i2] with
	      [i] -> i
	    | [] | _::_ as l -> make_il3 ~loc:(locget i2) (IL3stmtSequence l)
	    in
	    let targets = List.filter
		(fun d -> Earray.get decisions d <> Omit) targets in
	    [make_il3 ~loc (IL3stmtConditional(targets, cond, i1, i2))]
	end)
      is
  in

  let translate_block b = 
    { b with code = translate_instr_list b.code;
      phi_function = 
        List.filter
	(fun (d, _) -> Earray.get decisions d <> Omit) b.phi_function
    }
  in

  let translated_function = 
    Il3_visitor.visit_function
      ~v_block:translate_block ~v_prologue:identity
      f
  in
  let decisions = Earray.map
      (function
	  Omit -> Omit
	| _ -> GenTempVarM) decisions in

  decisions, f

let adjust_4a ~fname f = 
  let rec translate_instr i = 
    Il3_visitor.visit_instr
      ~v_src:(fun i -> IL3BTemp i) ~v_dest:identity
      ~self:translate_instr
      i
  in
  let rec translate_block = 
    Il3_visitor.visit_block
      ~v_instr:translate_instr ~v_phi:identity
  in
  let translated_function = 
    Il3_visitor.visit_function
      ~v_block:translate_block ~v_prologue:identity
      f
  in
  translated_function

(** Phase 3B: use inlining *)
(* TODO: inline of phi arguments and READ primitive *)

let optimize3b ~fname f = 
  dprintf 6 "optimize 3b for %s" fname;
  let decisions = analyze_for_optimize3 ~fname f in
  let rec r_expressions ~loc i = 
    dprintf 6 "require r_expressions for %d" i;
    match Earray.get decisions i with
      Omit -> assert false
    | GenTempVarM | GenTempVar1 -> IL3BTemp i
    | Inline i -> begin
	let iloc = locget i in
	match (locval i).il3_desc with
	  IL3stmtMove(_, _, s) -> r_expressions ~loc s
	| IL3stmtRexp(_, t, e) ->
	    IL3BRexp (locput ~loc:iloc (t, Il3_visitor.visit_rexp ~v_src:(r_expressions ~loc:iloc) e))
	| IL3stmtCallReducibleFun(_, rf, args) ->
	    IL3BRfun (locput ~loc:iloc (rf, list_map (r_expressions ~loc:iloc) args))
	| _ -> assert false
    end
    | Constant i -> begin
	let loc = locget i in
	match (locval i).il3_desc with
	| IL3stmtRexp(_, t, e) ->
	    IL3BRexp (locput ~loc (t, Il3_visitor.visit_rexp ~v_src:(r_expressions ~loc) e))
	| _ -> assert false
    end
  in

  (* remove unused statements *)
  let rec translate_instr_list is = 
    map_flatten
      (fun i ->
	dprintf 9 "translating %a" (pp_il3 pp_temp_id) i;
	let loc = locget i in
	match (locval i).il3_desc with
	| IL3stmtCallReducibleFun(d, _, _)
	| IL3stmtCallReducibleFunOverwriting(d, _, _)
	| IL3stmtMove(d, _, _)
	| IL3stmtRexp(d, _, _)
	| IL3stmtReadRaw(d, _, _, _)
	    when 
	      (match Earray.get decisions d with 
	        Omit 
	      | Inline _ | Constant _ -> true
	      | _ -> false)
	  -> 	dprintf 9 "... generation omitted. (%a)" (pp_il3 pp_temp_id) i; []

	| IL3stmtCallReducibleFun _
	| IL3stmtCallReducibleFunOverwriting _
	| IL3stmtMove _
	| IL3stmtRexp _
	| IL3stmtReadRaw _
	| IL3stmtCallImmobileFun _
	| IL3stmtCallReaderHelper _
	| IL3stmtIexp _
	| IL3stmtCallImmobileOp _
	| IL3stmtCallAbort _
	| IL3stmtCallWriterHelper _
	| IL3stmtWriteRaw _
	| IL3stmtIf _
	| IL3stmtGoto _
	| IL3stmtSwitch _
	| IL3stmtReturn _
	  -> [ Il3_visitor.visit_instr ~self:(fun x -> assert false) ~v_src:(r_expressions ~loc) ~v_dest:identity i ]

	| IL3stmtSequence l -> begin
	    let l = translate_instr_list l in
	    match l with
	      [] | [_] -> l
	    | _::_ ->
		[ make_il3 ~loc (IL3stmtSequence l) ]
	end
	| IL3stmtParallel l -> begin
	    let l' = translate_instr_list l in
	    match l' with
	      [] | [_] -> l'
	    | _::_ ->
		[ make_il3 ~loc (IL3stmtParallel l') ]
	end
	| IL3stmtConditional(targets, cond, i1, i2) -> begin
	    let i1 = match translate_instr_list [i1] with
	      [i] -> i
	    | [] | _::_ as l -> make_il3 ~loc:(locget i1) (IL3stmtSequence l)
	    in
	    let i2 = match translate_instr_list [i2] with
	      [i] -> i
	    | [] | _::_ as l -> make_il3 ~loc:(locget i2) (IL3stmtSequence l)
	    in
	    let targets = List.filter
		(fun d -> Earray.get decisions d <> Omit) targets in
	    [ make_il3 ~loc (IL3stmtConditional(targets, r_expressions ~loc cond, i1, i2)) ]
	end)
      is
  in

  let translate_block b = 
    { b with code = translate_instr_list b.code;
      phi_function = 
        List.filter
	(fun (d, _) -> Earray.get decisions d <> Omit) b.phi_function
    }
  in

  let translated_function = 
    Il3_visitor.visit_function
      ~v_block:translate_block ~v_prologue:identity
      f

  in decisions, translated_function

let cleanup4 ~fname f = 
  dprintf 6 "clean-up 4 for %s" fname;
  let rec iter_sequencial_body l = 
    map_flatten
      (fun i -> match (locval i).il3_desc with
	IL3stmtSequence l ->
	  iter_sequencial_body l
      | IL3stmtParallel [i] -> [i]
      | IL3stmtParallel [] -> []
      (* | IL3stmtConditional *)
      | _ -> [i])
      l
  in
  let visit_block b =
    { b with code = iter_sequencial_body b.code }
  in
  Il3_visitor.visit_function
    ~v_block:visit_block
    ~v_prologue:identity
    f
	  
let optimize_function_a ~fname f = 
  let f = optimize02 0 ~fname f in
  let f = optimize1 ~fname f in
  let f = optimize02 2 ~fname f in
  let map, f = optimize3a ~fname f in
  let f = cleanup4 ~fname f in
  let f = adjust_4a ~fname f in
  { f with more_info = map }

let optimize_program_a p = 
  Il3_visitor.visit_gdecl_for_function ~v_func:optimize_function_a p

let optimize_function_b ~fname f = 
  let f = optimize02 0 ~fname f in
  let f = optimize1 ~fname f in
  let f = optimize02 2 ~fname f in
  let map, f = optimize3b ~fname f in
  let f = cleanup4 ~fname f in
  { f with more_info = map }

let optimize_program_b p = 
  Il3_visitor.visit_gdecl_for_function ~v_func:optimize_function_b p

