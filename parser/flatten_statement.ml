(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)

open Big_int
open Ctt_abstree
open Il1
open Il0
open Set_list
open Locterm

let dprintf l f = Debug.dprintf ~category:40 l f
let debug_level () = Debug.get_debug_flags ~category:40

let collect_jump_target body = 
  (* collect all labels which is target of some branches *)
  let set = ref empty in
  let add_to_set l = 
    dprintf 5 "jump to label %s found" l;
    set := add !set l in
  List.iter 
    (fun i ->
      (*Format.eprintf "%B: %s" i.il0_nobranch (Separate_side_effect.strof_il0 i);*)
      match (locval i).il0_t with
      | IL0stmtIf(iftype,tid,l) ->
	  add_to_set l
      | IL0stmtSwitch(id,targets) ->
	  List.iter (fun (v,l) -> add_to_set l) targets
      | IL0stmtGoto l -> add_to_set l
      | IL0stmtLabel _
      | IL0stmtDeclAutoScalar _ 
      | IL0stmtDeclBulk _
      | IL0stmtReturn _
      | IL0stmtAbort _
      | IL0stmtDefTemp _
      | IL0stmtReadToTemp _
      | IL0stmtWrite _
	-> ()
      | IL0stmtSequence _
      | IL0stmtParallel _ -> assert ((locval i).il0_nobranch); ())
    body;
  !set

let dummy_return_instruction = 
  locput_dummy (* dummy location here for end-of-function return *)
    { il0_depends = Int_set.empty; il0_defines = Int_set.empty; il0_nobranch = false; il0_t = IL0stmtReturn None }

type basic_blocks = { 
    has_fallflow : bool;
    successor_set : string list;
    mutable live_mark : bool;
    instrs : il0 list
  }

let separate_basic_block target_set body = 
  let block_id = ref 0 in
  let blocks = Glist.empty () in
  let current_instrs = Glist.empty () in
  let label_map = Glist.empty () in
  let add_block ~has_fallflow ~successors = 
    Glist.put blocks
      { has_fallflow = has_fallflow;
	successor_set = successors;
	live_mark = false;
	instrs = Glist.to_list current_instrs };
    incr block_id;
    Glist.reset current_instrs
  in
  let add_instr i = Glist.put current_instrs i in
  let rec iter ~has_op = 
    function
	[] ->
	  add_instr dummy_return_instruction;
	  add_block ~has_fallflow:false ~successors:[];
	  ()
      | hd::tl -> begin
	  dprintf 5 "%a" Il0.pp_il0 hd;
	  match (locval hd).il0_t with
	    IL0stmtLabel(l) when mem l target_set ->
	      (* break before current instruction *)
	      if has_op then
		add_block ~has_fallflow:true ~successors:[];
	      add_instr hd;
	      Glist.put label_map (l, !block_id);
	      iter ~has_op:false tl
	  | IL0stmtLabel(l) -> (* label with no entry: ignore silently *)
	      iter ~has_op tl
	  | IL0stmtGoto l ->
	      (* break after current instruction *)
	      add_instr hd;
	      add_block ~has_fallflow:false ~successors:[l];
	      iter ~has_op:false tl
	  | IL0stmtReturn _ | IL0stmtAbort _ ->
	      (* break after current instruction *)
	      add_instr hd;
	      add_block ~has_fallflow:false ~successors:[];
	      iter ~has_op:false tl
	  | IL0stmtIf(iftyp,exp,l) ->
	      (* break after current instruction *)
	      add_instr hd;
	      add_block ~has_fallflow:true ~successors:[l];
	      iter ~has_op:false tl
	  | IL0stmtSwitch(exp,targets) ->
	      add_instr hd;
	      add_block ~has_fallflow:false ~successors:(Util.list_map snd targets);
	      iter ~has_op:false tl
	  | IL0stmtDeclAutoScalar _ 
	  | IL0stmtDeclBulk _
	  | IL0stmtDefTemp _
	  | IL0stmtReadToTemp _
	  | IL0stmtWrite _
	  | IL0stmtSequence _
	  | IL0stmtParallel _ -> 
	      assert ((locval hd).il0_nobranch);
	      add_instr hd;
	      iter ~has_op:true tl
      end
  in
  iter ~has_op:false body;
  Glist.to_list label_map, Glist.to_list blocks

let pickup_declarations_as_dead is = 
  let l = Glist.empty () in
  let rec iter is =
    List.iter
      (fun i ->
	match (locval i).il0_t with
	| IL0stmtDeclAutoScalar (c, t, s, _) ->
	    dprintf 5 "pickup_declarations: picked %s, scalar" s;
	    Glist.put l (locmap (fun i -> { i with il0_t = IL0stmtDeclAutoScalar (c, t, s, None) }) i)
	| IL0stmtDeclBulk (c, t, s, _) ->
	    dprintf 5 "pickup_declarations: picked %s, bulk" s;
	    Glist.put l  (locmap (fun i -> { i with il0_t = IL0stmtDeclBulk (c, t, s, None) }) i)
	| IL0stmtSequence is
	| IL0stmtParallel is -> 
	    iter is
	| IL0stmtIf _
	| IL0stmtSwitch _
	| IL0stmtGoto _
	| IL0stmtLabel _
	| IL0stmtReturn _
	| IL0stmtAbort _
	| IL0stmtDefTemp _
	| IL0stmtReadToTemp _
	| IL0stmtWrite _
	  -> ()) is
  in
  iter is;
  Glist.to_list l

let remove_unused_blocks label_map blocks_list = 
  let blocks = Array.of_list blocks_list in
  let rec iter = function
      [] -> ()
    | blk::tl ->
	if blocks.(blk).live_mark then
	  iter tl
	else begin
	  blocks.(blk).live_mark <- true;
	  let stk =
	    ExtList.List.append 
	      (Util.list_map (fun l -> List.assoc l label_map) blocks.(blk).successor_set)
	      tl
	  in
	  if blocks.(blk).has_fallflow then
	    iter ((blk + 1)::stk)
	  else
	    iter stk
	end
  in
  iter [0];

  (* rescue declarations in dead blocks *)
  (* TODO: more complete fix *)
  let d = ref [] in
  for b = Array.length blocks - 1 downto 0 do
    dprintf 6 "pickup_dead_declarations: block %d: %B" b blocks.(b).live_mark;
    if blocks.(b).live_mark then
      if !d <> [] then begin
	dprintf 6 "put %d decls into block %d" (List.length !d) b;
	blocks.(b) <- { blocks.(b) with instrs = blocks.(b).instrs @ !d };
	d := [];
      end else ()
    else
      let defs = pickup_declarations_as_dead blocks.(b).instrs in
      d := defs @ !d
  done;
  assert (!d = []);
 
  ExtList.List.filter (fun b -> b.live_mark) (Array.to_list blocks)

let make_labelmap body = 
  let s = ref [] in
  Util.list_iteri
    (fun i b ->
      let rec iter = function
	  { locterm_v = {il0_t = IL0stmtLabel l }}::tl ->
	    dprintf 5 "label %s -> %d" l i;
	    s := (l, i) :: !s;
	    iter tl
	| _ -> ()
      in
      iter b.instrs)
    body;
  !s

let translate_block ~genv ~preds ~succs ~label_map bnum block = 
  let add_flow ~from ~too = 
    assert (from <> -1); assert (too <> -1);
    preds.(too) <- add preds.(too) from;
    succs.(from) <- add succs.(from) too
  in
  let rec translate_instrs instrs = 
    Util.map_flatten
      (fun i ->
	let loc, i = locget i, locval i in
	let make i' = 
	  [ locput ~loc { il1_depends = i.il0_defines; il1_defines = i.il0_defines; il1_t = i' } ] in
	match i.il0_t with
	  IL0stmtLabel _ -> []
	| IL0stmtDeclAutoScalar(ls,ct,id,tidopt) ->
	    make (IL1stmtDeclAutoScalar(ls,ct,id,tidopt))
	| IL0stmtDeclBulk(ls,ct,id,tidopt) ->
	    make (IL1stmtDeclBulk(ls,ct,id,tidopt))
	| IL0stmtIf(iftype,tid,l) ->
	    let target = List.assoc l label_map in
	    add_flow ~from:bnum ~too:target;
	    make (IL1stmtIf(iftype,tid,target))
	| IL0stmtSwitch(tid,targetlist) ->
	    let targetlist = 
	      Util.list_map
		(fun (v,l) ->
		  let target = List.assoc l label_map in
		  add_flow ~from:bnum ~too:target;
		  (v, target))
		targetlist in
	    make (IL1stmtSwitch(tid,targetlist))
	| IL0stmtGoto l ->
	    let target = List.assoc l label_map in
	    add_flow ~from:bnum ~too:target;
	    make (IL1stmtGoto target)
	| IL0stmtReturn tidopt ->
	    make (IL1stmtReturn tidopt)
	| IL0stmtAbort reason ->
	    make (IL1stmtAbort reason)
	| IL0stmtDefTemp(tid,ct,iexp) ->
	    make (IL1stmtDefTemp(tid,ct,iexp))
	| IL0stmtReadToTemp(tid,ct,lv,fld) ->
	    make (IL1stmtReadToTemp(tid,ct,lv,fld))
	| IL0stmtWrite(lv,fld,tid) ->
	    make (IL1stmtWrite(lv,fld,tid))
	| IL0stmtSequence l ->
	    let l = translate_instrs l in
	    make (IL1stmtSequence l)
	| IL0stmtParallel l ->
	    let l = translate_instrs l in
	    make (IL1stmtSequence l))
      instrs
  in
  if block.has_fallflow then add_flow ~from:bnum ~too:(bnum + 1);
  let body = translate_instrs block.instrs in
  let loc = locget (List.hd block.instrs) in
  loc, body

let translate_funcbody ~genv body = 
  let body = 
    match body.il0_funcbody with
      { locterm_v = { il0_t = IL0stmtSequence l }} -> l
    | l -> [l] 
  in
  let target_set = collect_jump_target body in
  if debug_level () >= 5 then List.iter (fun l -> dprintf 5 "%s" l) (to_list target_set);
  let label_map, blocks = separate_basic_block target_set body in
  let blocks = remove_unused_blocks label_map blocks in
  let label_map = make_labelmap blocks in
  let nblocks = List.length blocks in
  dprintf 5 "%d blocks." nblocks;
  let preds = Array.make nblocks empty in
  let succs = Array.make nblocks empty in
  let il1blocks = Util.list_mapi (translate_block ~genv ~preds ~succs ~label_map) blocks in
  let l = Util.list_mapi
      (fun i (loc, v) ->
	{ location = loc;
	  predecessor = to_list preds.(i);
	  successor = to_list succs.(i);
	  code = v }) il1blocks
  in
  Array.of_list l

let translate_program ~genv p = 
  locmap_list
    (function
	IL0declFunction(gs,ct,id,args,body) ->
	  dprintf 1 "translating IL0->Il1 on function %s..." id;
	  IL1declFunction(gs,ct,id,args,translate_funcbody ~genv body)
      | IL0declVariable(gs,ct,id,initopt) ->
	  dprintf 1 "translating IL0->Il1 on variable %s..." id;
	  IL1declVariable(gs,ct,id,initopt))
    p
