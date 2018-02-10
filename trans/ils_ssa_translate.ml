(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007. *)

(**
   Renewed implementation of single static assignment translation on ILS.

   The whole algorithm is roughly based on Das-Ramakrishna's algorithm.

   The lazy phi-insertion algorithm is (re?-)invented by me, 
   and is roughly corresponds to or possibly inspired from Matsuno-Ohori's
   type-system-based representation.
 *)

(**
   input assumption: 
   -  all branch instructions (conditional and unconditional) are
     only at the bottom of each basic block.
   -  phi functions lists are empty.

   output obligation:
   -  all temporary variables are assigned only once.
   -  all "merges" of variables are represented as phi functions
      attached to the top of each blocks.
 *)

open Util
open Locterm
open Ctt_abstree
open Il
open Ils
open Ils_formatter

include Debug.Install (struct let category = 70 end)

(**************** step1: build DJ graph and merge sets ****************)

let dump_flow f = 
  Array.iteri
    (fun i b ->
      dprintf 6 "b%d => %s" i (string_of_list string_of_int "," b.successor))
    f.body

(* step1-0: put postorder for blocks *)
let step1_0 f = 
  let nblocks = Array.length f.body in
  let visited = Array.create nblocks false in
  let porder = Array.create nblocks (-1) in
  let porder_to_block = Array.create nblocks (-1) in
  let cnt = ref 0 in
  let rec iter b = 
    dprintf 7 "%d[" b;
    if visited.(b) then () else begin
      visited.(b) <- true;
      List.iter iter f.body.(b).successor;
      porder.(b) <- !cnt;
      porder_to_block.(!cnt) <- b;
      incr cnt
    end;
    dprintf 7 "]";
  in
  iter 0;
  porder, porder_to_block

(* step1-1: compute dominator tree: separating D-edges and J-edges *)
let step1_1 f pordermap revpordermap = 
  let nblocks = Array.length f.body in
  let doms = Array.create nblocks (-1) in
  let changed = ref true in
  doms.(0) <- 0;

  let rec intersect b1 b2 = 
    if b1 == b2 then b1
    else if pordermap.(b1) < pordermap.(b2) 
    then intersect doms.(b1) b2
    else intersect b1 doms.(b2)
  in

  while !changed do
    changed := false;
    for po = nblocks - 1 downto 0 do
      let b = revpordermap.(po) in
      if b != 0 then begin
	let new_idom = 
	  let rec find_processed_predecessors = function
	      [] -> assert false
	    | h::t -> if doms.(h) <> -1 then h else find_processed_predecessors t
	  in
	  find_processed_predecessors f.body.(b).predecessor
	in
	let rec iter new_idom = function
	    [] -> new_idom
	  | h::t ->
	      if doms.(h) = -1 then iter new_idom t else
	      iter (intersect h new_idom) t
	in
	let new_idom = iter new_idom f.body.(b).predecessor in
	if new_idom <> doms.(b) then begin
	  changed := true;
	  doms.(b) <- new_idom
	end
      end
    done
  done;

  doms

(* step1-2: create tables for the dominator-tree BFS order and depth *)
let step1_2 doms = 
  let nblocks = Array.length doms in
  let depth = Array.create nblocks (-1) in
  depth.(0) <- 0;
  let rec iter b = 
    if depth.(b) <> -1 then ()
    else let p = doms.(b) in
    iter p;
    depth.(b) <- depth.(p) + 1
  in
  for i = 0 to nblocks - 1 do iter i done;

  let bfsorder = 
    List.sort 
      (fun a b -> match compare depth.(a) depth.(b) with 0 -> compare a b | x -> x)
      (ExtList.List.init nblocks (fun i -> i)) in
  depth, bfsorder

module NodeSet = struct
  include Set.Make(struct type t = int let compare = compare end)
  let to_list m = 
    let g = Glist.empty () in
    iter (fun e -> Glist.put g e) m;
    Glist.to_list g
end
module VarSet = NodeSet

(* step1-3: compute merge set *)
let step1_3 f idoms depth bfsorder = 
  dprintf 6 "step 1-3: computing merge set";
  let merge = Array.create (Array.length f.body) NodeSet.empty in
  let tdmsc () = 
    let requireanotherpass = ref false in
    let visited = Hashtbl.create 16 in
    List.iter 
      (fun tnode ->
	dprintf 6 "looking at node %d" tnode;
	List.iter 
	  (fun snode -> 
	    if idoms.(tnode) = snode (* the edge is D-edge *) then 
	      dprintf 6 "looking at edge %d->%d: is D-edge" snode tnode
	    else if Hashtbl.mem visited (snode, tnode) then
	      dprintf 6 "looking at edge %d->%d: is visited" snode tnode
	    else begin
	      dprintf 6 "looking at edge %d->%d d(%d): is unvisited J-edge" snode tnode depth.(tnode);
	      Hashtbl.replace visited (snode, tnode) ();
	      let rec iter lnode tmp =
		dprintf 7 "  lnode = %d, tmp = %d (d%d)" lnode tmp depth.(tmp);
		if depth.(tmp) >= depth.(tnode) then begin
		  merge.(tmp) <-
		    NodeSet.union merge.(tmp)
		      (NodeSet.add tnode merge.(tnode));
		  dprintf 7 "  updating merge set at %d" tmp;
		  if tmp = 0 
		  then lnode 
		      (* added for preventing infinite loop (parent not defined) *)
		  else
		    iter tmp idoms.(tmp)
		end else lnode
	      in
	      let lnode = iter (-1) snode in
	      dprintf 6 "  checking consistency at lnode %d" lnode;
	      List.iter 
		(fun snode' -> 
		  if idoms.(lnode) = snode' then
		    dprintf 6 "    looking at edge %d->%d: is D-edge" snode' lnode
		  else if not (Hashtbl.mem visited (snode', lnode)) then
		    dprintf 6 "    looking at edge %d->%d: is not visited" snode' lnode
		  else begin
		    dprintf 6 "    looking at edge %d->%d: is visited J-edge" snode' lnode;
                    if not (NodeSet.subset merge.(lnode) merge.(snode'))
		    then begin
		      dprintf 6 "    ... edge %d->%d is inconsistent." snode' lnode;
		      requireanotherpass := true
		    end
		    else
		      ()
		  end
		) f.body.(lnode).predecessor
	    end)
	  f.body.(tnode).predecessor
      ) bfsorder;
    !requireanotherpass
  in
  while tdmsc () do dprintf 6 "go next round." done;
  merge

let step1 f = 
  let pordermap, revpordermap = step1_0 f in
  let idoms = step1_1 f pordermap revpordermap in
  let depth, bfsorder = step1_2 idoms in
  let merge = step1_3 f idoms depth bfsorder in
  bfsorder, merge, idoms

(**************** step2: insert phi functions ****************)

(* step2-1: list up possibly required phi functions *)
let step2_1 f merge = 
  let possible_phis = Array.create (Array.length merge) VarSet.empty in
  let rec walk_block s = function
      [] -> s
    | h::t -> begin
	let l = 
	match locval h with
	  ILSstmtIf _ -> VarSet.empty
	| ILSstmtSwitch _ -> VarSet.empty
	| ILSstmtGoto _ -> VarSet.empty
	| ILSstmtReturn0 -> VarSet.empty
	| ILSstmtAbort _ -> VarSet.empty
	| ILSstmtReturn1 _ -> VarSet.empty
	| ILSstmtReturn2 _ -> VarSet.empty
	| ILSstmtAssign (t1, _e) -> VarSet.singleton t1
	| ILSstmtAssign2 (t1, t2, _e) -> VarSet.add t1 (VarSet.singleton t2)
	| ILSstmtRead1 (t1, _, _) -> VarSet.singleton t1
	| ILSstmtRead2 (t1, t2, _, _) -> VarSet.add t1 (VarSet.singleton t2)
	| ILSstmtWrite1 _ -> VarSet.empty
	| ILSstmtWrite2 _ -> VarSet.empty
	| ILSstmtDeclScalar _ -> VarSet.empty
	| ILSstmtDeclBulk _ -> VarSet.empty
	| ILSstmtInitialize _ -> VarSet.empty
	| ILSstmtSequence l -> walk_block VarSet.empty l
	| ILSstmtParallel l -> walk_block VarSet.empty l
	| ILSstmtAbortIf _ -> VarSet.empty
	in
	walk_block (VarSet.union l s) t
    end
  in
  Array.iteri
    (fun b m ->
      if not (VarSet.is_empty m) then
	let s = walk_block VarSet.empty f.body.(b).code in
	VarSet.iter
	  (fun b' -> possible_phis.(b') <- VarSet.union possible_phis.(b') s)
	  m)
    merge;
  possible_phis

(* step2-2: rename all variables, lazily inserting phi functions *)

type allocated_phi_attribute = 
    { p_block: int;
      p_vid: int;
    }

type phi_attribute = 
    Punallocated of int (* block no of instanciation *)
  | Pallocated of allocated_phi_attribute

type val_type = 
  | VNormal of int
  | VPhi of phi_attribute ref

(* additional analysis information for optimization later *)
type analysis_results = 
    { nullbase_p : BitSet.t;
      referred_p : BitSet.t; 
      reduced_assign_map : (int, int) Hashtbl.t;
    }

module VarMap = Map.Make(struct type t = int 
  let compare (x:t) (y:t) = compare x y end)

let step2_2 f bfsorder merge possible_phis =
  let nblocks = Array.length merge in
  let visited = Array.create nblocks false in
  let new_instrs = Array.create nblocks [] in
  let phi_list = Array.create nblocks [] in
  let bottom_maps = Array.create nblocks VarMap.empty in
  let variable_map = Earray.empty () in

  let reduced_assign_map = Hashtbl.create 17 in
  let add_copy_map t1 t2 = 
    Hashtbl.add reduced_assign_map t1 t2 
  in

  let nullbase_p = BitSet.empty () in
  let referred_p = BitSet.empty () in

  let vcount = ref 0 in
  let new_v ov = 
    let v' = !vcount in incr vcount;
    Earray.set variable_map v' ov;
    v'
  in
  let new_phi b = ref (Punallocated b) in

  let equal_ils_type t1 t2 = 
    (* bug #lepidum-70: 
       Base parts of integer variables are copied without explicit cast.
       These copy should not be reduced. *)
    let v1 = f.variable_environment.(t1).variable_type in
    let v2 = f.variable_environment.(t2).variable_type in
    match v1, v2 with
    | ILStypeBase ({ ct_ty = Tbuiltin b } as t1), ILStypeBase ({ ct_ty = Tbuiltin b' } as t2) 
      -> 
	(* do not distinguish same-sized int *) 
	C_typing.equal_type  
	  (Il0_type_canonify.translate_c_type_mem t1) 
	  (Il0_type_canonify.translate_c_type_mem t2) 
    | ILStypeVal x, ILStypeVal y  
    | ILStypeOfs x, ILStypeOfs y 
    | ILStypeBase x, ILStypeBase y 
      -> 
	C_typing.equal_type x y 
    | _ -> false 
  in
   
  let create_block_top_map b parent_map = 
    let phis = ref [] in
    let m = VarMap.mapi
	(fun old_vid new_vid_ref ->
	  if VarSet.mem old_vid possible_phis.(b) then
	    let phi = new_phi b in
	    phis := (old_vid, phi) :: !phis;
	    ref (VPhi phi)
	  else
	    ref (!new_vid_ref))
	parent_map
    in
    !phis, m
  in

  let read_vid map ov = 
    try
      match !(VarMap.find ov map) with
      | VNormal i -> 
	  BitSet.set referred_p i;
	  i
      | VPhi r -> begin
	  match !r with
	    Punallocated b' ->
	      let nv = new_v ov in
	      dprintf 6 "   instantiating phi at block %d for var %d: new id is %d" b' ov nv;
	      r := Pallocated { p_block = b'; p_vid = nv };
	      BitSet.set referred_p nv;
	      nv
	  | Pallocated p ->
	      BitSet.set referred_p p.p_vid;
	      p.p_vid
      end
    with
      Not_found -> 
	dprintf 0 "assert failed: variable %d is not available" ov;
	assert false
  in

  let set_write_vid map ov nv = 
    if VarMap.mem ov map then begin
      (VarMap.find ov map) := nv;
      map
    end else
      VarMap.add ov (ref nv) map
  in

  let alloc_write_vid map ov = 
    let nv = new_v ov in
    dprintf 7 "allocated vid %d for old vid %d" nv ov;
    nv, set_write_vid map ov (VNormal nv)
  in

  (* phase 1: renaming variables *)

  let map_lv map = function
      ILSlvPtr(t1, t2) -> ILSlvPtr(read_vid map t1, read_vid map t2)
    | ILSlvPtrToFunc(t1, t2) -> ILSlvPtrToFunc(read_vid map t1, read_vid map t2)
    | ILSlvVar(v,i,t) -> ILSlvVar(v,i,t)
    | ILSlvSVar(v,i,t) -> ILSlvSVar(v,i,t)
    | ILSlvTemp t -> ILSlvTemp(read_vid map t)
  in

  let map_funcargs map l = 
    List.map
      (function
	  ILSFuncArgNarrow t1 -> ILSFuncArgNarrow (read_vid map t1)
	| ILSFuncArgWide(t1, t2) -> ILSFuncArgWide(read_vid map t1, read_vid map t2))
      l
  in

  let map_expr map = function
    | ILSexpCoerce1(typ, t1) -> ILSexpCoerce1(typ, read_vid map t1)
    | ILSexpCoerce2(typ, t1, t2) -> ILSexpCoerce2(typ, read_vid map t1, read_vid map t2)
    | ILSexpConstant(const) -> ILSexpConstant(const)
    | ILSexpUndefined -> ILSexpUndefined
    | ILSexpBinop(binop, t1, t2) -> ILSexpBinop(binop, read_vid map t1, read_vid map t2)
    | ILSexpBinop21(binop, (t11, t12), t2) ->
	ILSexpBinop21(binop, (read_vid map t11, read_vid map t12), read_vid map t2)
    | ILSexpUnaryop(uop, t1) -> ILSexpUnaryop(uop, read_vid map t1)
    | ILSexpInvoke(lv, ids) -> ILSexpInvoke(map_lv map lv, map_funcargs map ids)
    | ILSexpAddress(lv, fields) -> ILSexpAddress(map_lv map lv, fields)
    | ILSexpArgument(n) -> ILSexpArgument(n)
    | ILSexpArgumentV(n) -> ILSexpArgumentV(n)
    | ILSexpArgumentB(n) -> ILSexpArgumentB(n)
    | ILSexpIdent(t1) -> ILSexpIdent(read_vid map t1)
  in
  
  let rec map_init map init = init
  in
  
  let rec map_instr map i =
    let nmap = ref map in
    let i' = match locval i with
      ILSstmtIf(ift, t1, tb) -> ILSstmtIf(ift, read_vid map t1, tb)
    | ILSstmtSwitch(t1, stbl) -> ILSstmtSwitch(read_vid map t1, stbl)
    | ILSstmtGoto tb -> ILSstmtGoto tb
    | ILSstmtReturn0 -> ILSstmtReturn0
    | ILSstmtAbort err -> ILSstmtAbort err
    | ILSstmtReturn1 t1 -> ILSstmtReturn1 (read_vid map  t1)
    | ILSstmtReturn2(t1, t2) -> ILSstmtReturn2(read_vid map  t1, read_vid map t2)
    | ILSstmtAssign(t1, ILSexpIdent t2) when equal_ils_type t1 t2 ->
	(* special constant propargation *)
	dprintf 8 "reducing assignment %d = %d" t1 t2;
	add_copy_map t1 t2;
	nmap := set_write_vid map t1 !(VarMap.find t2 map);
	ILSstmtSequence []
    | ILSstmtAssign(t1, e) ->
	let e' = map_expr map e in
	let nv, map' = alloc_write_vid map t1 in
	nmap := map';
	begin
	  match e' with
	    ILSexpConstant CTTconstNull ->
	      BitSet.set nullbase_p nv
	  | ILSexpIdent t2 when BitSet.is_set nullbase_p t2 ->
	      BitSet.set nullbase_p nv
	  | _ -> ()
	end;
	ILSstmtAssign(nv, e')
    | ILSstmtAssign2(t1, t2, e) ->
	let e' = map_expr map e in
	let nv1, map' = alloc_write_vid map t1 in
	let nv2, map' = alloc_write_vid map' t2 in
	nmap := map';
	ILSstmtAssign2(nv1, nv2, e')
    | ILSstmtRead1(t1, lv, fl) ->
	let lv = map_lv map lv in
	let nv, map' = alloc_write_vid map t1 in
	nmap := map';
	ILSstmtRead1(nv, lv, fl)
    | ILSstmtRead2(t1, t2, lv, fl) ->
	let lv = map_lv map lv in
	let nv1, map' = alloc_write_vid map t1 in
	let nv2, map' = alloc_write_vid map' t2 in
	nmap := map';
	ILSstmtRead2(nv1, nv2, lv, fl)
    | ILSstmtWrite1(lv, fl, t1) ->
	ILSstmtWrite1(map_lv map lv, fl, read_vid map t1)
    | ILSstmtWrite2(lv, fl, t1, t2) ->
	ILSstmtWrite2(map_lv map lv, fl, read_vid map t1, read_vid map t2)
    | ILSstmtDeclScalar(vt, ct, id) -> ILSstmtDeclScalar(vt, ct, id)
    | ILSstmtDeclBulk(vt, ct, id) -> ILSstmtDeclBulk(vt, ct, id)
    | ILSstmtInitialize(vt, ct, id, init) ->
	ILSstmtInitialize(vt, ct, id, map_init map init)
    | ILSstmtSequence l ->
	let map', l = map_instrs map l in
	nmap := map';
	ILSstmtSequence l
    | ILSstmtParallel l ->
	let map', l = map_instrs map l in
	nmap := map';
	ILSstmtParallel l
    | ILSstmtAbortIf(e, er) ->
	ILSstmtAbortIf(map_expr map e, er)
    in
    !nmap, loccopy ~orig:i i'
      
  and map_instrs map l =
    let gi = Glist.empty () in
    let rec iter map = function
	[] -> map, Glist.to_list gi
      | h::t ->
	  let map, i = map_instr map h in
	  if locval i <> ILSstmtSequence [] && locval i <> ILSstmtParallel [] then Glist.put gi i;
	  iter map t
    in
    iter map l
  in
  
  let rec process_block b parent_map = 
    if visited.(b) then () else begin
      visited.(b) <- true;
      dprintf 6 "processing block %d" b;
      let phis, top_map = create_block_top_map b parent_map in
      phi_list.(b) <- phis;
      let bottom_map, instr = map_instrs top_map f.body.(b).code in
      new_instrs.(b) <- instr;
      bottom_maps.(b) <- bottom_map;
      List.iter 
	(fun s -> process_block s bottom_map)
	f.body.(b).successor
    end
  in

  process_block 0 VarMap.empty;

  (* phase 2: add only-transitively reffered phis *)

  dprintf 6 "transitively instantiating phi functions.";

  begin 
    let ovcount = ref (-1) in
    while !ovcount < !vcount do
      dprintf 6 ": %d ... %d" !ovcount !vcount;
      ovcount := !vcount;
      Array.iter
	(fun pl ->
	  List.iter
	    (fun (ov, p) ->
		  match !p with
		    Punallocated _ -> ()
		  | Pallocated { p_block = b; p_vid = v } ->
		      List.iter
			(fun pb ->
			  ignore (read_vid bottom_maps.(pb) ov))
			f.body.(b).predecessor)
			(* instantiated variables referred by live phi functions *)
	    pl)
	phi_list
    done;
  end;

  dprintf 6 "done.";

  (* phase 3: constructing phi functions *)

  let new_phis =
    Array.init
      nblocks
      (fun b ->
	map_flatten
	  (fun (ov, p) ->
	    match !p with
	      Punallocated _ -> []
	    | Pallocated { p_block = b'; p_vid = v } ->
		[v, List.map
		   (fun pb -> read_vid bottom_maps.(pb) ov)
		   f.body.(b).predecessor])
	  phi_list.(b))
  in
  new_instrs, new_phis, variable_map, 
  { reduced_assign_map = reduced_assign_map;
    nullbase_p = nullbase_p; referred_p = referred_p }

let step2 f merge bfsorder = 
  let pos_phis = step2_1 f merge in
  step2_2 f bfsorder merge pos_phis

(**************** step3: optimizing redundant phi moves ****************)

(* at SSA translation in step1--2, we looked on dominance of control flow.
   Now, we look on dominance of data flow for phi functions.
   Thanks to the SSA property, if a phi variable is dominated by another 
   variable, the dominatee can be replaced by the dominator. 

   Already in step2, all redundant moves except phi-related ones are
   reduced to its simplest form.
*)

(* step3-0: collect all concerned variables. (phis and those referred to by phis *)
let step3_0 phis =
  dprintf 6 "  collecting phis and predecessors.";
  let predecessors = Hashtbl.create 16 in
  Array.iter
    (fun block_phis ->
      List.iter
	(fun (phi, srcs) ->
	  Hashtbl.replace predecessors phi srcs;
	  List.iter
	    (fun src ->
	      if not (Hashtbl.mem predecessors src) then
		Hashtbl.replace predecessors src [])
	    srcs
	)
	block_phis)
    phis;
  predecessors

(* step3-1: create successor table *)
let step3_1 predecessors =
  dprintf 6 "  calculating successors.";
  let successors = Hashtbl.create 16 in
  Hashtbl.iter
    (fun node preds ->
      if not (Hashtbl.mem successors node) then
	Hashtbl.replace successors node Set_list.empty;
      List.iter
	(fun pred ->
	  if not (Hashtbl.mem successors pred) then
	    Hashtbl.replace successors pred Set_list.empty;
	  Hashtbl.replace successors pred (Set_list.add (Hashtbl.find successors pred) node))
	preds) predecessors;
  successors

(* step3-2: put postorder for nodes *)
let step3_2 predecessors successors = 
  dprintf 6 "  putting postorder.";
  (* here, we imagine virtual "start" node, on which all
     value-generating variables (non-phi nodes) depend to. *)
  let nnodes = Hashtbl.length predecessors in
  let visited = Hashtbl.create 16 in
  let porder_of_node = Hashtbl.create 16 in
  let porder_to_node = Array.create nnodes (-1) in
  let cnt = ref 0 in
  let rec iter n = 
    dprintf 7 "%d[" n;
    if Hashtbl.mem visited n then () else begin
      Hashtbl.replace visited n ();
      Set_list.iter iter (Hashtbl.find successors n);
      Hashtbl.replace porder_of_node n !cnt;
      porder_to_node.(!cnt) <- n;
      incr cnt
    end;
    dprintf 7 "]";
  in
  Hashtbl.iter
    (fun n pred ->
      if pred = [] then iter n)
    predecessors;
  porder_of_node, porder_to_node

(* step3-3: compute dominator tree *)
let step3_3 predecessors porder_of_node porder_to_node analysis_results =
  dprintf 6 "  constructing domination tree.";
  let nblocks = Array.length porder_to_node in
  let doms = Hashtbl.create 16 in
  let changed = ref true in
  (* assumption here: 
     the start node, which dominates all non-NULL assignment, is numbered -9.
     the "null" node, which dominates all NULL constants, is numbered -8.
     dominator of start node is itself. 
     the postorder of the start node is very last. *)

  let porder_of_node n = 
    if n == (-9) then nblocks+1 else 
    if n == (-8) then nblocks else 
    Hashtbl.find porder_of_node n
  in

  let get_dom n = 
    if n == (-9) || n == (-8) then -9 else
    Hashtbl.find doms n
  in

  let rec intersect n1 n2 = 
    if n1 == n2 then n1
    else if porder_of_node n1 < porder_of_node n2
    then intersect (get_dom n1) n2
    else intersect n1 (get_dom n2)
  in

  while !changed do
    changed := false;
    for po = nblocks - 1 downto 0 do
      let b = porder_to_node.(po) in
      let new_idom = 
	if Hashtbl.find predecessors b == [] then 
	  if BitSet.is_set analysis_results.nullbase_p b then -8 else -9
	else
	let rec find_processed_predecessors = function
	    [] -> assert false
	  | h::t -> if Hashtbl.mem doms h then h else find_processed_predecessors t
	in
	find_processed_predecessors (Hashtbl.find predecessors b)
      in
      let rec iter new_idom = function
	  [] -> new_idom
	| h::t ->
	    if not (Hashtbl.mem doms h) then iter new_idom t else
	    iter (intersect h new_idom) t
      in
      let new_idom = iter new_idom (Hashtbl.find predecessors b) in
      if not (Hashtbl.mem doms b) || new_idom <> Hashtbl.find doms b then begin
	changed := true;
	Hashtbl.replace doms b new_idom
      end
    done
  done;

  doms

(* step3-4: create grand dominator table *)
let step3_4 doms =
  dprintf 6 "  calculating grand dominators.";
  let gdoms = Hashtbl.create 16 in
  Hashtbl.iter
    (fun n _dom ->
      let rec iter b = 
	if Hashtbl.mem gdoms b then (dprintf 6 "(%dx)" b; ())
	else
	  let dom = Hashtbl.find doms b in
	  if dom = (-9) then begin
	    dprintf 6 "(%dnp)" b;
	    Hashtbl.replace gdoms b b
	  end else if dom = (-8) then begin
	    dprintf 6 "(%dnull)" b;
	    Hashtbl.replace gdoms b (-8)
	  end else begin
	    dprintf 6 "(%dr->%d)" b dom;
	    iter dom;
	    Hashtbl.replace gdoms b (Hashtbl.find gdoms dom)
	  end
      in
      iter n)
    doms;
  gdoms

let step3_5 gdoms instrs phis = 
  dprintf 6 "  optimizing code.";
  let map ov = 
    try
      let t = Hashtbl.find gdoms ov in
      if t >= 0 then t else ov
    with
      Not_found -> ov
  in

  let map_lv = function
      ILSlvPtr(t1, t2) -> ILSlvPtr(map t1, map t2)
    | ILSlvPtrToFunc(t1, t2) -> ILSlvPtrToFunc(map t1, map t2)
    | ILSlvVar(v,i,t) -> ILSlvVar(v,i,t)
    | ILSlvSVar(v,i,t) -> ILSlvSVar(v,i,t)
    | ILSlvTemp t -> ILSlvTemp (map t)
  in

  let map_funcargs l = 
    List.map
      (function
	  ILSFuncArgNarrow t1 -> ILSFuncArgNarrow (map t1)
	| ILSFuncArgWide(t1, t2) -> ILSFuncArgWide(map t1, map t2))
      l
  in

  let map_expr map = function
    | ILSexpCoerce1(typ, t1) -> ILSexpCoerce1(typ, map t1)
    | ILSexpCoerce2(typ, t1, t2) -> ILSexpCoerce2(typ, map t1, map t2)
    | ILSexpConstant(const) -> ILSexpConstant(const)
    | ILSexpUndefined -> ILSexpUndefined
    | ILSexpBinop(binop, t1, t2) -> ILSexpBinop(binop, map t1, map t2)
    | ILSexpBinop21(binop, (t11, t12), t2) ->
	ILSexpBinop21(binop, (map t11, map t12), map t2)
    | ILSexpUnaryop(uop, t1) -> ILSexpUnaryop(uop, map t1)
    | ILSexpInvoke(lv, ids) -> ILSexpInvoke(map_lv lv, map_funcargs ids)
    | ILSexpAddress(lv, fields) -> ILSexpAddress(map_lv lv, fields)
    | ILSexpArgument(n) -> ILSexpArgument(n)
    | ILSexpArgumentV(n) -> ILSexpArgumentV(n)
    | ILSexpArgumentB(n) -> ILSexpArgumentB(n)
    | ILSexpIdent(t1) -> ILSexpIdent(map t1)
  in
  
  let rec map_init init = init
  in
  
  let rec map_instr i = 
    let new_i = match locval i with
      ILSstmtIf(ift, t1, tb) -> ILSstmtIf(ift, map t1, tb)
    | ILSstmtSwitch(t1, stbl) -> ILSstmtSwitch(map t1, stbl)
    | ILSstmtGoto tb -> ILSstmtGoto tb
    | ILSstmtReturn0 -> ILSstmtReturn0
    | ILSstmtAbort err -> ILSstmtAbort err
    | ILSstmtReturn1 t1 -> ILSstmtReturn1 (map t1)
    | ILSstmtReturn2(t1, t2) -> ILSstmtReturn2(map t1, map t2)
    | ILSstmtAssign(t1, e) ->
	assert (map t1 = t1);
	let e' = map_expr map e in
	ILSstmtAssign(t1, e')
    | ILSstmtAssign2(t1, t2, e) ->
	assert (map t1 = t1);
	assert (map t2 = t2);
	let e' = map_expr map e in
	ILSstmtAssign2(t1, t2, e')
    | ILSstmtRead1(t1, lv, fl) ->
	assert (map t1 = t1);
	let lv = map_lv lv in
	ILSstmtRead1(t1, lv, fl)
    | ILSstmtRead2(t1, t2, lv, fl) ->
	assert (map t1 = t1);
	assert (map t2 = t2);
	let lv = map_lv lv in
	ILSstmtRead2(t1, t2, lv, fl)
    | ILSstmtWrite1(lv, fl, t1) ->
	ILSstmtWrite1(map_lv lv, fl, map t1)
    | ILSstmtWrite2(lv, fl, t1, t2) ->
	ILSstmtWrite2(map_lv lv, fl, map t1, map t2)
    | ILSstmtDeclScalar(vt, ct, id) -> ILSstmtDeclScalar(vt, ct, id)
    | ILSstmtDeclBulk(vt, ct, id) -> ILSstmtDeclBulk(vt, ct, id)
    | ILSstmtInitialize(vt, ct, id, init) ->
	ILSstmtInitialize(vt, ct, id, map_init init)
    | ILSstmtSequence l ->
	let l = map_instrs l in
	ILSstmtSequence l
    | ILSstmtParallel l ->
	let l = map_instrs l in
	ILSstmtParallel l
    | ILSstmtAbortIf(e, er) ->
	ILSstmtAbortIf(map_expr map e, er)
    in
    loccopy ~orig:i new_i
  and map_instrs l = list_map map_instr l
  in

  let convert_phis phis = 
    let g = Glist.empty () in
    let np = 
      map_flatten
	(fun (v, srcs) ->
	  let gd = Hashtbl.find gdoms v in
	  if gd = v then
	    (* non-dominated phi *)
	    [v, list_map map srcs]
	  else if gd == -8 then 
	    begin
	      dprintf 6 "removing NULL-constant phi %d" v;
	      Glist.put g 
		(locput_dummy (ILSstmtAssign(v, ILSexpConstant CTTconstNull)));
	      []
	    end
	  else 
	    begin
	      dprintf 6 "removing redundunt phi %d" v;
	      assert (gd >= 0);
	      []
	    end)
	phis
    in
    Glist.to_list g, np
  in

  let convert_block b = 
    let insn = map_instrs instrs.(b) in
    let add_insn, p = convert_phis phis.(b) in
    (add_insn @ insn), p
  in

  let new_instrs, new_phis = Array.copy instrs, Array.copy phis in

  for i = 0 to Array.length new_instrs - 1 do
    let insn, p = convert_block i in
    new_instrs.(i) <- insn;
    new_phis.(i) <- p
  done;
  
  new_instrs, new_phis

let step3 instrs phis vmap analysis_results = 
  dprintf 6 "optimizing redundunt phi movements.";
  let predecessors = step3_0 phis in
  let successors = step3_1 predecessors in
  let porder_of_node, porder_to_node = step3_2 predecessors successors in
  let doms = step3_3 predecessors porder_of_node porder_to_node analysis_results in
  let gdoms = step3_4 doms in
  let new_instrs, new_phis = step3_5 gdoms instrs phis in
  dprintf 6 "  done.";
  new_instrs, new_phis, vmap

(**** main functions ****)

let translate_function f = 
  let bfsorder, merge, idoms = step1 f in
  let new_instrs, new_phis, variable_map, analysis_results = step2 f merge bfsorder in
  let new_instrs, new_phis, variable_map = step3 new_instrs new_phis variable_map analysis_results in

  let basic_blocks = 
    Array.mapi
      (fun b bd ->
	{ bd with
	  phi_function = List.map (function v, r -> v, Array.of_list r) new_phis.(b);
	  immediate_dominator = idoms.(b);
	  code = new_instrs.(b) })
      f.body
  in
  let varenv = 
    Array.init
      (Earray.length variable_map)
      (fun i -> 
	let from = (Earray.get variable_map i) in
	let rec iter = function
	    [] -> 
		dprintf 9 "            ssa_varmap: %d: from %d, no more candidate, use %d" i from from;
	      from, f.variable_environment.(from) (* default *)
	  | f2::tl ->
	      let v = f.variable_environment.(f2) in
	      if v.original_name = None then begin
		dprintf 9 "            ssa_varmap: %d: from %d, checking %d, unnamed" i from f2;
		iter tl
	      end else begin
		dprintf 9 "            ssa_varmap: %d: from %d, checking %d, named" i from f2;
		f2, v
	      end
	in
	let f2, v = iter (from :: Hashtbl.find_all analysis_results.reduced_assign_map from) in
	dprintf 8 "ssa_varmap: %d: from %d (%d), name %s" i from f2
	  (Option.default "-" v.original_name);
	{ f.variable_environment.(from) with original_name = v.original_name })
  in
  { f with
    max_variable_number = Earray.length variable_map;
    variable_environment = varenv;
    body = basic_blocks }

let translate_program p = 
  dprintf_start "making SSA representation...";
  let r = locmap_list
      (function
	  ILdeclFunction(gs,ct,id,ids,f) -> 
	    dprintf_progress "%s" id;
	    ILdeclFunction(gs,ct,id,ids,translate_function f)
	| ILdeclVariable(gs,ct,id,initopt) -> ILdeclVariable(gs,ct,id,initopt))
      p
  in
  dprintf_end "done.";
  r

(* test functions *)

let dump_intarray m = 
  Array.iteri (fun i v -> dprintf 6 "%d => %d" i v) m
  
let test_func f = 
  for b = 0 to Array.length f.body - 1 do
    dprintf 6 "\nblock %3d:" b;
    List.iter (fun i -> dprintf 6 "%a" pp_ils i) f.body.(b).code
  done;

  dprintf 6 "flow:";
  dump_flow f;

  let pordermap, revpordermap = step1_0 f in
  (*dprintf 6 "pordermap:";
  dump_intarray pordermap;*)

  let idoms = step1_1 f pordermap revpordermap in
  let depth, bfsorder = step1_2 idoms in
  dprintf 6 "idoms:";
  for b = 0 to Array.length f.body - 1 do
    dprintf 6 "%3d: idom = %3d, depth = %3d" b idoms.(b) depth.(b)
  done;
  dprintf 6 "BFS order: %s" (string_of_list string_of_int ", " bfsorder);

  dprintf 6 "computing mergeset";
  let merge = step1_3 f idoms depth bfsorder in

  for b = 0 to Array.length f.body - 1 do
    dprintf 6 "%3d: %s" b (string_of_list string_of_int ", " (NodeSet.to_list merge.(b)));
  done;

  dprintf 6 "computing possibly required phis";
  let pos_phis = step2_1 f merge in

  for b = 0 to Array.length f.body - 1 do
    dprintf 6 "%3d: %s" b (string_of_list string_of_int ", " (VarSet.to_list pos_phis.(b)));
  done;

  dprintf 6 "renaming variables";
  let new_instrs, new_phis, vmap, analysis_results = step2_2 f bfsorder merge pos_phis in

  for b = 0 to Array.length f.body - 1 do
    dprintf 6 "\nblock %3d:" b;
    List.iter
      (fun (v, l) ->
	dprintf 6 "PHI: %d = { %s }" v 
	  (string_of_list string_of_int ", " l)) new_phis.(b);
    List.iter (fun i -> dprintf 6 "%a" pp_ils i) new_instrs.(b)
  done;

  dprintf 6 "variable map:";
  Earray.iteri (fun i v -> dprintf 6 "  %d <= %d" i v) vmap;

  dprintf 6 "step3: optimizing redundunt phi movements.";

  let predecessors = step3_0 new_phis in
  let successors = step3_1 predecessors in
  let porder_of_node, porder_to_node = step3_2 predecessors successors in
  let doms = step3_3 predecessors porder_of_node porder_to_node analysis_results in

  dprintf 6 "variable idoms:";
  Hashtbl.iter
    (fun n d ->
      dprintf 6 "%3d: idom = %3d" n d) doms;

  let gdoms = step3_4 doms in

  dprintf 6 "variable gdoms:";
  Hashtbl.iter
    (fun n d ->
      dprintf 6 "%3d: gdom = %3d" n d) gdoms;

  let new_instrs, new_phis = step3_5 gdoms new_instrs new_phis in

  for b = 0 to Array.length f.body - 1 do
    dprintf 6 "\nblock %3d:" b;
    List.iter
      (fun (v, l) ->
	dprintf 6 "PHI: %d = { %s }" v 
	  (string_of_list string_of_int ", " l)) new_phis.(b);
    List.iter (fun i -> dprintf 6 "%a" pp_ils i) new_instrs.(b)
  done;

  ()

let test p = 
  List.iter
    (fun d -> match locval d with
	ILdeclFunction(_,_,id,_,f) ->
	  dprintf 6 "--- testing ssa for function %s" id; test_func f
      | _ -> ())
    p

