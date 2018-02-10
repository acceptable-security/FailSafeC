(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2002-2004 and 2009. *)

open Util
open Il
open Il3

include Debug.Install (struct let category = 165 end)

type phi_assignment_chains = 
    (int * int) list list

type inserted_phi_assignments = 
    (int * int) list

type 'a block_info = 
    {
     mutable trampoline_blocks : (int * 'a) list;
     mutable phi_assignments_at_bottom : (int * 'a) option;
   }

type block_phi_info = phi_assignment_chains block_info
type phi_info = block_phi_info array

type ('a, 'b) environment = {
     min_additional_variables : int;
     mutable max_additonal_variables : int;
     mutable additional_variables : Ctt_abstree.c_type Il.il_variable_attribute_base Glist.t;
     f : ('a, 'b) il3_function
   }

type jump_target = 
    Direct of int
  | Trampoline of int * int

let phase1 f = 
  dprintf 5 "phase 1.";
  let n_blocks = Array.length f.body in
  let env = {
    min_additional_variables = f.max_variable_number;
    max_additonal_variables = f.max_variable_number;
    additional_variables = Glist.empty ();
    f = f
  } in
  let info = Array.init n_blocks
      (fun _ -> 
	{ trampoline_blocks = [];
	  phi_assignments_at_bottom = None;
	};
      ) in
  for i = 0 to n_blocks - 1 do
    let b = f.body.(i) in
    let bi = info.(i) in
    if b.phi_function = [] then
      dprintf 6 "block %d: no phi." i
    else begin
      let n_pred = List.length b.predecessor in
      dprintf 6 "block %d: n_pred = %d" i n_pred;
      for j = 0 to n_pred - 1 do
	let pred = List.nth b.predecessor j in
	let phi_assignments = 
	  list_map 
	    (fun (target, source_list) -> target, source_list.(j))
	    b.phi_function
	in
	if pred <> i - 1 && List.length (f.body.(pred).successor) >= 2 then begin
	  (* branch edge is critical: insert trampoline block *)
	  dprintf 6 "    %d -> %d needs trampoline block. (pred.succ #%d)"
	    pred i (List.length (f.body.(pred).successor));
	  bi.trampoline_blocks <- (pred, phi_assignments) :: bi.trampoline_blocks
	end 
	else begin
	  assert (info.(pred).phi_assignments_at_bottom = None);
	  dprintf 6 "    %d -> %d do not need trampoline block. put on %d's bottom. (pred.succ #%d)"
	    pred i pred (List.length (f.body.(pred).successor));
	  info.(pred).phi_assignments_at_bottom <- Some (i, phi_assignments)
	end
      done
    end
  done;
  env, info

let allocate_new_temp_variables ~env t = 
  let id = env.max_additonal_variables in
  env.max_additonal_variables <- env.max_additonal_variables + 1;
  Glist.put env.additional_variables t;
  id

let make_phi_assigns ~env l =
  dprintf 7 "make_phi_assigns: [%s]" (Util.string_of_list (fun (t, f) -> Printf.sprintf "%d<--%d" t f) ", " l);
  (* detect loop *)
  if l = [] then [] else 
  let s = List.length l in
  let loop_top = ref Set_list.empty in
  let walked = ref Set_list.empty in
  let bottom = ref Set_list.empty in
  let rec iter root cur = 
    if Set_list.mem cur !walked then ()
    else begin
      walked := Set_list.add !walked cur;
      let b = ref false in
      List.iter 
	(function
	    (t, f) when f = cur ->
	      b := true;
	      if t = root then begin (* loop found *)
		loop_top := Set_list.add !loop_top root;
		bottom := Set_list.add !bottom root
	      end else
		iter root t
	  | _ -> ())
	l;
      if not !b then bottom := Set_list.add !bottom cur;
    end
  in
  List.iter (function (t, f) -> iter f f) l;

  let loop_top = Set_list.to_list !loop_top in
  let bottom = Set_list.to_list !bottom in
  
  dprintf 7 "  loop tops: %s" (Util.string_of_list string_of_int ", " loop_top);
  dprintf 7 "  dependency bottoms: %s" (Util.string_of_list string_of_int ", " bottom);

  let temp_map = 
    list_map
      (fun v ->
	let tmp = allocate_new_temp_variables ~env 
	    (Earray.get env.f.variable_environment v) in
	(v, tmp)) loop_top
  in
  let assignment_chains = 
    let dones = ref Set_list.empty in
    let result = ref [] in
    List.iter
      (fun bottom ->
	let g = Glist.empty () in
	let rec iter cur = 
	  if List.mem_assoc cur l then
	    let from = List.assoc cur l in
	    let from = 
	      if List.mem_assoc from temp_map then List.assoc from temp_map
	      else from
	    in
	    Glist.put g (cur, from);
	    dones := Set_list.add !dones cur;
	    iter from
	in
	iter bottom;
	result := (Glist.to_list g) :: !result)
      bottom;
    [map_flatten (fun (x, y) -> [y, x]) temp_map; List.flatten (!result)]
  in
  dprintf 7 "  result_chain: [%s]" 
    (Util.string_of_list
       (fun l -> (Util.string_of_list (fun (t, f) -> Printf.sprintf "%d<--%d" t f) ", " l))
       " | " assignment_chains);
  assignment_chains

let phase2 ~env ~info : phi_info = 
  dprintf 5 "phase 2.";
  let f = env.f in
  let n_blocks = Array.length f.body in
  Array.init n_blocks (fun i ->
    dprintf 6 "block %d." i;
    begin
      match info.(i).phi_assignments_at_bottom with
	Some (t, _::_) ->
	  dprintf 6 "  computing phi %d -> %d to be placed at the bottom of %d" i t i;
      | Some (t, []) ->
	  dprintf 6 "  no phi variables %d -> %d to be placed at the bottom of %d" i t i;
      | None ->
	  dprintf 6 "  no bottom phi assignment at %d" i;
    end;
    let phi_bottom = Option.map
	(fun (t, phi) ->
	  t, make_phi_assigns ~env phi) 
	info.(i).phi_assignments_at_bottom
    in
    let make_trampoline from assigns = 
      dprintf 6 "  computing phi trampoline %d -> %d to be placed at the top of %d" from i i;
      make_phi_assigns ~env assigns
    in
    let trampolines = 
      List.map
	(fun (from, assigns) -> from, make_trampoline from assigns)
	info.(i).trampoline_blocks
    in
    { phi_assignments_at_bottom = phi_bottom;
      trampoline_blocks = trampolines })

let extend_variables ~env venv = 
  let add_vars = Glist.to_list env.additional_variables in
  assert (List.length add_vars + env.min_additional_variables = env.max_additonal_variables);
  let new_env = Earray.copy venv in
  list_iteri
    (fun i v ->
      Earray.set new_env (i + env.min_additional_variables) v;
      Earray.set env.f.more_info (i + env.min_additional_variables) (Il3_optimize.GenTempVarM);
    )
    add_vars;
  new_env

let translate_function
    ({ body = body;
       max_variable_number = max_variable_number;
       variable_environment = variable_environment;
     } as f) = 
  let env, info = phase1 f in
  let phi_info = phase2 ~env ~info in
  { f with
    max_variable_number = env.max_additonal_variables;
    variable_environment = extend_variables ~env variable_environment;
    more_info = f.more_info, phi_info
  }

let f prog = 
  Locterm.locmap_list
    (function
	IL3declVariable (gsc,t,id,init) -> 
	  IL3declVariable(gsc,t,id,init)
      | IL3declFunction (gsc,t,id,arg_id,func) ->
	  dprintf 1 "il3_decompose_ssa: %s" id;
	  IL3declFunction (gsc,t,id,arg_id,translate_function func))
    prog
