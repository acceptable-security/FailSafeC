(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

(* a support function which add a code piece on the function top. *)

open Util
open Locterm
open Il2

let scan_for_top_goto f =
  let have_top_goto = ref false in
  let rec scan_inst i = 
    match (locval i).il2_t with
      IL2stmtDeclAutoScalar _
    | IL2stmtDeclBulk _
    | IL2stmtRead _
    | IL2stmtAssign _
    | IL2stmtWrite _ -> ()

    | IL2stmtIf(_, _, gt)
    | IL2stmtGoto gt -> if gt = 0 then have_top_goto := true
	  
    | IL2stmtSwitch(_, l) ->
	List.iter (fun (_, gt) -> if gt = 0 then have_top_goto := true) l
	  
    | IL2stmtReturn _ -> ()
    | IL2stmtAbort _ -> ()
	  
    | IL2stmtSequence l
    | IL2stmtParallel l -> List.iter scan_inst l
	  
  in
  let scan_block b = 
    List.iter scan_inst b.code
  in
  Array.iter scan_block f;
  !have_top_goto

let shift_jump_target f = 
  let rec translate_inst i = 
    let tnew = match (locval i).il2_t with
    | IL2stmtIf(ift, t, gt) -> IL2stmtIf (ift, t, succ gt)
    | IL2stmtSwitch(t, l) -> IL2stmtSwitch (t, list_map (function l, gt -> l, succ gt) l)
    | IL2stmtGoto gt -> IL2stmtGoto (succ gt)

    | IL2stmtReturn _
    | IL2stmtAbort _
    | IL2stmtAssign _
    | IL2stmtDeclAutoScalar _
    | IL2stmtDeclBulk _
    | IL2stmtRead _
    | IL2stmtWrite _ as t -> t
    | IL2stmtSequence l -> IL2stmtSequence (list_map translate_inst l)
    | IL2stmtParallel l -> IL2stmtParallel (list_map translate_inst l)
    in
    loccopy ~orig:i { il2_t = tnew }
  in
  let translate_block b = 
    { location = b.location;
      code = list_map translate_inst b.code;
      predecessor = list_map succ b.predecessor;
      successor = list_map succ b.successor
    }
  in
  Array.map translate_block f

let add_code ~function_loc l ({ body = fb } as f) =
  if l = [] then f else
  if scan_for_top_goto fb then
    let top_block = { location = function_loc; predecessor = []; successor = [1]; code = l } in
    let fb = shift_jump_target fb in
    let b = Array.of_list (top_block :: Array.to_list fb) in
    b.(1) <- { b.(1) with predecessor = 0 :: b.(1).predecessor };
    { f with body = b }
  else
    let fb = Array.copy fb in
    fb.(0) <- { fb.(0) with code = l @ fb.(0).code };
    { f with body = fb }
