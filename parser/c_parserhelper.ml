(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2001 -- 2006. *)

(* typedef name *)

let name_table = ref []
let scope_stack = ref []

let push s v =
  s := v :: !s

let pop s = 
  match !s with
    v::tl -> s := tl; v
  | _ -> failwith "pop: empty stack"

let is_typedef_name (s : string) = 
  try
    if List.assoc s !name_table then
      true
    else
      false
  with
    Not_found ->
      false

let enter_identifier_scope () = 
(*  prerr_endline "enter"; *)
  push scope_stack !name_table
    
let leave_identifier_scope () = 
(*  prerr_endline "leave"; *)
  name_table := pop scope_stack

let rec list_between from too =
  if from == too then []
  else match from with
    [] -> failwith "list_between: target not found"
  | hd::tl -> hd::list_between tl too

let leave_and_save_identifier_scope v = 
(*  prerr_endline "leave (SAVE)"; *)
  let prev_name_table = pop scope_stack in
  v := Some (list_between !name_table prev_name_table);
  name_table := prev_name_table

let enter_merge_identifier_scope p =
(*   prerr_endline "enter (MERGE)"; *)
  match !p with
    None -> failwith "enter_merge_identifier_scope: no scope bound"
  | Some l -> 
      p := None;
      push scope_stack !name_table;
      name_table := l @ !name_table

let clear_typedef_name () = 
  scope_stack := [];
  name_table := []

let add_name s f = 
(*  prerr_endline ("new name : " ^ s ^ ":" ^ string_of_bool f); *)
  push name_table (s, f)
    
(* line number information *)

include Debug.Install (struct let category = 14 end)

let lno_table = ref [0, ("", 0, false)]

let lineno = ref 1
let filename = ref ""
let file_is_trustful = ref false
let filecnt = ref 0

let reset fname = 
  dprintf 8 "locinfo: resetting (0 <- %S:1) %S" fname fname;
  lno_table := [0, (fname, 1, false)];
  name_table := [];
  scope_stack := [];
  lineno := 1;
  filename := "";
  file_is_trustful := false

let register_linestart pos ~lineno = 
  dprintf 8 "locinfo: recording (%d <- %S:%d)" pos !filename lineno;
  lno_table := (pos, (!filename, lineno, !file_is_trustful)) :: !lno_table

let lookup_location p = 
  let rec iter = function
      [] -> assert false
    | (pos, (fname, line, is_trustful)) :: tl ->
	if p >= pos then Locterm.Located(fname, line, p - pos, is_trustful)
	else iter tl
  in
  let res = iter !lno_table in
  dprintf 8 "locinfo: getting   (%d -> %s)" p (Locterm.strof_location res);
  res
