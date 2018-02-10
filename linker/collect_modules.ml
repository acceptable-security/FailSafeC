(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

open Linker_types
open Parse_typeinfo

include Debug.Install (struct let category = 220 end)

let system_path = Parse_typeinfo.system_path

type environment = 
    {
     requested : (string, string) Hashtbl.t;
     bss_provided : (string, string) Hashtbl.t;
     provided : (string, string) Hashtbl.t;
     modules : (string * Parse_typeinfo.module_info) Glist.t
   }

let new_environment () = 
  let e = 
    { requested = Hashtbl.create 16;
      bss_provided = Hashtbl.create 16;
      provided = Hashtbl.create 16;
      modules = Glist.empty () }
  in
  Hashtbl.add e.requested "main" "<>";
  e

let value_provided ~env f id = 
  if Hashtbl.mem env.provided id then
    failwith 
      (Printf.sprintf 
	 "duplicated declaration: %s in %s, previously declared in %s"
	 id f (Hashtbl.find env.provided id));
  dprintf 5 "      Defined Symbol %s provided by %s" id f;
  Hashtbl.add env.provided id f;
  Hashtbl.remove env.requested id;
  Hashtbl.remove env.bss_provided id

let value_bss_provided ~env f id = 
  if Hashtbl.mem env.provided id then
    dprintf 5 "      BSS    symbol %s is already provided" id
  else if Hashtbl.mem env.bss_provided id then
    dprintf 5 "      BSS    symbol %s is already provided as BSS" id
  else begin
    dprintf 5 "      BSS    Symbol %s provided by %s" id f;
    Hashtbl.add env.bss_provided id f;
    Hashtbl.remove env.requested id
  end

let value_required ~env f id = 
  if Hashtbl.mem env.provided id then begin
    dprintf 5 "      Extern symbol %s is already provided" id
  end else if Hashtbl.mem env.bss_provided id then begin
    dprintf 5 "      Extern symbol %s is already provided as BSS by %s" id f
  end else begin
    dprintf 5 "      Extern symbol %s requested by %s" id f;
    Hashtbl.replace env.requested id f
  end

let add_single_module ~env f mi = 
  dprintf 4 "Adding module %s" f;
  List.iter 
    (function id, _ty, _attr -> value_provided ~env f id)
    mi.mi_provided_values;
  List.iter 
    (function id, _ty -> value_bss_provided ~env f id)
    mi.mi_bss_values;
  List.iter 
    (function id, _ty -> value_required ~env f id)
    mi.mi_required_values;
  Glist.put env.modules (f, mi)

let analyze_archive ~env f mis = 
  let h = Hashtbl.create 16 in
  List.iter
    (function mname, mi ->
      if mi.mi_attributes.forced_linking then
	add_single_module ~env ("[" ^ f ^ "]" ^ mname) mi
      else
	List.iter
	  (function name, _typ, _attr ->
	    if Hashtbl.mem h name then
	      failwith 
		(Printf.sprintf
		   "symbol %s found twice in archive %s: in %s and %s"
		   name f mname (fst (Hashtbl.find h name)));
	    Hashtbl.add h name (mname, mi))
	  mi.mi_provided_values)
    mis;
  h

let add_archive ~env f mis = 
  dprintf 4 "Adding archive %s" f;
  let provided_functions =
    analyze_archive ~env f mis in
  let updated = ref true in
  let c = ref 0 in
  while !updated do
    incr c;
    dprintf 4 "  [round %d]" !c;
    updated := false;
    let current_req_symbols = 
      Hashtbl.fold
	(fun nam _reqmod l -> nam :: l)
	env.requested []
    in
    List.iter
      (function id ->
	if Hashtbl.mem env.provided id then 
	  () (* the providing module is already added by a reference to another symbol *)
	else begin
	  match Hashtbl.find_all provided_functions id with
	    [] -> ()
	  | [mname, mi] ->
	      dprintf_progress "<%s>" mname;
	      dprintf 5 "    Symbol %s found in %s" id mname;
	      updated := true;
	      add_single_module ~env ("[" ^ f ^ "]" ^ mname) mi
	  | _ :: _ :: _ ->
	      assert false
	end)
      current_req_symbols
  done;
  dprintf 4 "Done on archive %s" f

type input_file_content = 
    Singleton of Parse_typeinfo.module_info
  | Archive of (string * Parse_typeinfo.module_info) list

let read_encoded_stream st =
  let r = Glist.empty () in
  begin
    try
      while true do
	let l =
	  try input_line st with End_of_file -> raise Exit
	in
	let tag, len = Scanf.sscanf l "%[a-zA-z---]:%d%!" (fun t l -> t, l) in
	let contents = 
	  let buf = String.create len in
	  really_input st buf 0 len;
	  buf
	in
	let c = input_char st in
	assert (c = '\n');
	Glist.put r (tag, contents);
      done;
      ()
    with
      Exit -> ()
  end;
  Glist.to_list r

let parse_encoded_stream ?(check_name = false) f fp =
  let r = read_encoded_stream fp in
  match r with
    [ "file-name", fn; "section", sn; "contents", c ] ->
      assert (fn = f);
      assert (sn = "__failsafeC_typeinfo");
      Singleton (Parse_typeinfo.read_string ~file:fn c)
  | ("archive-name", an) :: l ->
      if check_name then assert (an = f);
      let rec iter = function
	  [] -> []
	| ("file-name", fn) :: ("section", sn) :: ("contents", c) :: l ->
	    assert (sn = "__failsafeC_typeinfo");
	    (fn, Parse_typeinfo.read_string ~file:("<" ^ an ^ ">" ^ fn) c) :: iter l
	| _ -> failwith "cannot parse result of read_elf_section (2)"
      in
      Archive (iter l)
  | _ ->
      failwith "cannot parse result of read_elf_section"

let read_input_file f = 
  let command = system_path () ^ "/tools/read-elf-section" in
  let commandline = command ^ " -S __failsafeC_typeinfo -M " ^
    Filename.quote f in
  let in_proc = Unix.open_process_in commandline in
  let r = parse_encoded_stream f in_proc in
  if Unix.close_process_in in_proc <> Unix.WEXITED 0 then 
    failwith "Reading elf section failed:";
  r

let read_manifest f m = 
  let ifp = open_in m in
  let r = parse_encoded_stream ~check_name:false f ifp in
  close_in ifp;
  r

let modname_of_stdlib = "<fsc_stdlib.a>"

let f files = 
  let start_time = Unix.gettimeofday () in
  let env = new_environment () in
  dprintf_start "reading and collecting modules...";
  List.iter
    (fun f ->
      dprintf_progress "[%s]" f;
      match read_input_file f with
	Singleton mi ->
	  add_single_module ~env f mi
      | Archive mis ->
	  add_archive ~env f mis) files;

  dprintf_progress "[FSC stdlib]";
  begin
    match
      read_manifest modname_of_stdlib (Parse_typeinfo.system_path () ^ "/runtime/stdlib/stdlib.manifest") with
      Archive mis -> add_archive ~env modname_of_stdlib mis
    | _ -> assert false;
  end;

(*  add_single_module ~env
    modname_of_stdlib
    (Parse_typeinfo.parse_unit_manifest
       (Parse_typeinfo.system_path () ^ "/runtime/stdlib/stdlib.manifest"));*)
  dprintf_end "done. (%.3f s)" (Unix.gettimeofday () -. start_time);
  if (Hashtbl.length env.requested <> 0) then begin
    prerr_endline "linker error: following symbols are not provided:";
    Hashtbl.iter
      (fun id mname -> Printf.eprintf "  %s (requested by %s)\n%!" id mname)
      env.requested;
    failwith "cannot resolve dependencies";
  end;
  Glist.to_list env.modules
