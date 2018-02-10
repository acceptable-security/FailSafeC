(* Part of Fail-Safe C Compiler.
   Produced by Yutaka Oiwa.
   (c) 2005- AIST. *)

open Util
open Glist
open Linker_types

let system_path_ref = ref ""

let system_path () =
  if !system_path_ref = "" then
    failwith "panic: system_path not specified"
  else
    !system_path_ref

let re_separator = Str.regexp "\\(:\\|\t\t*\\)"
let comma_separator = Str.regexp ", *"
let re_line_separator = Str.regexp "\n"

type struct_declaration_info = 
    Sabstract
  | Sspecial
  | Sconcrete of (string * ltype) list

type bss_attr = Req_bss | Req_extern

type value_attributes = 
    {
     attr_required_native_libraries : string list;
   }

type module_attributes = {
    is_stdlib : bool;
    abi_revision : int;
    allowed_minimum_abi_revision : int;
    need_initialize : string option;
    forced_linking : bool;
  }

type module_info = {
    mi_structs : (struct_desc * string * struct_declaration_info) list;
    mi_required_typeinfo : ltype list;
    mi_required_values : (string * ltype) list;
    mi_bss_values : (string * ltype) list;
    mi_provided_values : (string * ltype * value_attributes) list;
    mi_attributes : module_attributes
  }

let is_extension_critical str = 
  Char.lowercase str.[0] <> str.[0]

let process_ext_default ~file ~line v tag value = 
  if is_extension_critical tag then
    failwith_p 
      "unknown critical extension found: %S, in file %s, line %S" tag file line
  else v

let process_ext_extern ~file ~line v tag value = 
  match tag, value with
    ("bss_data" | "Bss_data"), None -> 
      Req_bss
  | _ -> process_ext_default ~file ~line v tag value

let process_ext_declare ~file ~line v tag value = 
  match tag, value with
    ("Require-native-libraries" | "require-native-libraries"), Some value -> 
      if v.attr_required_native_libraries <> [] then
	failwith_p
	  "bad extension found: %s, in file %s, line %S" tag file line
      else
	{ attr_required_native_libraries = Str.split comma_separator value }
  | _ -> process_ext_default ~file ~line v tag value

let process_ext_module ~file ~line v tag value = 
  match tag, value with
    "Stdlib-impl", None ->
      { v with is_stdlib = true }
  | "ABI", Some s ->
      Scanf.sscanf s "%u,%u%!"
	(fun a min -> { v with abi_revision = a; allowed_minimum_abi_revision = min })
  | "Initialize", Some s ->
      if v.need_initialize <> None then
	failwith_p "duplicate_initialize in file %s" file
      else
	{ v with need_initialize = Some s }
  | "Force-linking", none ->
      { v with forced_linking = true }
  | _ -> process_ext_default ~file ~line v tag value

let parse_lines ~file strs = 
  let struct_decls = Glist.empty () in
  let required_typeinfo = Glist.empty () in
  let required_value = Glist.empty () in
  let bss_value = Glist.empty () in
  let provided_value = Glist.empty () in
  let revision = ref 0 in
  let module_attr =
    ref { is_stdlib = false; abi_revision = -1;
	  allowed_minimum_abi_revision = -1;
	  need_initialize = None;
	  forced_linking = false;
	} in
  let parse_ext ~line f v l = 
    match !revision with
      0 -> failwith "invalid typeinfo format: no revision appeared on top"
    | 1 ->
	if l <> [] then
	  failwith_p
	    "invalid typeinfo format: extension in revision 1 file %s, line %S" file line
	else v
    | 2 | _ -> List.fold_left
	  (fun v s ->
	    if s = "" then
	      failwith_p "invalid typeinfo format: empty extension in file %s, line %S" file line
	    else
	      let tag, value = 
		try 
		  let p = String.index s '=' in
		  String.sub s 0 p, Some (String.sub s (p + 1) (String.length s - p - 1))
		with
		  Not_found -> s, None
	      in
	      f ~file ~line v tag value)
	  v l
  in
  let parse_basename s = if s = "-" then "" else s in
  List.iter
    (fun line ->
      let parse_ext f v l = parse_ext ~line f v l in
      match Str.split re_separator line with
	["R"; "1" ] ->
	  failwith "%s: the module is too old (type-1 format): recompile it with newer compiler."
      | "R" :: "2" :: ext ->
	  if !revision = 0 then revision := 2
	  else failwith "invalid typeinfo format: revision appeared in the middle";
	  let attr = parse_ext process_ext_module !module_attr ext in
	  if attr.abi_revision == -1 then
	    failwith_p "%s: the module is too old (no ABI revision): recompile with newer compiler." file;
	  if attr.abi_revision < Add_support_funcs.allowed_minimum_object_abi_revision then
	    failwith_p "%s: the module is too old (ABI revision %d): recompile with newer compiler." file attr.abi_revision;
	  if attr.allowed_minimum_abi_revision > Add_support_funcs.abi_revision then
	    failwith_p "%s: the module is too new (ABI revision %d): use newer compiler chain." file attr.abi_revision;
	  module_attr := attr
      | "R" :: _ -> failwith "bad revision for type information record; possibly compiler version incompatibility"
      | "T" :: enc_type :: base_name :: "c" :: str_decl :: ([] as ext)
      | "TSC" :: enc_type :: base_name :: str_decl :: ext ->
	  let _ = parse_ext process_ext_default () ext in
	  Glist.put struct_decls 
	    (parse_enctype enc_type,
	     parse_basename base_name,
	     Sconcrete (parse_structfields str_decl))
      | "T" :: enc_type :: base_name :: "a" :: ([] as ext)
      | "TSA" :: enc_type :: base_name :: ext ->
	  let _ = parse_ext process_ext_default () ext in
	  Glist.put struct_decls 
	    (parse_enctype enc_type,
	     parse_basename base_name,
	     Sabstract)
      | "T" :: enc_type :: base_name :: "s" :: ([] as ext)
      | "TSS" :: enc_type :: base_name :: ext ->
	  let _ = parse_ext process_ext_default () ext in
	  Glist.put struct_decls
	    (parse_enctype enc_type,
	     parse_basename base_name,
	     Sspecial)
      | "T" :: enctype :: ([] as ext)
      | "TI" :: enctype :: ext ->
	  let _ = parse_ext process_ext_default () ext in
	  Glist.put required_typeinfo (parse_enctype enctype);
      | "E" :: value_name :: enctype :: ext ->
	  let req = parse_ext process_ext_extern Req_extern ext in
	  if req = Req_extern then
	    Glist.put required_value (value_name, parse_enctype enctype)
	  else
	    Glist.put bss_value (value_name, parse_enctype enctype);
	  ()
      | "D" :: value_name :: enctype :: ext ->
	  let attr = parse_ext process_ext_declare
	      { attr_required_native_libraries = [] }
	      ext in
	  Glist.put provided_value (value_name, parse_enctype enctype, attr)
      | tag :: ext ->
	  if !revision < 2 then
	    failwith_p "unknown/invalid type information: %S" line
	  else if is_extension_critical tag then
	    failwith_p "unknown critical line in information: %S" line
      | [] -> ()
    ) strs;
  { mi_structs = 
    list_map
      (function (LTstruct t, b, d) -> t, b, d
	| _ -> failwith "parse error: non-struct struct declaration")
	(Glist.to_list struct_decls);
    mi_required_typeinfo = Glist.to_list required_typeinfo;
    mi_required_values = Glist.to_list required_value;
    mi_bss_values = Glist.to_list bss_value;
    mi_provided_values = Glist.to_list provided_value;
    mi_attributes = !module_attr;
  }

let read_string ~file s = 
  let s = 
    (* strip final NUL *)
    if s.[String.length s - 1] = '\000' then 
      String.sub s 0 (String.length s - 1)
    else s
  in
  parse_lines ~file (Str.split re_line_separator s)

let read_stream ~file s =
  let r = Glist.empty () in
  begin
    try
      while true do
	let l = input_line s in
	let l = if l <> "" && Str.last_chars l 1 = "\000" then
	  String.sub l 0 (String.length l - 1) else l in
	let l = if l <> "" && Str.last_chars l 1 = "\n" then
	  String.sub l 0 (String.length l - 1) else l in
	if l <> "" && l <> "\n" then
	  Glist.put r l;
      done
    with
      End_of_file -> ()
  end;
  parse_lines ~file (Glist.to_list r)
	
let parse_compile_unit file = 
  let command = system_path () ^ "/tools/read-elf-section" in
  let commandline = command ^ " -S __failsafeC_typeinfo " ^
    Filename.quote file in
  let in_proc = Unix.open_process_in commandline in
  let r = read_stream ~file in_proc in
  if Unix.close_process_in in_proc <> Unix.WEXITED 0 then 
    failwith "Reading elf section failed:"
  else
    r

let parse_unit_manifest file = 
  let i = open_in file in
  let r = read_stream ~file i in
  close_in i;
  r

