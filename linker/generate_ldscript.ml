(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2005-2006. *)

include Debug.Install (struct let category = 210 end)

open Util
open Linker_types
open Resolve

(* result is "required-name, already-provided-name" *)

let is_polymorphic_function = 
  function
      LTfunction(_, _, LTpointer (LTconcrete "v")) -> true
    | _ -> false

let translate_required_gdecl (name, o, n) =
  match o with
    LTfunction _ ->
      if is_polymorphic_function o then
	[ "GV_" ^ encode_ltype o ^ "_" ^ name, "GV_" ^ encode_ltype n ^ "_" ^ name;
	  "FSP_" ^ encode_ltype o ^ "_" ^ name, "FSP_" ^ encode_ltype n ^ "_" ^ name ]
      else
	[ "GV_" ^ encode_ltype o ^ "_" ^ name, "GV_" ^ encode_ltype n ^ "_" ^ name;
	  "FS_" ^ encode_ltype o ^ "_" ^ name, "FS_" ^ encode_ltype n ^ "_" ^ name ]
  | _ ->
      [ "GV_" ^ encode_ltype ~no_array_size:true o ^ "_" ^ name,
	"GV_" ^ encode_ltype ~no_array_size:true n ^ "_" ^ name ]

let translate_provided_gdecl (name, o, n) =
  match o with
    LTfunction _ ->
      if is_polymorphic_function o then
	[ "GV_" ^ encode_ltype n ^ "_" ^ name, "GV_" ^ encode_ltype o ^ "_" ^ name;
	  "FSP_" ^ encode_ltype n ^ "_" ^ name, "FSP_" ^ encode_ltype o ^ "_" ^ name ]
      else
	[ "GV_" ^ encode_ltype n ^ "_" ^ name, "GV_" ^ encode_ltype o ^ "_" ^ name;
	  "FS_" ^ encode_ltype n ^ "_" ^ name, "FS_" ^ encode_ltype o ^ "_" ^ name ]
  | _ ->
      [ "GV_" ^ encode_ltype ~no_array_size:true n ^ "_" ^ name,
	"GV_" ^ encode_ltype ~no_array_size:true o ^ "_" ^ name ]

let translate_tinfo o n =
  [ "fsc_typeinfo_" ^ encode_ltype o, "fsc_typeinfo_" ^ encode_ltype n ]

let translate_struct_helpers ~r o n =
  if o = n then [] else
  if Hashtbl.mem r.required_typeinfo (LTstruct n) then
  [
   "read_struct_" ^ encode_ltype (LTstruct o), "read_struct_" ^ encode_ltype (LTstruct n);
   "write_struct_" ^ encode_ltype (LTstruct o), "write_struct_" ^ encode_ltype (LTstruct n);
 ]
  else []

let translate_unknown_functions (id, t) = 
  [ "GV_Xuf__" ^ id, "GV_" ^ encode_ltype t ^ "_" ^ id ]

let iter_flatten f ht = 
  let g = Glist.empty () in
  Hashtbl.iter (fun k v -> Glist.append g (f k v)) ht;
  Glist.to_list g

let genlist r = 
  Util.map_flatten translate_provided_gdecl r.provided_renamed_identifiers @
  Util.map_flatten translate_required_gdecl r.requested_renamed_identifiers @
  iter_flatten translate_tinfo r.renamed_typeinfo @
  iter_flatten (translate_struct_helpers ~r) r.rename_list @
  Util.map_flatten (translate_unknown_functions) r.renamed_unknown_functions

let p fp r = 
  let start_time = Unix.gettimeofday () in
  dprintf_start "Generating Linker Script...";
  Printf.fprintf fp 
    "/* linker script for module id %s */\nSECTIONS\n{\n  /DISCARD/ : { *(__failsafeC_typeinfo) }\n}\n    EXTERN(FG_main);\n\n" r.new_module_hash;
  let l = genlist r in
  List.iter (fun (req, provided) -> Printf.fprintf fp "  %s = %s;\n    EXTERN(%s);\n" req provided provided) l;
  dprintf_end "done. (%.3f s)" (Unix.gettimeofday () -. start_time)

let ptail fp r = 
  let start_time = Unix.gettimeofday () in
  dprintf_start "Generating Linker Second Script...";
  Printf.fprintf fp 
    "/* linker script 2 for module id %s */\n" r.new_module_hash;
  let l = genlist r in
  List.iter (fun s -> Printf.fprintf fp "INPUT(-l%s)\n" s) r.required_native_libraries;
  dprintf_end "done. (%.3f s)" (Unix.gettimeofday () -. start_time)
