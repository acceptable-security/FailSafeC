(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by: Yutaka Oiwa on 2005-2006. *)

open Linker_types

let error_exit s = 
  prerr_endline ("Error from linker: " ^ s);
  exit 1

let warn s = 
  prerr_endline ("Warning from linker: " ^ s)

let check_main_function r = 
  let (modnam, typ) = 
    try
      Hashtbl.find r.Resolve.provided_values "main"
    with
      Not_found ->
	error_exit "no main() function provided in any modules";
  in
  match typ with
    LTfunction 
      (([] | [LTconcrete "i"; LTpointer (LTpointer (LTconcrete "c"))] as a),
       var,
       (LTconcrete ("v" | "i" as rt) )) 
      when not var or a = []
    -> begin
      if rt <> "i" then
	warn "main() returns void (should be int)";
      ()
    end
  | _ -> error_exit 
	(Printf.sprintf "bad type %S for main() (in module %S)"
	   (strof_ltype typ) modnam)

let main ~output l = 
  let r = 
    try
      let modules = Collect_modules.f l in
      Resolve.f modules
    with Failure s -> error_exit s
  in
  check_main_function r;
  let stubtree = Generate_link_stubs.generate r in
  let stubf = open_out (output ^ ".linkstub.safe.c") in
  begin
    let headerf = open_in (Parse_typeinfo.system_path () ^ "/runtime/include-header") in
    try
      while true do
	output_string stubf (input_line headerf ^ "\n")
      done
    with
      End_of_file -> close_in headerf
  end;
  C_pp.pp_print_program (Format.formatter_of_out_channel stubf) stubtree;
  close_out stubf;
  let ldsf = open_out (output ^ ".linkstub.script") in
  Generate_ldscript.p ldsf r;
  close_out ldsf;
  let ldaf = open_out (output ^ ".linkstub.script2") in
  Generate_ldscript.ptail ldaf r;
  close_out ldaf

let _ = 
  if not !Sys.interactive then begin
    let l = Glist.empty () in
    let output = ref "a.out" in
    Arg.parse
      [
       "-o", Arg.String ((:=) output), "\tspecify output filename prefix";
       "--debug", Arg.String (Debug.parse_debug_flags), "\tset compiler-debugging flags";
       "---systemdir", Arg.String ((:=) Parse_typeinfo.system_path_ref), "\t(undocumented)";
     ]
      (Glist.put l) "link:";
    main ~output:!output (Glist.to_list l)
  end
