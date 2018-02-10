open Util
open Arg
open Fscw_tree
open Wl
open Wl_require
open Wl_generate
open Ctt_abstree

let dprintf l f = Debug.dprintf ~category:2 l f
let get_debug_flags () = Debug.get_debug_flags ~category:2

let make_cpp_cmdline systemdir includedirs defines fname = 
  let sysincludedir =
    if systemdir = None || Option.get systemdir = "" then failwith "panic: systemdir not specified"
    else Option.get systemdir ^ "/include"
  in
  let definition_endian =
    if Fsc_config.is_littleendian
    then "-D__failsafeC_little_endian"
    else "-D__failsafeC_big_endian" in
  let args =
    ["cpp"; "-nostdinc"] @
    ["-idirafter"; sysincludedir] @
    (Util.map_flatten (fun d -> ["-I"; d]) includedirs) @
    ["-ansi"; "-undef"; "-D__failsafeC"; definition_endian] @ defines
  in
  (String.concat " "
     (Util.map_flatten
	(fun s -> [Filename.quote s]) args))
    ^ " - < " ^ Filename.quote fname
  (* CPP input by fscw is in tempdir; use pipeline to avoid include path problems *)

let separate_fscw fn =
  Fscw_tree.reset fn;
  let chan = open_in fn in
  let lexbuf = Lexing.from_channel chan in
  try 
    let tokenizer = Fscw_lexer.make_tokenizer () in
    let t = Fscw_parser.parse_fscw tokenizer lexbuf in
    t
  with
    Failure "lexing: empty token" | Parsing.Parse_error ->
      failwith_p "%s:%d: syntax error during parsing fscw source structure" fn (!Fscw_tree.lineno)

let read_chan chan =
  C_parserhelper.reset "";
  let lexbuf = Lexing.from_channel chan in
  C_parser.translation_unit C_lexer.token lexbuf

let output_string_and_pos outch (str, (file, line)) =
  Printf.fprintf outch "#line %d \"%s\"\n" line file;
  output_string outch str

let process ~infile ~outfile systemdir includedirs defines =
  let fscw = separate_fscw infile in
  (*Printf.printf "P:%s\nC:%s\nF:%s\nR:%s\nN:%s\n"
    fscw.r_prologue fscw.r_c_prologue fscw.r_fsc_prologue fscw.r_required_decls fscw.r_native_implementation;*)

  let outch = open_out outfile in
  begin try
  let tmpfn, tmpch = Filename.open_temp_file "fscw" ".c" in
  output_string_and_pos tmpch fscw.r_fsc_prologue;
  close_out tmpch;
  let cmdline = make_cpp_cmdline systemdir includedirs defines tmpfn in

  let chan = Unix.open_process_in cmdline in
  dprintf 1 "parsing...";
  let t =
    try read_chan chan with e -> close_in chan; raise e
  in
  let r = Unix.close_process_in chan in
  Sys.remove tmpfn;
  if r <> Unix.WEXITED 0 then
    (dprintf 1 "preprocessor failed."; exit 1);

  let t = C_abstree_remove_definitions.f t in
  let genv, t, gv, dep, orig = C_global_only_typing.parse_global_declarations t in
  dprintf 1 "cleaning...";
  let t = Expunge_localstatic.reduce_program ~genv t in
  let genv, t = Ctt_pad_structures.translate_program genv t in
  let genv, t = Ctt_reduce_mdarray.reduce_program ~genv t in
  let genv, t = Ctt_reduce_unions.translate_program ~genv t in
  let genv, t = Ctt_add_tentative_initializer.reduce_program ~genv t in
  (*let genv, t = Ctt_remove_unuseddecls.f ~genv t in*)
  dprintf 1 "analyzing assignments...";
  let genv, t = genv, Cttm_expunge_assignment.translate_program t in
  let genv, t = genv, Separate_side_effect.translate_program_to_il0 ~genv t in
  let genv, t = Il0_translate_bitfield.translate_program ~genv t in
  let genv, t = Il0_type_canonify.translate_program ~genv t in

  let genv2 = make_genv2 genv gv dep orig in

  let req_list = List.map
    (function
    | RequireTypedef name -> if name = "fscw_primitive_types" then WLR_primitive_type  else WLR_function (WLTabstract name)
    (*| RequireStruct name -> WLR_typedef (WLTstruct (wl_struct_of_struct_tag ~genv2 name)) *)
    | RequireStruct name -> WLR_function (WLTstruct (wl_struct_of_struct_name ~genv2 name))
    | RequireFunction (name, decl_p) -> WLR_wrapper_decl (name, decl_p)
    | AutoGenerate name -> WLR_autogen name
    | EmitTypeinfo name -> WLR_emit_typeinfo (WLTstruct (wl_struct_of_struct_name ~genv2 name))
    | RequireValue name -> WLR_value name
    | EmitModuleExtension str -> WLR_emit_modext (Parse_const.decode_c_string str)
    ) (fst fscw.r_required_decls) in

  let req_list = Wl_require.resolve_required ~genv2 req_list in

  let rec strof_wltype = function
      WLTvoid -> "void"
    | WLTbuiltin bt -> sfprintf "%a" Ctt_formatter.pp_builtin_type bt
    | WLTpointer -> "ptr"
    | WLTstruct { wl_struct_tag = t; wl_struct_name = n; wl_struct_id = i } -> 
	"struct (tag=" ^ Option.default "-" t ^ ", name=" ^ Option.default "-" n ^ ", id = " ^ string_of_int i ^ ")"
    | WLTabstract s -> "abstract " ^ s
    | WLTstring -> "string"
  in

(*  List.iter
    (fun r ->
      prerr_endline 
	(match r with
	| WLR_primitive_type -> "wlr_primitive_types"
	| WLR_typedef t -> "wlr_typedef " ^ strof_wltype t
	| WLR_function t -> "wlr_function " ^ strof_wltype t
	| WLR_sp_unwrap t -> "wlr_sp_unwrap " ^ strof_wltype t
	| WLR_wrapper_decl (s, true) -> "declare " ^ s
	| WLR_wrapper_decl (s, false) -> "extern " ^ s
	| WLR_autogen s -> "declare_auto " ^ s
	| WLR_typeinfo t -> "typeinfo " ^ strof_wltype t
	| WLR_emit_typeinfo t -> "emit_typeinfo " ^ strof_wltype t))
    req_list; *)

  let declared_functions = 
    Util.map_flatten
      (function
	  WLR_wrapper_decl (name, true) ->
	    let t = snd (c_type_of_varname ~genv2 name) in
	    [ true, canonify_c_type_mem t, t, name ]
	| WLR_wrapper_decl (name, false) ->
	    let t = snd (c_type_of_varname ~genv2 name) in
	    [ false, canonify_c_type_mem t, t, name ]
	| WLR_value name ->
	    let t = snd (c_type_of_varname ~genv2 name) in
	    [ false, canonify_c_type_mem t, t, name ]
	| WLR_autogen name -> 
	    (* for auto-gen entries there will be correspoinding WLR_wrapper_decl; no need to handle here *)
	    []
	| _ -> [])
      req_list
  in

  let used_structs = 
    let d = Hashtbl.create 17 in
    let o = ref [] in
    let rec walk i = 
      if Hashtbl.mem d i then () else begin
	Hashtbl.replace d i ();
	o := i :: !o;
	let s = Earray.get genv.struct_table i in
	List.iter
	  (fun (id, (cty, _ofs)) ->
	    walk_type cty)
	  s.str_fields_byname
      end
    and walk_type cty = 
      match cty.ct_ty with
	Tvoid | Tbuiltin _ | Tabstract _ -> ()
      | Tstruct id -> walk id
      | Tpointer t | Tarray(t, _) -> walk_type t
      | Tfunction(ats, _, rt) ->
	  List.iter walk_type ats; walk_type rt
    in
    List.iter
      (function
	WLR_typeinfo (WLTstruct w) ->
	  walk w.wl_struct_id
      | _ -> ())
      req_list;
    !o 
  in
    
  let filter_struct_table l orig = 
    let t = Earray.empty () in
    List.iter
      (fun i -> Earray.set t i (Earray.get orig i)) l;
    t
  in

  let new_orig_t = 
    Util.list_map
      (fun (decl_p, ctymem, ctyorig, name) ->
	let orig_storage_class = (List.assoc name genv.global_declarations).gbind_storage_class in
	let orig_ext = 
	  match orig_storage_class with
	    Global e | Extern e -> e
	  | ModuleStatic -> assert false
	in
	Locterm.locput_dummy
	  (Il0.IL0declVariable
	     ((if decl_p then Global orig_ext else Extern orig_ext), ctymem, name, None)))
      declared_functions in

  let new_genv = { genv with struct_table = filter_struct_table used_structs genv.struct_table } in

  Record_globalnames.clear ();

  let stubs = 
    let loc = Locterm.dummy_location in
    let t = 
      map_flatten
	(function 
	    (true, ({ ct_ty = Tfunction(at, vf, rt) } as ctymem), ctyorig, id) ->
	      let argnum = List.length at in
	      let args = ExtList.List.init
		  argnum
		  (fun i -> "arg" ^ string_of_int (i + 1))
	      in
	      let stubf = 
		Translate_to_il3.generate_bridge_function ~genv ~loc (Global []) id ctymem args
	      in
	      let stubrec = Translate_to_il3.generate_function_stub ~genv ~loc (Global []) id ctymem args in
	      let proto = 
		Locterm.locput_dummy
		  (Il3.IL3declVariable(Global [], Translate_to_il3.translate_c_type_genv ~genv ctymem,
				       Translate_to_il3.translate_global_function_name ~genv id ctymem,
				       None))
	      in
	      [proto; stubf; stubrec]
	  | (false, ctymem, ctyorig, id) -> begin
	      match ctymem.ct_ty with
		Tfunction _ -> []
	      | _ -> begin
		  let t = Locterm.locput_dummy
		      (Il.ILdeclVariable (Extern [], ctyorig, id, None)) in
		  Translate_to_il3.f ~genv [t]
	      end
	  end
	  | _ -> [])
	declared_functions
    in
    let t = Il3_to_ctt.translate_from_raw_il3 ~genv t in
    Ctt_to_ptree.convert_program ~genv ~emit_structs:false t
  in

  List.iter (function 
             | WLR_typeinfo wty when wty <> WLTvoid ->
               let p = parse_wl_type ~genv2 wty in
               Record_globalnames.require_name (Record_globalnames.GNtypeinfo p.tp_c_t)
             | WLR_emit_typeinfo (WLTstruct s) ->
               Add_support_funcs.specially_emit_structs := s.wl_struct_id::!Add_support_funcs.specially_emit_structs
             | _ -> ()) req_list;
  
  let module_extensions = 
    Util.map_flatten
      (function
	  WLR_emit_modext s -> [s]
	| _ -> [])
      req_list
  in

  let rr = Add_support_funcs.f ~genv:genv ~orig_t:new_orig_t ~mod_exts:module_extensions stubs in

  let fmt = (Format.formatter_of_out_channel outch) in

  output_string_and_pos outch fscw.r_c_prologue;

  let (_, (_, line_1)), (_, (_, line_2)) = fscw.r_c_prologue, fscw.r_fsc_prologue in
  Printf.fprintf outch "#line %d \"%s\"\n"
                       (line_2 - line_1 + 2)
                       outfile;

  List.iter
    (fun s -> Format.fprintf fmt "%s@\n" s)
    [ "#define FSC_RUNTIME_LIBRARY";
      "#include <fsc_runtime.h>";
      "#include <wrapper_helper.h>";
      "#include <wrapper/fscw_helper.h>";
      "#ifndef FSCW_INLINE";
      "#define FSCW_INLINE static inline";
      "#endif";
    ];

  C_pp.pp_print_program fmt rr;

  List.iter (pp_wlr fmt ~genv2) req_list;

  output_string_and_pos outch fscw.r_native_implementation;

  close_out outch;
  with
    e -> close_out outch; (try Sys.remove outfile with _ -> ()); raise e
  end;
  exit 0

let parse_f str =
  if str = "stdlib-implementation" then
    Transutil.compiler_mode := Transutil.StdlibImplementation
  else if str = "multi-module" then
    Transutil.compiler_mode := Transutil.MultiModule
  else
    raise (Arg.Bad ("unknown option -f" ^ str))

let _ =
  let systemdir = ref None in
  let includedirs = ref [] in
  let infile = ref None in
  let outfile = ref None in
  let setfile var s = var := Some s in
  let defines = ref [] in
  if not !Sys.interactive then begin
    let argspec = Arg.align
	([
	 "-I", String (fun s -> includedirs := !includedirs @ [s]), " specify include directories";
	 "-D", String (fun s -> defines := !defines @ ["-D" ^ s]), " declare preproessor symbols";
	 "-U", String (fun s -> defines := !defines @ ["-U" ^ s]), " undeclare preproessor symbols";
         (*"--debug", String (Debug.parse_debug_flags), " set compiler-debugging options"; *)
         "-f", String (fun s -> parse_f s), " (not documented)";
         "---systemdir",  String (setfile systemdir), " specify system include directory";
         "---input-file",  String (setfile infile), " specify name for .fscw input file";
         "---output-file", String (setfile outfile), " specify name for .c output file";
	 ])
    in
    let usagemsg = "Usage: fscw [options]" in
    let usage s = Arg.usage argspec (s ^ usagemsg); exit 1 in
    Arg.parse argspec (fun s -> raise (Bad "bad usage")) usagemsg;
    if !infile = None  then usage "no input file specified: ";
    if !outfile = None then usage "no output file specified: ";
    let infile  = Option.get !infile in
    let outfile = Option.get !outfile in
    Install_fsc_extension.trust_input_file_anyway := true;
    Install_fsc_extension.allow_require_libraries := true;
    Install_fsc_extension.install ();
    process ~infile:infile ~outfile:outfile !systemdir !includedirs !defines
  end

