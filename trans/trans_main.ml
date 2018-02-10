(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by: Yutaka Oiwa. *)

open Format

let dprintf l f = Debug.dprintf ~category:2 l f
let get_debug_flags () = Debug.get_debug_flags ~category:2

let use_c99_tokenizer = ref false

let read fname =
  C_parserhelper.reset fname;
  let chan = open_in fname in
  try
    let lexbuf = Lexing.from_channel chan in
    let t = C_parser.translation_unit C_lexer.token lexbuf in
    close_in chan;
    t
  with
    e -> close_in chan; raise e
	
let read_chan chan =
  C_parserhelper.reset "";
  if !use_c99_tokenizer then C_lexer.use_c99_constants := true;
  let lexbuf = Lexing.from_channel chan in
  C_parser.translation_unit C_lexer.token lexbuf

let systemdir = ref ""
let includedirs = ref []
let defines = ref []

let use_weak_optimizer = ref false
let use_location = ref true

type stopat = 
    Never | Preprocessor | Parse | Type | TypePP | Type_filter 
  | Il0p | Il0 | Il2 | Il2reduce | Ils | SSA | NewSSA_test | Ilc | CodeGen | Translated
  | Il3 | Il3opt | Output

let stop_at_alist = 
  [
   "preprocessor", Preprocessor;
   "parsing", Parse;
   "typing", Type;
   "typing-pp", TypePP;
   "typing2", Type_filter;
   "il0p", Il0p;
   "il0", Il0;
   "il2", Il2;
   "classify-local", Il2;
   "il2-reduce", Il2reduce;
   "ils", Ils;
   "ssa", SSA;
   "new-ssa-test", NewSSA_test;
   "ilc", Ilc;
   "il3", Il3;
   "il3opt", Il3opt;
   "translated", Translated;
   "code-gen", CodeGen;
   "output", Output;
 ]

let stop_at_symbols_list = Util.list_map fst stop_at_alist

let make_cpp_cmdline fname = 
  let sysincludedir = 
    if !systemdir = "" then failwith "panic: systemdir not specified"
    else !systemdir ^ "/include"
  in
  let definition_endian =
    if Fsc_config.is_littleendian
    then "-D__failsafeC_little_endian"
    else "-D__failsafeC_big_endian" in
  let args =
    ["cpp"; "-nostdinc"; "-x" ; "c"] @
    ["-idirafter"; sysincludedir] @
    (Util.map_flatten (fun d -> ["-I"; d]) !includedirs) @
    [if !use_c99_tokenizer then "-std=c99" else "-ansi";
     "-undef";
     "-D__failsafeC";
     definition_endian] @
    !defines @
    [fname]
  in
  (String.concat " "
     (Util.map_flatten
	(fun s -> [Filename.quote s]) args))

let put_top_include_directive och = 
  if !systemdir <> "" then
    let i = open_in (!systemdir ^ "/runtime/include-header") in
    try
      while true do
	Format.pp_print_string och (input_line i);
	Format.pp_print_newline och ()
      done
    with
      End_of_file -> close_in i

let force_stack_allocation = ref false

let use_polymorphic_analysis = ref true

let process ~stopat ~outfile fname = 
  let () = Install_fsc_extension.install () in

  let debugout_formatter = 
    if !use_location then Locterm.location_formatter
    else Format.std_formatter
  in
  
  let cmdline = make_cpp_cmdline fname in
  dprintf 1 "preprocessing...";
  Debug.dprintf ~category:9 1 "invoking %s ..." cmdline;
  let cmdline = 
    if Sys.os_type <> "Win32" then cmdline else 
    "sh -c " ^ Filename.quote cmdline
  in
  if stopat = Preprocessor then begin
    let cmdline = 
      if outfile = "" then cmdline
      else cmdline ^ " > " ^ Filename.quote outfile 
    in
    exit (Sys.command cmdline);
  end;
  let chan = Unix.open_process_in cmdline in
  dprintf 1 "parsing...";
  let t =
    try read_chan chan with e -> close_in chan; raise e
  in

  if Unix.close_process_in chan <> Unix.WEXITED 0 then
    (dprintf 1 "preprocessor failed."; exit 1);
  
  if stopat = Parse then
    (C_pp.pp_print_program debugout_formatter t;
     exit 0);

  dprintf 1 "typing...";
  let genv, t = C_typing.parse_global_declarations t in
  if stopat = Type then
    (Ctt_formatter.pp_global_declarations std_formatter t;
     exit 0);
  if stopat = TypePP then 
    (C_pp.pp_print_program debugout_formatter
       (Ctt_to_ptree.convert_program ~genv t);
     exit 0);

  dprintf 1 "cleaning...";
  let t = Expunge_localstatic.reduce_program ~genv t in
  let genv, t = Ctt_pad_structures.translate_program genv t in
  let genv, t = Ctt_reduce_mdarray.reduce_program ~genv t in
  let genv, t = Ctt_reduce_unions.translate_program ~genv t in
  let genv, t = Ctt_add_tentative_initializer.reduce_program ~genv t in
  let genv, t = Ctt_remove_unuseddecls.f ~genv t in
  if stopat = Type_filter then 
    (Ctt_formatter.pp_global_declarations std_formatter t;
     exit 0);

  dprintf 1 "analyzing assignments...";
  let genv, t = genv, Cttm_expunge_assignment.translate_program t in
  let genv, t = genv, Separate_side_effect.translate_program_to_il0 ~genv t in
  if stopat = Il0p then (Il0.pp_il0_program Il_formatter.pp_il_initializer std_formatter t; exit 0);
  let genv, t = Il0_translate_bitfield.translate_program ~genv t in
  let genv, t = Il0_type_canonify.translate_program ~genv t in
  if stopat = Il0 then (Il0.pp_il0_program Il_formatter.pp_il_initializer std_formatter t; exit 0);
  let orig_t = t in
  let t = Flatten_statement.translate_program ~genv t in

  dprintf 1 "classifying local variables...";

  let t = Classify_local.translate_program ~genv t in
  if stopat = Il2 then (Il2_formatter.pp_il2_program std_formatter t; exit 0);

  dprintf 1 "reducing local variables...";

  let t = 
    let param = if !force_stack_allocation then 
      { Install_fsc_extension.fsc_default_il2_reduction_parameter with 
	Il2_reduce_local_variable.forced_stack_alloc = true }
    else 
      Install_fsc_extension.fsc_default_il2_reduction_parameter
    in (* TODO: use function attribute to determine setjmp functions *)
    Il2_reduce_local_variable.translate_program ~genv ~param t
  in
  if stopat = Il2reduce then (Il2_formatter.pp_il2_program std_formatter t; exit 0);

  dprintf 1 "making fat pointer representation...";
  let t = Separate_fatpointer.translate_program ~genv t in
  if stopat = Ils then 
    (Ils_formatter.pp_ils_program Format.std_formatter t; exit 0);

  if stopat = NewSSA_test then (Ils_ssa_translate.test t; exit 0);

  let t = Ils_ssa_translate.translate_program t in

  if stopat = SSA then 
    (Ils_formatter.pp_ils_program Format.std_formatter t; exit 0);

  dprintf 1 "analyzing pointer usage (dummy)...";
  let t = Dummy_analyzer.f ~genv t in
  
  let t = 
    if !use_polymorphic_analysis then
      (dprintf 1 "analyzing polymorphic function types...";
       Ils_basetype_analysis.f ~genv t)
    else t
  in

  dprintf 1 "inserting check condition...";
  let t = Insert_check.f ~genv t in
  if stopat = Ilc then
    (Ilc_formatter.pp_ilc_program Format.std_formatter t; exit 0);
  dprintf 1 "translating...";
  let t = Translate_to_il3.f ~genv t in
  let t = Il3_fixup.translate_program ~genv t in
  if stopat = Il3 then
    (Il3_formatter.pp_il3_program 
       Il3_formatter.pp_temp_id
       Il3_formatter.pp_ignore
       Format.std_formatter t;
     exit 0);
  let t =
    if !use_weak_optimizer then
      Il3_optimize.optimize_program_a t
    else
      Il3_optimize.optimize_program_b t
  in
  if stopat = Il3opt then
    (Il3_formatter.pp_il3_program
       Il3_formatter.pp_il3b_rexp
       Il3_formatter.pp_ignore
       Format.std_formatter t;
     exit 0);
  let t = Il3_decompose_ssa.f t in
  dprintf 1 "generating typed C code...";
  let t = Il3_to_ctt.translate_program ~genv t in
  if stopat = Translated then
    (Ctt_formatter.pp_global_declarations Format.std_formatter t;
     exit 0);
  dprintf 1 "generating C syntax tree...";
  let t = Ctt_to_ptree.convert_program ~genv ~emit_structs:false t in
  if stopat = CodeGen then begin
    C_pp.pp_print_program debugout_formatter t; exit 0
  end;

  dprintf 1 "generating support functions and declarations...";
  let t = Add_support_funcs.f ~genv ~orig_t t in
  dprintf 1 "printing...";
  if stopat = Output then begin
    put_top_include_directive debugout_formatter;
    C_pp.pp_print_program debugout_formatter t;
  end else if outfile <> "" then begin
    let ofp = open_out outfile in
    try
      let ppf =
	if !use_location 
	then Locterm.location_formatter_of_channel ~fname:outfile ofp
	else (Format.formatter_of_out_channel ofp)
      in
      put_top_include_directive ppf;
      C_pp.pp_print_program ppf t;
      close_out ofp;
    with
      e -> close_out ofp; (try Sys.remove outfile with _ -> ()); raise e
  end;
  dprintf 1 "done."

open Arg

let usagemsg = "Usage: trans [options]"

let dummy_f = Unit (fun () -> assert false)

let userusage = ref (fun () -> assert false)

let f_options = 
  ["force-emit-stack-unwinding", Set Transutil.force_emit_stack_unwind, " emit stack management code for every function";
   "no-polymorphic-analysis", Clear use_polymorphic_analysis, " disable polymorphic function analysis";
   "ptr-compare-signed-offset", Set Transutil.use_signed_compare_for_offset, " use signed comparison for pointer offsets";
   "gnu-autoconf-workaround", Set C_typing.enable_gnu_autoconf_workaround, " enables workaround for autoconf";
   "no-boundary-cache", Clear Transutil.use_boundary_t, " disable caching of block boundary information";
   "no-clearflag", Clear Transutil.clear_castflag_at_memop, " do not clear redundant cast flags at memory accesses";
   "no-shortcut-cast", Clear Transutil.use_optimized_narrow_cast, " disable short-cut cat behavior of narrow-type pointers";
   "no-use-location", Clear use_location, " do not emit source location information";
   "force-setjmp-safe", Unit (fun () -> force_stack_allocation := true), " force all generated code setjmp-safe";
   "use-c99-tokens", Set use_c99_tokenizer, " (not documented)";
   "use-weak-optimizer", Set use_weak_optimizer, " (not documented)";
   "accmeth-expand-limit",
   Int 
     (fun i -> 
       if i >= 0 then Add_support_funcs.indexed_ofsmap_expand_limit := i 
       else
	 raise 
	   (Bad "-faccmeth-expand-limit must given a non-negative integer")),
   " (not documented)";
   "use-alignpad", Set Add_support_funcs.use_alignpad, " (not documented)";
   "multi-module", Unit (fun () -> Transutil.compiler_mode := Transutil.MultiModule),
   " (not documented)";
   "trust-intermodule-struct", Unit (fun () -> Transutil.compiler_mode := Transutil.TrustInterModule),
   " (not documented)";
   "stdlib-implementation", Unit (fun () -> Transutil.compiler_mode := Transutil.StdlibImplementation),
   " (not documented)";
   "trust-all-input-files", Set Install_fsc_extension.trust_input_file_anyway, " (not documented)";
 ]

let parse_f str = 
  let f, argl = 
    try
      let n = String.index str '=' in
      String.sub str 0 n,
      Some (String.sub str (n + 1) (String.length str - n - 1))
    with
      Not_found-> str, None
  in
  let rec iter = function
      [] -> raise (Arg.Bad ("unknown option -f" ^ f))
    | (f', p, _)::_ when f' = f -> begin
	match p, argl with
	  Unit f, None -> f ()
	| Set v, None -> v := true
	| Clear v, None -> v := false
	| (Unit _ | Set _ | Clear _), Some _ -> 
	    raise (Arg.Bad ("-f" ^ f ^ " do not take arguments"))
	| Int _, None ->
	    raise (Arg.Bad ("-f" ^ f ^ " requires an integer argument"))
	| Int ff, Some s ->
	    let i = try int_of_string s with Failure _ ->
	      raise (Arg.Bad ("-f" ^ f ^ " requires an integer argument"))
	    in ff i
	| _, _ -> failwith "unimp237"
    end
    | _::tl -> iter tl
  in
  iter f_options

let show_versions () = 
  Printf.printf 
    ("    Fail-Safe C compiler ABI version: %d (in >=%d, out >=%d)\n" ^^
     "    Built with: OCaml %s\n")
    Add_support_funcs.abi_revision
    Add_support_funcs.allowed_minimum_object_abi_revision
    Add_support_funcs.required_minimum_compiler_abi_revision
    Sys.ocaml_version;
  exit 0

let _ =
  let stop_at = ref Never in
  let infile = ref "" in
  let outfile = ref "" in
  if not !Sys.interactive then begin
    let argspec = Arg.align
	([
	 "-I", String (fun s -> includedirs := !includedirs @ [s]), " specify include directories";
	 "-D", String (fun s -> defines := !defines @ ["-D" ^ s]), " declare preproessor symbols";
	 "-U", String (fun s -> defines := !defines @ ["-U" ^ s]), " undeclare preproessor symbols";
(*	 "-Xfsc-extension", Set C_lexer.use_failsafec_extension, " enable Fail-Safe C extension keywords"; *)
	 "-E", Unit (fun () -> stop_at := Preprocessor), " preprocess source and print to stdout";
	 "-f", String parse_f, " specify compiler flags"]
	 @
	   List.map (function n,f,d -> "   " ^ n, f, d) f_options
	 @[
	   "", dummy_f, "";
	   "--stop-at", Symbol
	   (stop_at_symbols_list ,
	    (fun s -> stop_at :=
	      try List.assoc s stop_at_alist 
	      with Not_found -> raise (Bad "bad keyword for stop_at"))),
	 "";
	   "", dummy_f, " stop processing at given phase";
	   "--debug", String (Debug.parse_debug_flags), " set compiler-debugging options";
	   "---systemdir", String (fun s -> systemdir := s), " specify system include directory";
	   "---input-file", String ((:=) infile), " specify name for .c input file";
	   "---output-file", String ((:=) outfile), " specify name for .safe.c output file";
	   "---help-user", Unit (fun () -> !userusage ()), " (not documented)";
	   "---show-version-info", Unit show_versions, " (not documented)";
	 ])
    in
    let usagemsg = "Usage: trans [options]" in
    let usage s = Arg.usage argspec (s ^ usagemsg); exit 1 in
    userusage :=
      (fun () ->
	Arg.usage
	  (List.filter (fun (s,_,_) -> String.length s < 3 or String.sub s 0 3 <> "---") argspec)
	  ""; exit 1);
    Arg.parse argspec (fun s -> raise (Bad "bad usage")) usagemsg;
    if !infile = "" then usage "no input file specified: ";
    if !outfile = "" && !stop_at = Never then usage "no output file specified: ";
    process ~stopat:!stop_at ~outfile:!outfile !infile
  end
