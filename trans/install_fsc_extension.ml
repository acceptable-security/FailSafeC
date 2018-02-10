(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

(* install various hooks into parser routines *)
open C_abstree
open Ctt_abstree
open C_pp

let dprintf l f = Debug.dprintf ~category:140 l f
let get_debug_flags () = Debug.get_debug_flags ~category:140

let trust_input_file_anyway = ref false
let allow_require_libraries = ref false

let check_location ~loc s = 
  let loc_is_trustful = 
    match loc with
      Locterm.Unlocated -> false
    | Locterm.Located (_,_,_,loc_is_trustful) -> loc_is_trustful 
  in
  if loc_is_trustful || !trust_input_file_anyway then ()
  else
    failwith ("extension " ^ s ^ " is only allowed in system header.")

let translate_struct_by_extension ~loc ~ext def = 
  let o = ref [] in
  let () = match ext with
    [] -> ()
  | ["__fsc_attribute__", Eplist[Eplist l]] ->
      List.iter
	(function
	    Elist[Eident "named"; Estring str] ->
	      o := !o @ ["named", Estring str]
	  | Eident "external" ->
	      o := !o @ ["external", Elist []]
	  | _ -> 
	      failwith "unknown extension (42)") l
  | _ ->
      failwith "unknown extension"
  in
  { def with str_extension = !o }

let translate_external_extension ~loc ~ext ~sclass ~ty ~is_initialized = 
  let o = ref [] in
  let () = match ext with
    [] -> ()
  | ["__fsc_attribute__", Eplist[Eplist l]] ->
      List.iter
	(function
	  | Eident ("external" | "noreturn" as e) -> begin
	      check_location ~loc e;
	      match ty.ct_ty with
		Tfunction _ ->
		  o := !o @ [e, Elist []]
	      | _ -> failwith ("__fsc_attribute__((" ^ e ^ ")) can only be applied to functions")
	  end
	  | Eident "emit_typeinfo" -> begin
	      check_location ~loc "emit_typeinfo";
	      match ty.ct_ty with
		Tstruct sid ->
		  Add_support_funcs.specially_emit_structs :=
		    sid :: !Add_support_funcs.specially_emit_structs;
		  o := !o @ ["emit_typeinfo", Elist []]
	      | _ ->
		  failwith "__fsc_attribute__((emit_typeinfo)) can only be applied to struct variables"
	  end
	  | Elist [Eident "require_native_libraries"; Estring s] ->
	      if not !allow_require_libraries then
		failwith "__fsc_attribute__((require_native_libraries)) not allows in this file";
	      check_location ~loc "require_native_libraries";
	      o := !o @ ["require_native_libraries", Estring s]
	  | _ -> 
	      check_location ~loc "";
	      failwith "unknown extension (34)")
	l
  | _ ->
      failwith "unknown extension"
  in
  !o

let merge_value_extensions ~old_ext ~new_ext ~have_definition = 
  let o = ref [] in
  let f_noreturn = ref false in
  let f_external = ref false in
  let f_req_native = ref None in
  List.iter
    (function
	"noreturn", _ -> 
	  f_noreturn := true
      | "external", _ ->
	  if have_definition then
	    failwith "__fsc_attribute__((external)) declaration cannot be redefined"
	  else
	    f_external := true
      | "require_native_libraries", Estring s ->
	  f_req_native := Some s;
      | ext, _ ->
	  failwith ("unknown extension " ^ ext))
    old_ext;
  List.iter
    (function
	"noreturn", _ ->
	  f_noreturn := true;
      | "external", _ ->
	  failwith "__fsc_extension__((external)) must be declared first"
      | "require_native_libraries", Estring s ->
	  f_req_native := Some s;
      | ext, _ ->
	  failwith ("unknown extension " ^ ext))
    new_ext;
  if have_definition then
   f_noreturn := false   (* when have_definition, it must return []. *);
  ((if !f_noreturn then ["noreturn", Elist []] else []) @
   (if !f_external then ["external", Elist []] else []) @
   (match !f_req_native with None -> [] | Some s -> ["require_native_libraries", Estring s])
  )

let merge_struct_extensions ~old_ext ~new_ext =
  let o = ref [] in
  let f_named = ref None in
  let f_external = ref false in
  List.iter
    (function
	"named", n -> 
	  f_named := Some n
      | "external", _ ->
	  f_external := true
      | ext, _ ->
	  failwith ("unknown extension " ^ ext))
    old_ext;
  List.iter
    (function
	"named", n -> begin
	  match !f_named with
	    None -> f_named := Some n
	  | Some n' ->
	      if n = n' then () else failwith "inconsistent __fsc_extension__((named)) attribute"
	end
      | "external", _ ->
	  failwith "__fsc_extension__((external)) must be declared first"
      | ext, _ ->
	  failwith ("unknown extension " ^ ext))
    new_ext;
  ((match !f_named with None -> [] | Some n -> ["named", n]) @
   (if !f_external then ["external", Elist []] else []))

let remove_unused_filter ~ext _ id b = 
  dprintf 6 "declaration %s asked." id;
  if List.mem_assoc "emit_typeinfo" ext then 
    (dprintf 6 "declaration %s kept." id; true)
  else b

let install () = 
  C_lexer.extension_keywords := ["__fsc_attribute__"];
  C_lexer.use_failsafec_extension := true;
  C_typing.translate_struct_by_extension_hook := translate_struct_by_extension;
  C_typing.translate_external_extension_hook := translate_external_extension;
  C_typing.merge_value_extensions_hook := merge_value_extensions;
  C_typing.merge_struct_extensions_hook := merge_struct_extensions;
  Ctt_remove_unuseddecls.external_definition_filter_hook := remove_unused_filter;
  C_typing.fill_zero_initializer := false;
  C_typing.allow_calling_undefined_functions := true;
  if !Transutil.compiler_mode = Transutil.MultiModule then
    Ctt_add_tentative_initializer.fill_bss_initializers := false;
  ()


(* used by setjmp detection on Il2_reduce_local_variable *)

let fsc_default_il2_reduction_parameter = 
  { Il2_reduce_local_variable.is_setjmp = (fun i -> i = "__builtin_setjmp" || i = "__builtin_sigsetjmp"); 
    Il2_reduce_local_variable.forced_stack_alloc = false;
  }
