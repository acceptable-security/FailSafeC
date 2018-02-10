open Wl
open Wl_require
open C_abstree
open Ctt_abstree
open Transutil
open Translate_to_il3

let specs_and_declarator ~genv2 sc_list ty name =
  let specs, decl = Ctt_to_ptree.convert_declarator ~genv:genv2.genv
                      ty name in
    (List.map (fun sc -> StorageClass sc) sc_list) @ specs, decl


(*
 * pp_wlr_typedef (WLTbuitin Tint)
 * prints
 *   "typedef value fscw_int;"
 *)
let pp_wlr_typedef ppf ~genv2 wty =
  if wty = WLTvoid then () else
  let p = parse_wl_type ~genv2 wty in
  let name = fscw_T_of_prop p in
  let specs, decl = specs_and_declarator ~genv2
                      [Typedef] p.tp_prop.packed_t name in
  let typedef = PdeclVariable(specs, [PinitDecl(decl, None)]) in
  Format.fprintf ppf "%a@." C_pp.pp_print_declaration (Locterm.locput_dummy typedef)

let is_incomplete ~genv2 wty =
  if wty = WLTvoid then true else
  let p = parse_wl_type ~genv2 wty in
  match wl_type_of_c_type ~genv2 p.tp_c_t with
    WLTstruct s -> (*false *)
      p.tp_c_t.ct_size = None
  | _ -> false

(*
 * pp_wlr_function (WLTbuitin Tint)
 * prints
 *   inline fscw_T fscw_T_wrap(T v) { return 
 *
 *)
let pp_wlr_function ppf ~genv2 wty =
  if is_incomplete ~genv2 wty then () else
  let wrap   = "FSCW_INLINE <fscw_T> <fscw_T>_wrap(<T> v) { return (<fscw_T>)<wrap_expr>; }\n" in
  let unwrap = "FSCW_INLINE <T> <fscw_T>_unwrap(<fscw_T> v) { return (<T>)<unwrap_expr>; }\n" in
  let param  = "#define <fscw_T>_param(n) <param_decl>\n" in
  let member = "#define <fscw_T>_member(t, m) <member_expr>\n" in
  let rd     = "FSCW_INLINE <fscw_T> <fscw_T>_read(const <fscw_T> *p) { return *p; }\n" in
  let wr     = "FSCW_INLINE void <fscw_T>_write(<fscw_T> *p, <fscw_T> v) { *p = v; }\n" in
  let rd_unw = "FSCW_INLINE <T> <fscw_T>_read_unwrap(const <fscw_T> *p) {\n" ^
               "  return <fscw_T>_unwrap(<fscw_T>_read(p));\n" ^
               "}\n" in
  let wrp_wr = "FSCW_INLINE void <fscw_T>_wrap_write(<fscw_T> *p, <T> v) {\n" ^
               "  <fscw_T>_write(p, <fscw_T>_wrap(v));\n" ^
               "}\n" in
  let rd_str = "extern <fscw_T> read_<tag>(base_t, ofs_t);\n" in
  let wr_str = "extern void write_<tag>(base_t, ofs_t, <fscw_T>);\n" in
  let rd_pp  = "#define <fscw_T>_read_pointer_param(p, n) <rd_pp_expr>\n" in
  let wr_pp  = "#define <fscw_T>_write_pointer_param(p, n, v) <wr_pp_expr>\n" in
  let alloc  = "FSCW_INLINE <fscw_T> *<fscw_T>_alloc(size_t n) {\n" ^
               " return (<fscw_T>*)fsc_alloc_static_block(&<ti>.val, n);\n" ^
               "}\n" in

  let p = parse_wl_type ~genv2 wty in
  let pp = p.tp_prop in

  let tag = match wl_type_of_c_type ~genv2 p.tp_c_t with
    | WLTstruct s -> "struct_" ^ Translate_to_il3.encoded_name_of_struct ~genv:genv2.genv s.wl_struct_id
    | _ -> "" in

  let wrap_expr, unwrap_expr, rd_pp_expr, wr_pp_expr =
  match pp.is_pointer, pp.size, pp.is_fat, pp.is_floating with
  | true, _, _, _ ->
    (if wty = WLTstring then
      "(v ? wrapper_make_new_static_string(v) : 0)"
     else
      "ptrvalue_of_base_ofs((base_t)v, 0)"
    ), "",
    "ptrvalue_of_value_Pv(read_word(p##_base, p##_ofs+(n)*4))", "write_word(p##_base, p##_ofs+(n)*4, value_of_ptrvalue(v), 0)"
  | _, Size_byte,  false, false -> "v", "v",
                                   "read_byte(p##_base, p##_ofs+(n))", "write_byte(p##_base, p##_ofs+(n), (v), 0)"
  | _, Size_hword, false, false -> "v", "v",
                                   "read_hword(p##_base, p##_ofs+(n)*2)", "write_hword(p##_base, p##_ofs+(n)*2, (v), 0)"
  | _, Size_word,  false, false -> "v", "v",
                                   "read_word(p##_base, p##_ofs+(n)*4)", "write_word(p##_base, p##_ofs+(n)*4, (v), 0)"
  | _, Size_dword, false, false -> "v", "v",
                                   "read_dword(p##_base, p##_ofs+(n)*8)", "write_dword(p##_base, p##_ofs+(n)*8, (v), 0)"
  | _, Size_word,  true,  false ->  "value_of_base_vaddr(0, (v))",  "vaddr_of_value(v)",
                                   "read_word(p##_base, p##_ofs+(n)*4)", "write_word(p##_base, p##_ofs+(n)*4, (v), 0)"
  | _, Size_dword, true,  false -> "dvalue_of_base_vaddr(0, (v))", "vaddr_of_dvalue(v)",
                                   "read_dword(p##_base, p##_ofs+(n)*8)", "write_dword(p##_base, p##_ofs+(n)*8, (v), 0)"
  | _, Size_word,  false, true  -> "v", "v",
                                   "float_of_word(read_word(p##_base, p##_ofs+(n)*4))",
                                   "write_word(p##_base, p##_ofs+(n)*4, value_of_int(word_of_float(v)), 0)"
  | _, Size_dword, false, true  -> "v", "v",
                                   "double_of_dword(read_dword(p##_base, p##_ofs+(n)*8))",
                                   "write_dword(p##_base, p##_ofs+(n)*8, dvalue_of_dword(dword_of_double(v)), 0)"
  | _, Size_other _, _, _ ->  (* struct *)
    let sz = Big_int_infix.int_of_big_int (Util.someof p.tp_c_t.ct_size) in
    let r, w = if tag = "" then "", "" else
      Printf.sprintf "read_%s(p##_base, p##_ofs+(n)*%d)" tag sz,
      Printf.sprintf "write_%s(p##_base, p##_ofs+(n)*%d, (v))" tag sz
    in
    "", "", r, w
  | _, _, _, _ -> assert false
  in
  let param_decl = match pp.unpacked_types with
  | [t] -> [(t, "n")]
  | [bt; t] when bt = type_base_t & t = type_ofs_t -> [(bt, "n##_base"); (t, "n##_ofs")]
  | [bt; t] when bt = type_base_t ->  [(bt, "n##_base"); (t, "n")]
  | _ -> assert false
  in
  let param_decl = List.map (fun (tp, nm) ->
    let specs, decl = specs_and_declarator ~genv2 [] tp nm in
    PpdeclConcrete(specs, decl)
    ) param_decl
  in
  let param_decl =
    let b = Buffer.create 128 in
    let f = Format.formatter_of_buffer b in
    Format.fprintf f "@[<h>%a@]@?"
      (C_pp.pp_print_list ~elem_pp:C_pp.pp_print_paramdecl ~sep_pp:C_pp.pp_print_sep_comma) param_decl;
    Buffer.contents b
  in
  let member_expr = if pp.is_fat then "(&(t)->fld_##m.cv)" else "(&(t)->fld_##m)" in

  let ti_name = "fsc_typeinfo_" ^
                Translate_to_il3.encoded_name_of_type_genv
                  ~genv:genv2.genv
                  (canonify_c_type_mem p.tp_c_t) in

  let tmpl =
    (if wrap_expr <> "" then [wrap] else []) @
    (if unwrap_expr <> "" then [unwrap] else []) @
    [param; member; rd; wr] @
    (if tag <> "" then [rd_str; wr_str] else []) @
    (if unwrap_expr <> "" then [rd_unw] else []) @
    (if wrap_expr <> "" then [wrp_wr] else []) @
    (if rd_pp_expr <> "" then [rd_pp] else []) @
    (if wr_pp_expr <> "" then [wr_pp] else []) @
    [alloc]
  in
  let tmpl = List.fold_left (^) "" tmpl in
  let repl name value str = Str.global_substitute (Str.regexp name) (fun s -> value) str in
  let result = repl "<fscw_T>" (fscw_T_of_prop p) tmpl in
  let result = repl "<T>" p.tp_c_t_str result in
  let result = repl "<wrap_expr>" wrap_expr result in
  let result = repl "<unwrap_expr>" unwrap_expr result in
  let result = repl "<param_decl>" param_decl result in
  let result = repl "<member_expr>" member_expr result in
  let result = repl "<tag>" tag result in
  let result = repl "<rd_pp_expr>" rd_pp_expr result in
  let result = repl "<wr_pp_expr>" wr_pp_expr result in
  let result = repl "<ti>" ti_name result in

  Format.fprintf ppf "%s@." result

let pp_wlr_wrapper_decl ppf ~genv2 (name, decl_p) =
  let genv = genv2.genv in
  let abs_ty, ty = c_type_of_varname ~genv2 name in
  match abs_ty.ct_ty with
  | Tfunction (param_ty, is_var, ret_ty) ->
      if !Transutil.compiler_mode <> Transutil.MultiModule then
	Format.fprintf ppf "/* manifest entry\n%s\t%s\t%s\n*/\n\n"
	  (if decl_p then "D" else "E")
          name
          (Translate_to_il3.encoded_name_of_type_genv
             ~genv
             (canonify_c_type_mem ty));
    let full_name = Translate_to_il3.translate_global_function_name ~genv name 
                      (canonify_c_type_mem ty) in
    Format.fprintf ppf "@[<h>#define fscw_%s %s@]@\n" name full_name;

    let ret_wty = wl_type_of_c_type ~genv2 ret_ty in
    let param_wty = List.map (wl_type_of_param_c_type ~genv2) param_ty in
    let param_wty = if is_var then param_wty @ [WLTpointer]
                    else if param_wty = [] then [WLTvoid]
                    else param_wty in
    let param_wty = Util.list_mapi (fun n wty -> wty, Printf.sprintf "param_%d" n) param_wty in
    Format.fprintf ppf "extern %s fscw_%s(@[<hv>%a@]);@\n"
      (fscw_T_of_wty ~genv2 ret_wty)
      name
      (C_pp.pp_print_list
        ~sep_pp:C_pp.pp_print_sep_comma
        ~elem_pp:(fun ppf (wty, pname) ->
          match wty with
          | WLTvoid -> Format.fprintf ppf "void"
          | _ -> Format.fprintf ppf "%s_param(%s)" (fscw_T_of_wty ~genv2 wty) pname
        )
      ) param_wty
  | _ -> assert false (* TODO *)

let pp_wlr_special_unwrapper ppf ~genv2 wty =
  match wty with
  | WLTstring ->
    let code =
      "typedef struct { char *v; void *tmpbuf; } fscw_string_buf;\n" ^
      "FSCW_INLINE fscw_string_buf fscw_string_get(base_t base, ofs_t ofs, char *libname)\n" ^
      "{\n" ^
      "  fscw_string_buf b;\n" ^
      "  b.v = wrapper_get_string_z(base, ofs, &b.tmpbuf, libname);\n" ^
      "  return b;\n" ^
      "}\n" ^
      "FSCW_INLINE void fscw_string_release(fscw_string_buf b)\n" ^
      "{\n" ^
      "  wrapper_release_tmpbuf(b.tmpbuf);\n" ^
      "}\n" ^
      "#define fscw_string_unwrap_param(pp, libname) fscw_string_get(pp##_base, pp##_ofs, (libname))\n"
    in
    Format.fprintf ppf "%s" code
  | WLTstruct s -> (* TODO *)
    ()
  | _ -> ()

let pp_wlr_autogen ppf ~genv2 name =
  let genv = genv2.genv in
  let abs_ty, ty = c_type_of_varname ~genv2 name in
  match abs_ty.ct_ty with
  | Tfunction (param_ty, false, ret_ty) ->
    let i = ref 0 in
    let ret_wty = wl_type_of_c_type ~genv2 ret_ty in
    let param_wty = Util.list_mapi
      (fun n ty ->
        let wty = wl_type_of_param_c_type ~genv2 ty in
        wty, parse_wl_type ~genv2 wty, n
      ) param_ty
    in
    let original_decl = List.assoc name genv2.original_decls in
    Format.fprintf ppf "/**\n * @@fn @[<h>%a@]\n * @@brief auto generated function.\n * \n * @@author auto generated\n */\n"
                       C_pp.pp_print_declaration (Locterm.locput_dummy original_decl);
    Format.fprintf ppf "%s fscw_%s(@[<hv>%a@])@\n{@\n  @[<hv>%a%a%a%a%a@]}@\n"
      (fscw_T_of_wty ~genv2 ret_wty)
      name
      (* parameter list *)
      (C_pp.pp_print_list
        ~sep_pp:C_pp.pp_print_sep_comma
        ~elem_pp:(fun ppf (wty, _, pid) ->
           Format.fprintf ppf "%s_param(p_%d)" (fscw_T_of_wty ~genv2 wty) pid
        )
      ) param_wty
      (* declare ret *)
      (fun ppf () -> if ret_wty <> WLTvoid then
        let ret_p = parse_wl_type ~genv2 ret_wty in
        Format.fprintf ppf "%s ret;@\n" ret_p.tp_c_t_str) ()
      (* unwrap_param *)
      (C_pp.pp_print_list
        ~sep_pp:ignore
        ~elem_pp:(fun ppf (wty, p, pid) ->
           if p.tp_param_has_unwrap_param then
             Format.fprintf ppf "%s_buf b_%d = %s_unwrap_param(p_%d, \"%s\");@\n"
                                (fscw_T_of_wty ~genv2 wty) pid
                                (fscw_T_of_wty ~genv2 wty) pid name)
      ) param_wty
      (* invoke *)
      (fun ppf () ->
        Format.fprintf ppf "%s%s(@[<hv>%a@]);@\n"
          (if ret_wty = WLTvoid then "" else "ret = ")
          name
          (* arguments *)
          (C_pp.pp_print_list
            ~sep_pp:C_pp.pp_print_sep_comma
            ~elem_pp:(fun ppf (wty, p, pid) ->
              if p.tp_param_has_unwrap_param then
                 Format.fprintf ppf "b_%d.v" pid
              else
                 Format.fprintf ppf "p_%d" pid
            )
          ) param_wty) ()
      (* release buffer *)
      (C_pp.pp_print_list
        ~sep_pp:ignore
        ~elem_pp:(fun ppf (wty, p, pid) ->
           if p.tp_param_has_unwrap_param then
             Format.fprintf ppf "%s_release(b_%d);@\n"
                                (fscw_T_of_wty ~genv2 wty) pid)
      ) param_wty
      (* return *)
      (fun ppf () -> if ret_wty <> WLTvoid then
        Format.fprintf ppf "return %s_wrap(ret);@\n"
                           (fscw_T_of_wty ~genv2 ret_wty)) ()
  | _ -> assert false

let pp_wlr_manifest ppf ~genv2 wty =
  match wty with
    WLTstruct ws -> begin
      if !Transutil.compiler_mode <> Transutil.MultiModule then
	let genv = genv2.genv in
	let id = ws.wl_struct_id in
	let desc = Ctt_abstree.get_struct_desc ~genv id in
	match desc.str_size with
	  Some s ->
            let str =
	      Printf.sprintf "TSC\t%s\t%s\t%s"
		(encoded_name_of_struct ~genv id)
		(Option.default "" ws.wl_struct_tag)
		(String.concat "" 
		   (List.map
		      (function
			  _, NormalField {sf_type=t; sf_id = s} ->
			    string_of_int (String.length s) ^ s ^ "_" ^ (encoded_name_of_type_genv ~genv t) ^ "_"
			| _, BitField _ -> assert false)
		      desc.str_fields)) in
            Format.fprintf ppf "/* manifest entry\n%s\n*/\n\n" str
	| _ -> ()
    end
  | _ -> ()

let pp_wlr ppf ~genv2 req =
  match req with
  | WLR_typedef ty -> pp_wlr_typedef ppf ~genv2 ty
  | WLR_function ty -> pp_wlr_function ppf ~genv2 ty
  | WLR_sp_unwrap ty -> pp_wlr_special_unwrapper ppf ~genv2 ty
  | WLR_wrapper_decl name -> pp_wlr_wrapper_decl ppf ~genv2 name
  | WLR_autogen name -> pp_wlr_autogen ppf ~genv2 name
  | WLR_value _
  | WLR_typeinfo _ -> ()
  | WLR_emit_typeinfo ty -> pp_wlr_manifest ppf ~genv2 ty
  | WLR_primitive_type -> ()
  | WLR_emit_modext _ -> ()
