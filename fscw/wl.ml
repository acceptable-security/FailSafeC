open Ctt_abstree
open Transutil
open Translate_to_il3

type wl_struct = {
  (* struct tag name *)
  wl_struct_tag: string option;
  (* __fsc_attribute__ name *)
  wl_struct_name: string option;
  wl_struct_id: struct_id;
}

(*
 * value of wrapped type
 *  fscw_void, fscw_int, fscw_pointer, ...
 *)
type wl_type =
| WLTvoid
| WLTbuiltin of builtin_type
| WLTpointer
| WLTstruct of wl_struct
| WLTabstract of string
| WLTstring

(*
 * global names.
 *)
type wl_global_decl = 
| WLTypedef of identifier * c_type          (* typedef c_type identifier; *)
| WLDeclVar of identifier * c_type * c_type (* extern  c_type identifier; *)
| WLStruct  of wl_struct                    (* struct tag { ... }; *)

type environment2 = {
  genv: Ctt_abstree.environment;
  decls: wl_global_decl list;
  depends: (identifier * identifier list) list;
  original_decls: (identifier * C_abstree.declaration_desc) list
}

let make_genv2 ge dec dep orig = { genv = ge; decls = dec; depends = dep; original_decls = orig; }

let c_type_of_typename ~genv2 name =
  let rec f x = match x with
    | [] -> failwith ("type name not found: " ^ name)
    | WLTypedef(n, ty)::_ when name = n -> ty
    | _::tl -> f tl in
  f genv2.decls

let c_type_of_varname ~genv2 name =
  let rec f x = match x with
    | [] -> failwith ("variable name not found: " ^ name)
    | WLDeclVar(n, abs_ty, ty)::_ when name = n -> abs_ty, ty
    | _::tl -> f tl in
  f genv2.decls

let wl_struct_of_struct_tag ~genv2 name =
  let rec f x = match x with
    | [] -> failwith ("struct tag not found: " ^ name)
    | WLStruct(s)::_ when s.wl_struct_tag = Some name -> s
    | _::tl -> f tl in
  f genv2.decls

let wl_struct_of_struct_name ~genv2 name =
  let rec f x = match x with
    | [] -> failwith ("struct name not found: " ^ name)
    | WLStruct(s)::_ when s.wl_struct_name = Some name -> s
    | _::tl -> f tl in
  f genv2.decls

let wl_struct_of_struct_id ~genv2 id =
  let rec f x = match x with
    | [] -> raise Not_found
    | WLStruct(s)::_ when s.wl_struct_id = id -> s
    | _::tl -> f tl in
  f genv2.decls

let wl_type_of_c_type ~genv2 ty =
  match ty.ct_ty with
  | Tvoid -> WLTvoid
  | Tbuiltin bt -> WLTbuiltin bt
  | Tpointer base -> begin
    match base.ct_ty with
    | Tbuiltin Tchar -> WLTstring (* TODO: need annotation syntax *)
    | _ -> WLTpointer 
    end
  | Tfunction _ -> assert false (* TODO *)
  | Tarray _ -> assert false (* TODO *)
  | Tstruct id -> WLTstruct (wl_struct_of_struct_id ~genv2 id)
  | Tabstract name -> WLTabstract name

let wl_type_of_param_c_type ~genv2 ty =
  match ty.ct_ty with
  | Tvoid -> WLTvoid
  | Tbuiltin bt -> WLTbuiltin bt
  | Tpointer base -> begin
    match base.ct_ty with
    | Tbuiltin Tchar -> WLTstring (* TODO: need annotation syntax *)
    | _ -> WLTpointer 
    end
  | Tfunction _ -> WLTpointer
  | Tarray _ -> WLTpointer
  | Tstruct id -> WLTstruct (wl_struct_of_struct_id ~genv2 id)
  | Tabstract name -> WLTabstract name

let wl_type_of_typename ~genv2 name =
  wl_type_of_c_type ~genv2 (c_type_of_typename ~genv2 name)

let canonify_c_type_mem t =
  Il0_type_canonify.translate_c_type_mem t

type wl_type_property = {
  tp_wl_type: wl_type;
  tp_fscw_t: c_type; (* Tabstract "fscw_T" *)
  tp_c_t: c_type;    (* T *)
  tp_c_t_str: string; (* str of T *)
  tp_param_is_native: bool;  (* param is native value (i.e. all builtin types) *)
  tp_param_has_unwrap_param: bool; (* fscw_T_unwrap_param is available (i.e. string) *)
  tp_prop: type_properties;
}

let rec parse_wl_type ~genv2 wty =
  let genv = genv2.genv in
  match wty with
  | WLTvoid -> assert false
  | WLTbuiltin bt -> begin
    let name, str_ty, bt' = match bt with
    | Tchar ->     "fscw_char", "char", Tuchar
    | Tschar ->    "fscw_signed_char", "signed char", Tuchar
    | Tuchar ->    "fscw_unsigned_char", "unsigned char", Tuchar
    | Tshort ->    "fscw_short", "short", Tushort
    | Tushort ->   "fscw_unsigned_short", "unsigned short", Tushort
    | Tint ->      "fscw_int", "int", Tuint
    | Tuint ->     "fscw_unsigned_int", "unsigned int", Tuint
    | Tlong ->     "fscw_long", "long", Tulong
    | Tulong ->    "fscw_unsigned_long", "unsigned long", Tulong
    | Tlonglong -> "fscw_long_long", "long long", Tulonglong
    | Tulonglong -> "fscw_unsigned_long_long", "unsigned long long", Tulonglong
    | Tfloat -> "fscw_float", "float", Tfloat
    | Tdouble -> "fscw_double", "double", Tdouble
    | Tlongdouble -> "fscw_long_double", "long double", Tlongdouble
    in
(*    let ty = make_c_type (Tbuiltin bt') in *)
    let ty = canonify_c_type_mem (make_c_type (Tbuiltin bt)) in
    {
      tp_wl_type = wty;
      tp_fscw_t = make_c_type (Tabstract name);
      tp_c_t = make_c_type (Tbuiltin bt);
      tp_c_t_str = str_ty;
      tp_param_is_native = true; (* (short x) or (base_t x_base, int x) *)
      tp_param_has_unwrap_param = false;
      tp_prop = parse_type_genv ~genv ty;
    }
    end
  | WLTpointer ->
    let ty = make_c_type (Tpointer (make_c_type Tvoid)) in
    {
      tp_wl_type = wty;
      tp_fscw_t = make_c_type (Tabstract "fscw_pointer");
      tp_c_t = ty;
      tp_c_t_str = "void *";
      tp_param_is_native = false;
      tp_param_has_unwrap_param = false;
      tp_prop = parse_type_genv ~genv ty;
    }
  | WLTstruct s ->
    let n = match s.wl_struct_tag, s.wl_struct_name with
            | _, Some t -> t
            | Some t, None -> t
            | None, None -> assert false in
    let ty = make_c_type ~s_table:genv.struct_table (Tstruct s.wl_struct_id) in
    {
      tp_wl_type = wty;
      tp_fscw_t = make_c_type (Tabstract ("fscw_struct_" ^ n));
      tp_c_t = ty;
      tp_c_t_str = "";
      tp_param_is_native = false;
      tp_param_has_unwrap_param = false;
      tp_prop = parse_type_genv ~genv ty;
    }
  | WLTabstract name ->
    let ty = c_type_of_typename ~genv2 name in
    let wty' = wl_type_of_c_type ~genv2 ty in (* resolve typedef name *)
    let p' = parse_wl_type ~genv2 wty' in
    {
      tp_wl_type = wty;
      tp_fscw_t = make_c_type (Tabstract ("fscw_" ^ name));
      tp_c_t = ty;
      tp_c_t_str = name;
      tp_param_is_native = p'.tp_param_is_native;
      tp_param_has_unwrap_param = p'.tp_param_has_unwrap_param;
      tp_prop = p'.tp_prop;
    }
  | WLTstring ->
    let ty = make_c_type (Tpointer (make_c_type (Tbuiltin Tchar))) in
    {
      tp_wl_type = wty;
      tp_fscw_t = make_c_type (Tabstract "fscw_string");
      tp_c_t = ty;
      tp_c_t_str = "const char *";
      tp_param_is_native = false;
      tp_param_has_unwrap_param = true;
      tp_prop = parse_type_genv ~genv ty;
    }

let fscw_T_of_prop p =
  match p.tp_fscw_t.ct_ty with
  | Tabstract name -> name
  | _ -> assert false

let fscw_T_of_wty ~genv2 wty =
  match wty with
  | WLTvoid -> "void"
  | _ -> fscw_T_of_prop (parse_wl_type ~genv2 wty)

