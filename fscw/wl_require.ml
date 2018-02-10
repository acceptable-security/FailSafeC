open Wl
open Ctt_abstree
open Record_globalnames

type wl_required =
| WLR_primitive_type
| WLR_typedef  of wl_type (* typedef value fscw_<T> *)
| WLR_function of wl_type (* fscw_<T>_... function and macros *)
| WLR_sp_unwrap of wl_type
| WLR_wrapper_decl of (string * bool)        (* #define fscw_atoi FS_FPc_i_atoi *)
| WLR_value of string    (* GVs for global value *)
| WLR_autogen  of string             (* auto generate function *)
| WLR_typeinfo of wl_type
| WLR_emit_typeinfo of wl_type
| WLR_emit_modext of string

(*
 * x depends on (wl_depends x)
 *)
let wl_depends ~genv2 x =
  match x with
  | WLR_typedef ty -> begin
      let d = [ WLR_typeinfo ty ] in
      match ty with
      | WLTabstract name ->
        let ty2 = wl_type_of_typename ~genv2 name in
        WLR_typedef ty2 :: d
      | _ -> d
    end
  | WLR_function ty -> [ WLR_typedef ty ]
  | WLR_sp_unwrap ty -> [ WLR_function ty; WLR_typeinfo ty ]
  | WLR_wrapper_decl (name, decl_p) -> begin
      let abs_ty, ty = c_type_of_varname ~genv2 name in
      match abs_ty.ct_ty with
      | Tfunction (param_ty, is_var, ret_ty) ->
        let lis1 = WLR_function (wl_type_of_c_type ~genv2 ret_ty)::
                    List.map
                      (fun p_ty -> WLR_sp_unwrap (wl_type_of_param_c_type ~genv2 p_ty))
                    param_ty
        in
        let dep = List.assoc name genv2.depends in
        let lis2 = List.map
                    (fun dname -> WLR_sp_unwrap(WLTabstract dname))
                    dep
        in
        let lis = WLR_primitive_type::lis1@lis2 in
        if is_var then (WLR_function WLTpointer)::lis else lis
      | _ -> Util.failwith_p "%s is not function" name
    end
  | WLR_autogen name -> [ WLR_wrapper_decl (name, true) ]
  | WLR_typeinfo ty -> []
  | WLR_emit_typeinfo ty -> [ WLR_typeinfo ty ]
  | WLR_value _ (* handled in ctt tree *)
  | WLR_emit_modext _ -> []
  | WLR_primitive_type -> [
    WLR_function (WLTbuiltin Tchar);
    WLR_function (WLTbuiltin Tschar);
    WLR_function (WLTbuiltin Tuchar);
    WLR_function (WLTbuiltin Tshort);
    WLR_function (WLTbuiltin Tushort);
    WLR_function (WLTbuiltin Tint);
    WLR_function (WLTbuiltin Tuint);
    WLR_function (WLTbuiltin Tlong);
    WLR_function (WLTbuiltin Tulong);
    WLR_function (WLTbuiltin Tlonglong);
    WLR_function (WLTbuiltin Tulonglong);
    WLR_function (WLTbuiltin Tfloat);
    WLR_function (WLTbuiltin Tdouble);
    (* WLR_function (WLTbuiltin Tlongdouble); TODO *)
    WLR_function (WLTpointer);
    WLR_function (WLTstring);
    WLR_sp_unwrap (WLTstring)
  ]

let resolve_required ~genv2 required_list =
  Dep.flatten ~depends:(wl_depends ~genv2) ~roots:required_list

