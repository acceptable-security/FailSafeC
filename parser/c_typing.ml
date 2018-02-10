(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2001 -- 2006. *)

(**** untyped tree -> typed tree ****)

open Locterm
open Big_int_infix
open C_abstree
open Ctt_abstree
open Fsc_config
open Printf
(* open ExtList *)
open Util

open Parse_const

let dprintf l f = Debug.dprintf ~category:11 l f
let get_debug_flags () = Debug.get_debug_flags ~category:11

(** change gbind_type's type from c_type to variableType.
    related structures and functions are redefined. **)

module BigInt_Ht = Hashtbl.Make (struct type t = big_int let equal = (==!) let hash = Hashtbl.hash end)

type variableType = EnumVal of big_int | Var of c_type | TypeDefName of c_type

type local_binding = {
    lbind_new_name : identifier;
    lbind_storage_class : local_storage_class;
    lbind_type : variableType;
  }

type local_bind_frame = (identifier * local_binding) list

type global_binding = {
    gbind_storage_class : global_storage_class;
    gbind_is_initialized : bool;
    gbind_type : variableType;
    gbind_loc : location;
  }

type struct_name_entry = EnumName | StructName of struct_id

type environment = {
    module_name : string;
    mutable var_seqno : int;
    mutable struct_counter : int;
    global_binding : (identifier, global_binding) Hashtbl.t;
    mutable struct_table : struct_desc earray;
    mutable struct_name_table : (identifier * struct_name_entry) list;
    mutable local_binding : local_bind_frame list;
    mutable local_struct_name_table : (identifier * struct_name_entry) list list;

    mutable break_allowed : bool;
    mutable continue_allowed : bool;
    mutable goto_labels : (identifier, bool) Hashtbl.t;
    mutable switch_frames : (c_type * bool ref * unit BigInt_Ht.t) list;
  }

let make_c_type ~env = 
  Ctt_abstree.make_c_type ~s_table:env.struct_table
    
let update_c_type ~env = 
  Ctt_abstree.update_c_type ~s_table:env.struct_table

let make_c_type_ql ~env ql ty = 
  let rec iter const volatile = function
      [] -> make_c_type ~env ~const ~volatile ty
    | Const :: tl ->
	if const then failwith "const appeared twice"
	else iter true volatile tl
    | Volatile :: tl ->
	if volatile then failwith "volatile appeared twice"
	else iter const true tl
	
  in iter false false ql

let remove_qualifier t = { t with ct_volatile_p = false; ct_const_p = false }

let get_struct_desc ~env id = 
  try 
    Earray.get env.struct_table id
  with
    Not_found -> assert false

let fold_constants = Reduce.reduce_expression

exception TypeError_untyped of C_abstree.expr * string
exception TypeError_typed of expr * string
exception TypeError_local of string
exception TypeError_located of location * string

let failwith_loc ~loc s = raise (TypeError_located (loc, s))

exception NotConstant = Reduce.NotConstant

(*let make_expr_p e ty ~orig = 
  make_expr e ty ~loc:(loc_of_p orig)*)

(* let make_expr_t e ty ~orig = 
  Locterm.loccopy ~orig (make_texpr_desc e ty) *)

let exp_constant_zero = 
  let d = (CTTexpConstant (CTTconstInteger zero_big_int)) in
  fun ~loc -> make_expr d type_int loc

let exp_constant_one = 
  let d = (CTTexpConstant (CTTconstInteger unit_big_int)) in
  fun ~loc -> make_expr d type_int loc

let exp_constant_one_ptrdiff_t = 
  let d = (CTTexpConstant (CTTconstInteger unit_big_int)) in
  fun ~loc -> make_expr d type_ptrdiff_t loc 

let make_new_struct_id ~env = 
  env.struct_counter <- env.struct_counter + 1; 
  env.struct_counter

let rec get_field_type_from_desc id = 
  function
      [] -> raise (TypeError_local ("field "^id^" not exist"))
    | (_, NormalField f) :: rest ->
	if f.sf_id = id then f.sf_type
	else get_field_type_from_desc id rest
    | (_, BitField f) :: rest ->
	let rec srch = function
	    [] -> get_field_type_from_desc id rest
	  | (Some s, t, _, _)::_ when s = id -> t
	  | _ :: rest -> srch rest
	in
	srch f.s_bf_fields

let get_field_type ~env t1 ident = 
  match t1.ct_ty with
    Tstruct id ->
      let d = get_struct_desc ~env id in
      get_field_type_from_desc ident d.str_fields
  | _ -> raise (TypeError_local "not a struct")
    
let rec lookup_var_type_from_local frames (id : identifier) =
  match frames with
    [] -> raise Not_found
  | hd::tl ->
      try
	let a = (List.assoc id hd) in
	(a.lbind_new_name, a.lbind_type)
      with
	Not_found -> lookup_var_type_from_local tl id

let rec lookup_var_type_from_global ~env id =
  let bind = Hashtbl.find env.global_binding id in
  (id, bind.gbind_type)
  
let lookup_var_type ~env (id : identifier) = 
  try
    lookup_var_type_from_local env.local_binding id
  with
    Not_found ->
	lookup_var_type_from_global ~env id

let translate_external_extension_default ~loc ~ext ~sclass ~ty ~is_initialized = 
  (failwith "extension is not supported" : extension_list)

let translate_external_extension_hook = ref translate_external_extension_default

let merge_value_extensions_hook = ref 
    (fun ~(old_ext : extension_list) ~(new_ext : extension_list) ~(have_definition : bool) ->
      if old_ext <> [] || new_ext <> [] then failwith "extension is not supported"
      else ([] : extension_list))

let merge_struct_extensions_hook = ref 
    (fun ~(old_ext : extension_list) ~(new_ext : extension_list) ->
      if old_ext <> [] || new_ext <> [] then failwith "extension is not supported"
      else ([] : extension_list))

let translate_external_extension ~loc ~ext ~sclass ~ty ~is_initialized =
  try
    (!translate_external_extension_hook) ~loc ~ext ~sclass ~ty ~is_initialized
  with
    Failure s ->
      raise (TypeError_local s)

let alpha_convert_use_prefixed_name = ref true

let alpha_convert ~env id =
   env.var_seqno <- env.var_seqno + 1;
   let nid = 
   if !alpha_convert_use_prefixed_name then 
     string_of_int env.var_seqno ^ id
   else
     id ^ "__" ^ string_of_int env.var_seqno
   in
   nid

exception Duplicate_definition

let rec merge_c_types ~env t1 t2 = 
  let desc = 
    match t1.ct_ty, t2.ct_ty with
      Tvoid, Tvoid -> Tvoid
    | Tbuiltin bt1, Tbuiltin bt2 when bt1 = bt2 -> t1.ct_ty
    | Tpointer t1, Tpointer t2 -> Tpointer (merge_c_types ~env t1 t2)
    | Tfunction(at1,v1,rt1), Tfunction(at2,v2,rt2) ->
	let l1 = List.length at1
	and l2 = List.length at2
	in
	if (not v1 && l1 < l2 || not v2 && l2 < l1) then
	  raise Exit
	else begin
	  let rt = merge_c_types ~env rt1 rt2 in
	  let at = 
	    let matched_length = min l1 l2 in
	    let m1, r1 = Util.split_at_nth matched_length at1 in
	    let m2, r2 = Util.split_at_nth matched_length at2 in
	    assert (r1 = [] || r2 = []);
	    List.map2 (merge_c_types ~env) m1 m2 @ r1 @ r2
	  in
	  let v = v1 && v2 in
	  Tfunction(at, v, rt)
	end
    | Tarray(t1,None), Tarray(t2,size) 
    | Tarray(t1,size), Tarray(t2,None) ->
	Tarray(merge_c_types ~env t1 t2, size)
    | Tarray(t1,Some s1), Tarray(t2, Some s2) when s1 ==! s2 ->
	Tarray(merge_c_types ~env t1 t2, Some s1)
    | Tstruct id1, Tstruct id2 when id1 = id2 ->
	Tstruct id1
    | _ -> 
	raise Exit
  in
  if (t1.ct_volatile_p <> t2.ct_volatile_p
    || t1.ct_const_p <> t2.ct_const_p)
  then
    raise Exit
  else
    Ctt_abstree.make_c_type 
      ~const:t1.ct_const_p ~volatile:t1.ct_volatile_p
      ~s_table:env.struct_table desc

let merge_c_types ~env ~name t1 t2 = 
  try
    merge_c_types ~env t1 t2
  with
    Exit ->
      failwith_p 
	"conflicting type in duplicated definitions: %s:@ %a@ <--> %a"
	name Ctt_formatter.pp_c_type t1 Ctt_formatter.pp_c_type t2

let merge_global_binding ~env ~name ~new_gb ~old_gb =
  match new_gb.gbind_type, old_gb.gbind_type with
    Var new_typ, Var old_typ -> begin
      try begin
	let merged_typ = merge_c_types ~env ~name new_typ old_typ in
	let new_sclass, keep_newer_initialization = 
	  match new_gb.gbind_is_initialized, old_gb.gbind_is_initialized with
	    true, true -> raise Duplicate_definition
	  | false, false -> begin
	      match new_gb.gbind_storage_class, old_gb.gbind_storage_class with
		ModuleStatic, (Extern [] | Global [] | ModuleStatic) |
		(Extern [] | Global []), ModuleStatic -> ModuleStatic, true
	      | Global [], (Extern [] | Global []) | Extern [], Global [] -> Global [], true
	      | Extern [] , Extern [] -> Extern [], true
	      | Extern new_ext, Extern old_ext ->
		  Extern ((!merge_value_extensions_hook) ~new_ext ~old_ext ~have_definition:true), true
	      | Global new_ext, Extern old_ext ->
		  let merged_ext = 
		    (!merge_value_extensions_hook) ~new_ext ~old_ext ~have_definition:true
		  in 
		  Global merged_ext, true
	      | _ -> raise Duplicate_definition
	  end
	  | false, true -> begin
	      match new_gb.gbind_storage_class, old_gb.gbind_storage_class with
	      | _, Extern _ -> assert false
	      | (Extern [] | ModuleStatic), ModuleStatic -> ModuleStatic, false
	      | Global _ , ModuleStatic | ModuleStatic, Global _ -> raise Duplicate_definition
	      | (Extern [] | Global []), Global [] -> Global [], false
	      | Global _, Global _
	      | Extern _, _ -> raise Duplicate_definition
	  end
	  | true, false -> begin
	      match new_gb.gbind_storage_class, old_gb.gbind_storage_class with
		ModuleStatic, (ModuleStatic | Extern []) | Global [], ModuleStatic -> ModuleStatic, true
	      | (Extern _), ModuleStatic | ModuleStatic, Global _ -> raise Duplicate_definition
	      | (Extern [] | Global []), (Global [] | Extern []) -> Global [], true
	      | Extern new_ext, Extern old_ext ->
		  Extern ((!merge_value_extensions_hook) ~new_ext ~old_ext ~have_definition:false), true
	      | Global new_ext, Extern old_ext ->
		  let merged_ext = 
		    (!merge_value_extensions_hook) ~new_ext ~old_ext ~have_definition:true
		  in 
		  Global merged_ext, true
	      | Extern [], Global (_::_)
	      | Global (_::_), (Global _|ModuleStatic)
	      | (Global [], Global (_::_))
	      | Extern (_::_), _ | _, Extern (_::_) -> raise Duplicate_definition
	  end
	in
	{ gbind_type = Var merged_typ;
	  gbind_storage_class = new_sclass;
	  gbind_is_initialized = new_gb.gbind_is_initialized || old_gb.gbind_is_initialized; 
          gbind_loc = if keep_newer_initialization then new_gb.gbind_loc else old_gb.gbind_loc },
	keep_newer_initialization
      end
      with
	Duplicate_definition ->
	  failwith ("conflicting definition: " ^ name)
    end
  | _ -> failwith ("conflicting definition kind: " ^ name)


let add_global_binding_to_env ~env ~loc id sclass ~is_initialized ty = 
  let gb = {
    gbind_type = ty; gbind_storage_class = sclass;
    gbind_is_initialized = is_initialized;
    gbind_loc = loc
  } in
  if Hashtbl.mem env.global_binding id then
    let gb, keep_newer_initialization =
      merge_global_binding ~env ~name:id ~new_gb:gb ~old_gb:(Hashtbl.find env.global_binding id) in
    Hashtbl.replace env.global_binding id gb;
    id, keep_newer_initialization
  else
    let gb =
      match gb.gbind_storage_class with
	Extern [] when gb.gbind_is_initialized ->
	  { gb with gbind_storage_class = Global [] }
      | Extern ext when gb.gbind_is_initialized ->
	  { gb with gbind_storage_class = Global [] }
      | Extern ((_::_) as _ext) ->
	  gb
      | _ -> gb
    in
    Hashtbl.add env.global_binding id gb;
    id, true
  
let add_local_binding_to_env ~env id sclass ty = 
  let nid = alpha_convert ~env id in
  let lb = { lbind_type = ty; lbind_storage_class = sclass; lbind_new_name = nid } in
  let local_frame, lower_frame = List.hd env.local_binding, List.tl env.local_binding in
  if List.mem_assoc id local_frame then begin
    failwith ("duplicate definition of local variable " ^ id)
  end;
  let local_frame = (id, lb) :: local_frame in
  env.local_binding <- local_frame :: lower_frame;
  nid, true

let update_local_binding ~env id oldty newty = 
  let rec iter = function
      [] -> failwith ("update_local_binding: panic: cannot find " ^ id)
    | (id',({lbind_type = (Var ty) } as r))::tl when id' = id ->
	if oldty == ty then
	  (id',{ r with lbind_type = (Var newty) })::tl
	else
	  failwith "update_local_binding: panic: type does not match (phisical equivalence required)"
    | hd::tl -> hd::iter tl
  in
  match env.local_binding with
    hd::rest ->
      env.local_binding <- iter hd :: rest
  | _ ->
      failwith "update_local_binding: panic: empty environment"

let extend_local_frame_env ~env = 
  env.local_binding <- [] :: env.local_binding;
  env.local_struct_name_table <- [] :: env.local_struct_name_table

let shrink_local_frame_env ~env = 
  env.local_binding <- List.tl env.local_binding;
  env.local_struct_name_table <- List.tl env.local_struct_name_table

(* compare two types exactly.
   ?check_qual = true  --> const int != int
   ?check_qual = false --> const int == int

   int () != int ( * )()
   int[3] != int *
 *)

let pointers_ignore_signedness = ref true

let qual_eq t1 t2 = 
  (t1.ct_const_p = t2.ct_const_p && t1.ct_volatile_p = t2.ct_volatile_p)
let qual_gt t1 t2 = 
  ((not t1.ct_const_p || t2.ct_const_p) && (not t1.ct_volatile_p || t2.ct_volatile_p))
let qual_lt t1 t2 = 
  ((t1.ct_const_p || not t2.ct_const_p) && (t1.ct_volatile_p || not t2.ct_volatile_p))
let qual_dcare t1 t2 = 
  true

let equal_builtin_type_except_typedness bt1 bt2 = 
  match bt1, bt2 with
    (Tchar | Tschar | Tuchar | Tshort | Tushort | Tint | Tuint | Tlong | Tulong | Tlonglong | Tulonglong),
    (Tchar | Tschar | Tuchar | Tshort | Tushort | Tint | Tuint | Tlong | Tulong | Tlonglong | Tulonglong)
  | (Tfloat | Tdouble | Tlongdouble), 
    (Tfloat | Tdouble | Tlongdouble) ->
      size_of_builtin_type bt1 = size_of_builtin_type bt2
  | _, _ -> false

let rec equal_type ?(check_qual = qual_dcare) ?(check_iqual = qual_dcare) ?(ignore_signeness = false)
    ?(pointers_ignore_signedness = false) t1 t2 = 
  if not (check_qual t1 t2) then false else
  match t1.ct_ty, t2.ct_ty with
    Tvoid, Tvoid -> true
  | Tbuiltin bt1, Tbuiltin bt2 ->
      if ignore_signeness then
	equal_builtin_type_except_typedness bt1 bt2
      else
	bt1 = bt2
  | Tpointer pt1, Tpointer pt2 ->
      equal_type ~check_qual:check_iqual ~check_iqual ~ignore_signeness:(pointers_ignore_signedness) pt1 pt2
  | Tfunction (pt1,var1,rt1), Tfunction(pt2,var2,rt2) ->
      equal_type ~check_qual:qual_eq ~check_iqual:qual_eq rt1 rt2 &&
      var1 = var2 &&
      List.length pt1 = List.length pt2 &&
      List.for_all2 (equal_type ~check_qual:qual_dcare ~check_iqual:qual_eq) pt1 pt2
  | Tarray(et1,Some sz1), Tarray(et2,Some sz2) ->
      sz1 ==! sz2 && equal_type ~check_qual ~check_iqual ~ignore_signeness:(pointers_ignore_signedness) et1 et2
  | Tarray(et1,None), Tarray(et2,None) ->
      equal_type ~check_qual ~check_iqual ~ignore_signeness:(pointers_ignore_signedness) et1 et2
  | Tstruct(s1), Tstruct(s2) -> 
      s1 = s2 (* name-based equivalence *)
  | Tabstract(s1), Tabstract(s2) -> s1 = s2
  | _ -> false

let type_of exp = (locval exp).expr_type

let rec is_constant_zero e1 = 
  match (locval e1).expr_t with
    CTTexpConstant (CTTconstInteger x) ->
      x ==! zero_big_int
  | CTTexpConstant CTTconstNull ->
      true
  | CTTexpCoerce (t,e) ->
      is_constant_zero e
  | _ -> false

let is_constant_null = is_constant_zero

let type_is_numeric typ =
  match typ.ct_ty with
    Tbuiltin _ -> true
  | _ -> false

let is_numeric exp = 
  match (type_of exp).ct_ty with
    Tbuiltin _ -> true
  | _ -> false

let is_pointer_without_null_rule exp = 
  match (type_of exp).ct_ty with
    Tpointer _ -> true
  | _ -> false

let is_pointer_or_null exp = 
  match (type_of exp).ct_ty with
    Tpointer _ -> true
  | Tbuiltin _ -> 
      is_constant_zero exp
  | _ -> false

let is_void_pointer exp = 
  match (type_of exp).ct_ty with
    Tpointer { ct_ty = Tvoid } -> true
  | _ -> false

(* make pointer from pointer-capable types *)
(* int ()(double) -> int ( * )(double) , int[3] -> int * *)
(* no change for numeric types *)
let coerce_implicit_pointer ~env (exp : expr) =
  let loc = locget exp in
  let make_expr = make_expr ~loc in
  let ty = (type_of exp) in
  match ty.ct_ty with
    Tbuiltin _ | Tpointer _ | Tvoid -> exp
  | Tfunction _ ->
      make_expr (CTTexpAddress(exp)) (make_c_type ~env (Tpointer ty))
  | Tarray(ety,sz) ->
      let ty = make_c_type ~env (Tpointer ety) in
      make_expr (CTTexpCoerce(ty,exp)) ty
  | Tstruct _ -> exp
  | Tabstract _ -> assert false

let make_null_pointer t ~loc = 
  make_expr ~loc (CTTexpConstant CTTconstNull) t

let rec cast_coerce ~env ?(l = false) e1 t_target = 
  let loc = locget e1 in
  let make_expr = make_expr ~loc in
  let t1 = type_of e1 in
  if equal_type t1 t_target then
    (* example: ( const char * ) -> ( char * ) *)
    make_expr (locval e1).expr_t t_target
  else begin
    let ec = make_expr (CTTexpCoerce(t_target,e1)) t_target in
    match t1.ct_ty, t_target.ct_ty with
      _, Tvoid -> ec
    | Tbuiltin _, Tbuiltin _ -> ec
    | Tbuiltin _, Tpointer _ ->
	if is_constant_zero e1 then
	  make_null_pointer t_target ~loc
	else
	  ec
    | Tpointer _, Tbuiltin _ -> ec
    | Tpointer _, Tpointer _ ->
	if is_constant_null e1 then
	  make_null_pointer t_target ~loc
	else
	  ec
    | (Tarray _ | Tfunction _), _ ->
	assert (not l);
	cast_coerce ~env ~l:true (coerce_implicit_pointer ~env e1) t_target
    | _ ->
	raise 
	  (TypeError_typed
	     (e1,
	      (sfprintf "cast failed to %a" Ctt_formatter.pp_c_type t_target)))
  end

let cast_coerce ~env e1 t = 
  let e1 = cast_coerce ~env e1 t in
  try
    fold_constants e1
  with
    NotConstant -> e1

let subset_qualifiers ~sub ~sup =
  (sup.ct_const_p || not sub.ct_const_p)
    && (sup.ct_volatile_p || not sub.ct_volatile_p)

let rec assign_coerce ~env ?(check_qual=qual_lt) ?(struct_allowed=true) e1 t_target = 
  let loc = locget e1 in
  let make_expr = make_expr ~loc in
  let t1 = type_of e1 in
  try
    if equal_type ~check_qual ~check_iqual:qual_gt t1 t_target then
      match t1.ct_ty with
	Tvoid | Tbuiltin _ | Tpointer _ ->
	  e1
      | Tstruct _ when struct_allowed ->
	  e1
      | Tarray _ ->
	  raise (TypeError_local "array value cannot be assigned")
      | Tstruct _ ->
	  raise (TypeError_local "struct value cannot be assigned here")
      | Tfunction _ ->
	  assert false
      | Tabstract _ ->
	  assert false
    else begin
      let t_target = remove_qualifier t_target in
      let ec = make_expr (CTTexpCoerce(t_target,e1)) t_target in
      match t1.ct_ty, t_target.ct_ty with
      | Tbuiltin _, Tbuiltin _ -> ec
      | Tbuiltin _, Tpointer _ when is_constant_zero e1 ->
	  make_null_pointer t_target ~loc
      | Tpointer t1, Tpointer t_target_pt ->
	  if not (qual_gt t1 t_target_pt) then
	    raise (TypeError_local "incompatible type: qualifier mismatch");
	  if is_constant_null e1 then
	    make_null_pointer t_target ~loc
	  else
	    if t1.ct_ty = Tvoid || t_target_pt.ct_ty = Tvoid ||
	    equal_type
	      ~ignore_signeness:!pointers_ignore_signedness
	      ~pointers_ignore_signedness:!pointers_ignore_signedness
	      ~check_qual ~check_iqual:qual_gt t1 t_target_pt
	    then
	      ec
	    else
	      raise (TypeError_local "incompatible type: type mismatch");
      | (Tarray _ | Tfunction _), Tpointer _ ->
	  assign_coerce ~env ~check_qual ~struct_allowed (coerce_implicit_pointer ~env e1) t_target
      | _ -> raise 
	    (TypeError_typed
	       (e1,
		(sfprintf "assign coercion failed (%a -> %a)" 
		   Ctt_formatter.pp_c_type t1
		   Ctt_formatter.pp_c_type t_target)))
    end
  with
    TypeError_local s ->
      raise 
	(TypeError_typed
	   (e1, sfprintf ":: converting %a -> %a"  Ctt_formatter.pp_c_type t1 Ctt_formatter.pp_c_type t_target))

let assign_coerce ~env ?check_qual ?struct_allowed e1 t = 
  let e1 = assign_coerce ~env ?check_qual ?struct_allowed e1 t in
  try
    fold_constants e1
  with
    NotConstant -> e1

let rec arithmetic_coerce e1 t_target =
  let loc = locget e1 in
  let make_expr = make_expr ~loc in
  match type_of e1, t_target with
    ({ct_ty = Tbuiltin bt1}), ({ct_ty = Tbuiltin bt2}) ->
      begin
	match bt1, bt2 with
	  (Tchar | Tuchar | Tschar), _ -> ()
	| (Tshort | Tushort), 
	    (Tshort | Tushort | Tint | Tuint |
	    Tlong | Tulong | Tlonglong | Tulonglong | Tfloat | Tdouble | Tlongdouble) -> ()
	| (Tint | Tuint),
	    (Tint | Tuint | Tlong | Tulong | 
	    Tlonglong | Tulonglong | 
	    Tfloat | Tdouble | Tlongdouble) -> ()
	| (Tlong | Tulong),
	    (Tlong | Tulong | Tlonglong | Tulonglong | Tfloat | Tdouble | Tlongdouble) -> ()
	| (Tlong | Tulong),
	    (Tint | Tuint) when Fsc_config.sizeof_int = Fsc_config.sizeof_long -> ()
		(* TODO: use size information even for other cases *)
	| (Tlonglong | Tulonglong),
	    (Tlonglong | Tulonglong | Tfloat | Tdouble | Tlongdouble) -> ()
	| Tfloat, (Tfloat | Tdouble | Tlongdouble) -> ()
	| Tdouble, (Tdouble | Tlongdouble) -> ()
	| Tlongdouble, Tlongdouble -> ()
	| _ -> 
	    raise
	      (TypeError_local 
		 (sfprintf "incompatible type: invalid implicit arithmetic conversion from %a to %a"
		    Ctt_formatter.pp_builtin_type bt1
		    Ctt_formatter.pp_builtin_type bt2))
      end;
      if bt1 = bt2 then e1
      else
	make_expr (CTTexpCoerce(t_target,e1)) t_target
  | _ ->
      raise (TypeError_local "incompatible type: numeric expected")

let arithmetic_coerce e1 t = 
  let e1 = arithmetic_coerce e1 t in
  try
    fold_constants e1
  with
    NotConstant -> e1

let ensure_same_type ?check_qual ?check_iqual (e1 : expr) (e2 : expr) = 
  if equal_type ?check_qual ?check_iqual (type_of e1) (type_of e2) then ()
  else raise (TypeError_local "not a same type")
     
let coerce_merge_numeric ~env (e1 : expr) (e2 : expr) =
  match type_of e1, type_of e2 with
    {ct_ty = Tbuiltin bt1}, {ct_ty = Tbuiltin bt2} ->
      let merged_type =
	match bt1, bt2 with
	  Tlongdouble, _ | _, Tlongdouble -> Tlongdouble
	| Tdouble, _ | _, Tdouble -> Tdouble
	| Tfloat, _ | _, Tfloat -> Tfloat
	| Tulonglong, _ | _, Tulonglong -> Tulonglong
	| (Tlonglong, _ | _, Tlonglong) when sizeof_longlong > sizeof_long -> Tlonglong
	| (Tulong, _ | _, Tulong) -> Tulong
	| (Tlonglong, _ | _, Tlonglong) when sizeof_longlong > sizeof_int -> Tlonglong
	| (Tlong, _ | _, Tlong) when sizeof_long > sizeof_int -> Tlong
	| Tuint, _ | _, Tuint -> Tuint
	| Tlonglong, _ | _, Tlonglong -> Tlonglong
	| Tlong, _ | _, Tlong -> Tlong
	| (Tushort, _ | _, Tushort) when sizeof_short = sizeof_int -> Tuint
	| (Tuchar, _ | _, Tuchar) when sizeof_char = sizeof_int -> Tuint
	| (Tchar, _ | _, Tchar) when sizeof_char = sizeof_int && char_is_signed -> Tuint
	| Tint, _ | _, Tint -> Tint
	| _ -> Tint
      in
      let target_type =
	make_c_type ~env (Tbuiltin merged_type)
      in
      let e1 = arithmetic_coerce e1 target_type in
      let e2 = arithmetic_coerce e2 target_type in
      e1, e2
  | {ct_ty = Tbuiltin bt1}, _ ->
      raise (TypeError_typed(e2, "not a numeric"))
  | _ ->
      raise (TypeError_typed(e1, "not a numeric"))

type cmp_which = 
    Neither | Only1 | Only2 | Both

let coerce_merge_pointer ~env (e1 : expr) (e2 : expr) =
  (* zero constants (integer) -> any pointer, any pointer -> void *. *)
  (* otherwise, types must be equal *)
  let f f = 
    match f e1, f e2 with
      true, true -> Both
    | true, false -> Only1
    | false, true -> Only2
    | false, false -> Neither
  in
  let to_be_cast = match
    f is_pointer_without_null_rule,
    f is_void_pointer with
    Both, Neither | Both, Both ->
      Neither
  | Both, Only1 ->
      Only1
  | Both, Only2 ->
      Only2
  | Only1, _ ->
      Only2
  | Only2, _ ->
      Only1
  | Neither, Both ->
      Neither
  | Neither, Only1 ->
      Only2
  | Neither, Only2 ->
      Only1
  | Neither, Neither ->
      assert false (* both are numeric *)
  in
  match to_be_cast, f is_numeric with
    Neither, Neither ->
      ensure_same_type e1 e2;
      e1, e2
  | Only1, Neither ->
      let e1 = assign_coerce ~env e1 (type_of e2) in
      e1, e2
  | Only1, Only1 ->
      assert (is_constant_zero e1);
      let e1 = make_null_pointer (type_of e2) ~loc:(locget e1) in
      e1, e2
  | Only2, Neither ->
      let e2 = assign_coerce ~env e2 (type_of e1) in
      e1, e2
  | Only2, Only2 ->
      assert (is_constant_zero e2);
      let e2 = make_null_pointer (type_of e1) ~loc:(locget e2) in
      e1, e2
  | _ ->
      dprintf 0
	"coerce_merge_pointer failed: is_ptr %d, is_vptr %d, is_num %d, deci %d"
	(Obj.magic (f is_pointer_without_null_rule) : int)
	(Obj.magic (f is_void_pointer) : int)
	(Obj.magic (f is_numeric) : int)
	(Obj.magic to_be_cast : int);
	assert false
      
(* merge types of two expressions. used in "? :" expression (arg2 <--> arg3) *)
(* CAVEAT: "qualifier propagation" rule ignored because it's not an lvalue *)
let coerce_merge ~env (e1 : expr) (e2 : expr) =
  let e1, e2 = coerce_implicit_pointer ~env e1, coerce_implicit_pointer ~env e2 in
  let t1, t2 = type_of e1, type_of e2 in
  if equal_type t1 t2 then
    e1, e2
  else if is_numeric e1 && is_numeric e2 then
    coerce_merge_numeric ~env e1 e2
  else if is_pointer_or_null e1 && is_pointer_or_null e2 then
    coerce_merge_pointer ~env e1 e2
  else
    raise (TypeError_local("type merge failed"))

let rec size_of_type ~(env:environment) t = (update_c_type ~env t).ct_size

let rec align_of_type ~(env:environment) t = (update_c_type ~env t).ct_align

let is_sized_pointer ~env (exp : expr) = 
  match (type_of exp).ct_ty with
  | Tvoid -> false
  | Tbuiltin _ -> false
  | Tstruct _ -> false
  | Tarray _ | Tfunction _ -> assert false (* should be pointer *)
  | Tpointer t ->
      if size_of_type ~env t = None then false else true
  | Tabstract(_) -> assert false

let merge_quals t ~from = 
  { t with
    ct_volatile_p = t.ct_volatile_p || from.ct_volatile_p;
    ct_const_p = t.ct_const_p || from.ct_const_p;
  }    

let rec is_type_assignable ~env t = 
  if t.ct_const_p then
    false
  else match t.ct_ty with
  | Tvoid -> false
  | Tbuiltin _ -> true
  | Tpointer _ -> true
  | Tarray(t, _sz) -> is_type_assignable ~env t
  | Tfunction _ -> false
  | Tabstract _ -> assert false
  | Tstruct s -> 
      (get_struct_desc ~env s).str_assignable

let check_assignable ~env e = 
  let t = type_of e in
  if not (is_type_assignable ~env t) then
    raise (TypeError_local "this lvalue is not assignable")

let ensure_numeric (e1 : expr) = 
  if is_numeric e1 then () else raise (TypeError_typed(e1,"not a numeric"))

let ensure_not_void (e1 : expr) = 
  if (type_of e1).ct_ty <> Tvoid then ()
  else raise (TypeError_typed(e1,"not a numeric"))

let ensure_integer e1 = 
  match (type_of e1).ct_ty with
    Tbuiltin
      (Tchar | Tschar | Tuchar 
      | Tshort | Tushort 
      | Tint | Tuint
      | Tlong | Tulong
      | Tlonglong | Tulonglong) -> ()
  | _ -> raise (TypeError_typed(e1,"not an integer"))

let ensure_sized_pointer ~env e1 =
  if is_sized_pointer ~env e1 then () else 
  raise (TypeError_typed(e1,"size not known"))

let coerce_to_boolean ~env (e1 : expr) = 
  let e1 = coerce_implicit_pointer ~env e1 in
  if not (is_numeric e1) && not (is_pointer_without_null_rule e1) then
    raise (TypeError_typed(e1,"not a boolean value"))
  else
    e1

let coerce_at_least_integer e1 = 
  match (type_of e1).ct_ty with
    Tbuiltin
      ((Tchar | Tschar | Tuchar | Tshort | Tushort) as t) ->
	let target_t = 
	  match t with
	  | Tushort when sizeof_short = sizeof_int -> type_unsigned_int
	  | Tuchar when sizeof_char = sizeof_int -> type_unsigned_int
	  | Tchar when sizeof_char = sizeof_int && not char_is_signed -> type_unsigned_int
	  | _ -> type_int
	in
	arithmetic_coerce e1 target_t
  | Tbuiltin
      ( Tint | Tuint
      | Tlong | Tulong
      | Tlonglong | Tulonglong
      | Tfloat | Tdouble | Tlongdouble) -> e1
  | _ -> raise (TypeError_typed(e1,"not a numeric"))

let promote_KandR ~env e1 = 
  let e1 = coerce_implicit_pointer ~env e1 in
  match (type_of e1).ct_ty with
    Tbuiltin
      ((Tchar | Tschar | Tuchar | Tshort | Tushort) as t) ->
	let target_t = 
	  match t with
	  | Tushort when sizeof_short = sizeof_int -> type_unsigned_int
	  | Tuchar when sizeof_char = sizeof_int -> type_unsigned_int
	  | Tchar when sizeof_char = sizeof_int && not char_is_signed -> type_unsigned_int
	  | _ -> type_int
	in
	arithmetic_coerce e1 target_t
  | Tbuiltin Tfloat ->
      arithmetic_coerce e1 type_double
  | _ -> e1

let pointer_offset_coerce e1 t = 
  let loc = locget e1 in
  let make_expr = make_expr ~loc in
  match type_of e1, t with
    ({ct_ty = Tbuiltin bt1}), ({ct_ty = Tbuiltin bt2}) ->
      if bt1 = bt2 then e1 else begin
	ensure_integer e1;
	make_expr (CTTexpCoerce(t,e1)) t
      end
  | _ -> raise (TypeError_local "incompatible type: integral type expected")
	
let pointer_offset_coerce e1 t = 
  let e1 = pointer_offset_coerce e1 t in
  try
    fold_constants e1
  with
    NotConstant -> e1

let strictly_follow_C90_weakness = ref true

(* workaround for undefined function call and autoconf dirtiness *)

let allow_calling_undefined_functions = ref false

let enable_gnu_autoconf_workaround = ref false

let is_type_loose_function t = 
  if !allow_calling_undefined_functions then
    match t.ct_ty with
      Tfunction([], true, { ct_ty = Tbuiltin bt }) ->
	(size_of_builtin_type bt = sizeof_int || !enable_gnu_autoconf_workaround)
    | _ -> false
  else
    false

let type_of_undefined_functions =
  Ctt_abstree.make_c_type (Tfunction([], true, Ctt_abstree.make_c_type (Tbuiltin Tint)))

let is_type_functype t = 
  match t.ct_ty with
    Tfunction _ -> true
  | _ -> false

let resolv_function_type t =
  match t.ct_ty with
    Tfunction(at,var,rt) -> rt, at, var
  | _ -> raise (TypeError_local "invoking non function")

let cvt_binop = function
    PbinTimes -> CTTbinTimes
  | PbinDiv -> CTTbinDiv 
  | PbinPlus | PbinMinus -> assert false
  | PbinModulo -> CTTbinModulo
  | PbinLshift -> CTTbinLshift
  | PbinRshift -> CTTbinRshift
  | PbinLogAnd -> CTTbinLogAnd
  | PbinLogOr -> CTTbinLogOr
  | PbinIntAnd -> CTTbinIntAnd
  | PbinIntOr -> CTTbinIntOr
  | PbinIntXor -> CTTbinIntXor
  | PbinLessThan -> CTTbinLessThan
  | PbinLessEqual -> CTTbinLessEqual
  | PbinGtrThan -> CTTbinGtrThan
  | PbinGtrEqual -> CTTbinGtrEqual
  | PbinEqual -> CTTbinEqual
  | PbinNotEqual -> CTTbinNotEqual

let get_integer_constant_value e = 
  match (locval e).expr_t with
    CTTexpConstant (CTTconstInteger c) -> c
  | _ -> raise (TypeError_typed(e,"not an integer constant"))

let convert_funcarg_type ~env t = 
  match t.ct_ty with
    Tarray(et,sz) -> make_c_type ~env (Tpointer(et))
  | Tfunction(_) -> make_c_type ~env (Tpointer(t))
  | Tvoid -> failwith "void type not allowed in parameter"
  | _ -> t

let check_type_as_function_return_type t = 
  match t.ct_ty with
    Tarray(et,sz) -> raise (TypeError_local "array type not allowed as function return value")
  | Tfunction(_) -> raise (TypeError_local "function type not allowed as function return value")
  | _ -> ()

let define_enum_item ~env ~loc name v = 
  if env.local_binding = [] then
    ignore (add_global_binding_to_env ~env ~loc name ModuleStatic
	      ~is_initialized:false (EnumVal v))
  else
    ignore (add_local_binding_to_env ~env name LocalStatic (EnumVal v))

let add_enum_to_struct_name_table ~allow_exists ~env name = 
  let add_to_list l = 
    try
      match List.assoc name l with
	EnumName when allow_exists -> l
      | EnumName -> raise (TypeError_local "duplicated enumeration definition")
      | StructName _ -> raise (TypeError_local "enum name conflicts with other struct/union name")
    with
      Not_found ->
	(name, EnumName) :: env.struct_name_table
  in
   match env.local_struct_name_table with
   [] -> env.struct_name_table <- add_to_list env.struct_name_table
   | h::t -> env.local_struct_name_table <- add_to_list h :: t

let define_enum ~env name = 
  add_enum_to_struct_name_table ~allow_exists:false ~env name

let declare_enum ~env name = 
  add_enum_to_struct_name_table ~allow_exists:true ~env name

let translate_struct_by_extension_default ~loc ~ext def = 
  if (ext <> []) then
    failwith "extension is not supported"
  else
    def

let translate_struct_by_extension_hook = ref translate_struct_by_extension_default

let translate_struct_by_extension ~ext def =
  try
    (!translate_struct_by_extension_hook) ~ext ~loc:def.str_loc def
  with
    Failure s ->
      raise (TypeError_local s)


let make_incomplete_struct_definition ~env ~ext ~is_union ~loc = 
  let id = make_new_struct_id ~env in
  let def = { str_union_p = is_union; str_size = None; 
	      str_align = None; str_fields = []; str_fields_byname = []; str_extension = [];
	      str_assignable = false;
	      str_loc = loc
	    } in
  let def = translate_struct_by_extension ~ext def in
  Earray.set env.struct_table id def;
  id

let add_incomplete_struct_definition_to_env ~env ~ext ~is_union ~loc name =
  let id = make_incomplete_struct_definition ~env ~ext ~is_union ~loc in
  (match env.local_struct_name_table with
    [] -> env.struct_name_table <- (name, StructName id) :: env.struct_name_table;
  | h::t -> env.local_struct_name_table <- ((name, StructName id) :: h)::t);
  id

let find_named_struct ~env ~is_union ~ext ~loc name =
  (* Find struct_id for already-declared struct. If not found, create new one. *)
  let rec iter_frame = function
      [] -> None
    | (n,EnumName)::_ when n = name ->
	raise (TypeError_local "name of union/struct crash with other enum name")
    | (n,StructName id)::_ when n = name -> Some id
    | _::tl -> iter_frame tl
  in
  let rec iter_frames = function
      [] -> None
    | hd::tl ->
	let r = iter_frame hd in
	if r = None then iter_frames tl
	else r
  in
  let check id = 
    let desc = get_struct_desc ~env id in
    if desc.str_union_p <> is_union then
      raise (TypeError_local "name of union/struct crash with other struct/union name");
    if (ext <> []) then
      raise (TypeError_local "already defined struct must be reffered without any extensions (1)")
  in
  match iter_frames env.local_struct_name_table with
    Some id -> check id; id
  | None ->
      match iter_frame env.struct_name_table with
	Some id -> check id; id
      | None ->
	  add_incomplete_struct_definition_to_env ~env ~ext ~is_union ~loc name

let create_named_struct_locally ~env ~is_union ~ext ~loc name = 
  (* Find struct_id for struct to be defined. *)
  let rec iter_frame = function
      [] -> None
    | (n,EnumName)::_ when n = name ->
	raise (TypeError_local "name of union/struct crash with other enum name")
    | (n,StructName id)::_ when n = name -> Some id
    | _::tl -> iter_frame tl
  in
  let check_merge id = 
    let desc = get_struct_desc ~env id in
    if desc.str_union_p <> is_union then
      raise (TypeError_local "name of union/struct crash with other struct/union name");
    if desc.str_size <> None then
      raise (TypeError_local ("duplicated definition of struct " ^ name));
    id
  in
  let current_frame = 
    match env.local_struct_name_table with
      h::_ -> h
    | [] -> env.struct_name_table
  in
  match iter_frame current_frame with
    Some id -> check_merge id
  | None -> add_incomplete_struct_definition_to_env ~env ~ext ~is_union ~loc name

let update_named_struct ~env ~is_union ~ext id decl =
  let old_sdesc = Earray.get env.struct_table id in
  assert (old_sdesc.str_size = None);
  let decl = translate_struct_by_extension ~ext decl in
  let new_ext =
    (!merge_struct_extensions_hook)
      ~new_ext:decl.str_extension
      ~old_ext:old_sdesc.str_extension in
  let decl = { decl with str_extension = new_ext } in
  Earray.set env.struct_table id decl

let define_anonymous_struct ~env ~is_union ~ext def = 
  let def = translate_struct_by_extension ~ext def in
  let id = make_new_struct_id ~env in
  Earray.set env.struct_table id def;
  id

let calculate_fields_cache = 
  let rec loop = function
      [] -> []
    | (ofs, NormalField { sf_id = name; sf_type = ty })::tl ->
	(name, (ty, StrOfsNormal ofs)) :: loop tl
    | (ofs, BitField { s_bf_fields = bfs })::tl ->
	loop_bfs ofs tl bfs
  and loop_bfs ofs gtl = function
      [] -> loop gtl
    | (None, _, _, _)::tl -> loop_bfs ofs gtl tl
    | (Some name, ty, wid, start)::tl -> 
	(name, (ty, StrOfsBitfield(ofs, wid, start))) :: loop_bfs ofs gtl tl
  in 
  loop

let update_fields_cache desc = 
  { desc with str_fields_byname = calculate_fields_cache desc.str_fields }

let check_struct_assignable ~env fields = 
  List.for_all
    (function
	(_ofs, NormalField { sf_type = ty }) ->
	  is_type_assignable ~env ty
      | (_ofs, BitField { s_bf_fields = bfs }) ->
	  List.for_all
	    (fun (_, ty, _, _) -> is_type_assignable ~env ty)
	    bfs)
    fields

let rec parse_struct_definition ~env ~is_union ~loc decl = try
  let current_size = ref zero_big_int in
  let current_align = ref (big_int_of_int Fsc_config.minimum_align_struct) in
  let current_output = ref [] in
  let current_pending_bitfields_rev = ref [] in

  let round_up ofs align = 
    ((ofs +! align -! unit_big_int) /!  align) *! align
  in
  
  let emit_element elem elem_size elem_align = 
    let elem_offset = round_up !current_size elem_align in
    current_output := !current_output @ [elem_offset, elem];
    current_align := max_big_int !current_align elem_align;
    current_size := 
      (if is_union = Union
      then max_big_int !current_size elem_size 
      else elem_offset +! elem_size);
    ()
  in
  let emit_bfields () = 
    if !current_pending_bitfields_rev = [] then ()
    else
      let bfs = List.rev !current_pending_bitfields_rev in
      current_pending_bitfields_rev := [];
      let sizes = List.map (fun (_, _, size) -> size) bfs in
      let bf_pos, bf_bytesize, bf_align = calculate_bitfields_packing sizes in
      let sf = 
	{ s_bf_size = big_int_of_int bf_bytesize;
	  s_bf_fields = 
	  List.map2 (fun (id, ty, size) pos -> (id, ty, size, pos))
	    bfs bf_pos } in
      emit_element (BitField sf) (big_int_of_int bf_bytesize) (big_int_of_int bf_align)
  in
  List.iter 
    (fun (PstructDecl(dslist,sd)) ->
      let (storageclass, ext, (basetype : c_type)) = specs_to_type ~env ~no_structdef:false dslist in
      if storageclass != None then
	failwith "parse error: storage class not allowed in element type";
      if ext != [] then
	failwith "parse error: extension not allowed in element type";
      List.iter
	(function
	    PstructDeclNormal(decl) ->
	      emit_bfields ();
	      let (id, t , _) = decl_to_type ~env decl basetype in
	      let size, align = match size_of_type ~env t, align_of_type ~env t with
		Some s, Some a -> s, a
	      | _ -> failwith_loc ~loc "incomplete type cannot be used as struct element"
	      in
	      emit_element 
		(NormalField {sf_id = id; sf_type = t; sf_size = size; })
		size align
	  | PstructDeclBitfield(Some decl, es) ->
	      if is_union = Union then failwith "bitfield in union" else
	      let (id, t, _) = decl_to_type ~env decl basetype in
	      let es = add_type_to_expr ~env es in
	      let s = get_integer_constant_value es in
	      if s ==! zero_big_int then
		failwith_loc ~loc "named zero-width bit field"
	      else if s <! zero_big_int then
		failwith_loc ~loc  "negative-width bit field"
	      else if s >! (big_int_of_int max_bitfield_width) then
		failwith_loc ~loc "too-wide bit field"
	      else if equal_type t type_int || equal_type t type_unsigned_int then
		current_pending_bitfields_rev := 
		  (Some id, t, int_of_big_int s) :: !current_pending_bitfields_rev
	      else
		failwith_loc ~loc "invalid type for bit fields"
	  | PstructDeclBitfield(None, es) ->
	      if is_union = Union then failwith "bitfield in union" else
	      let es = add_type_to_expr ~env es in
	      let s = get_integer_constant_value es in
	      if s ==! zero_big_int then
		emit_bfields ()
	      else if s >! (big_int_of_int max_bitfield_width) then
		failwith_loc ~loc "too-wide bit field"
	      else if s <! zero_big_int then
		failwith_loc ~loc "negative-width bit fields"
	      else if equal_type basetype type_int || equal_type basetype type_unsigned_int then
		current_pending_bitfields_rev := 
		  (None, basetype, int_of_big_int s) :: !current_pending_bitfields_rev
	      else
		failwith_loc ~loc "invalid type for bit fields"
	) sd
    ) decl;
  emit_bfields ();
  let total_size = round_up !current_size !current_align in
  let total_size = if total_size ==! zero_big_int then !current_align else total_size in
  let is_assignable = check_struct_assignable ~env !current_output in
  let desc = 
    { str_union_p = is_union; str_size = Some total_size;
      str_align = Some !current_align;
      str_fields = !current_output;
      str_fields_byname = [];
      str_assignable = is_assignable;
      str_extension = [];
      str_loc = loc;
    } in
  update_fields_cache desc
with Failure e -> failwith_loc ~loc e

and parse_enum_definition ~env ~loc decl = 
  ignore
    (List.fold_left
       (fun cur (name, v) ->
	 let v = match v with
	   None -> cur
	 | Some e1 -> 
	     let e1 = add_type_to_expr ~env e1 in
	     get_integer_constant_value e1
	 in
	 define_enum_item ~env ~loc name v;
	 v +! unit_big_int)
       zero_big_int decl)

and specs_to_type ~env ~no_structdef specs =
  let is_const = ref false in
  let is_volatile = ref false in
  let extension = ref [] in
  let signedness = ref None in
  let storageclass = ref None in
  let modifier = ref None in
  let builtin_basetype = ref None in
  let full_basetype = ref None in
  let is_void = ref false in
  let repl r v = 
    match !r with
      None -> r := Some v
    | Some _ -> failwith "duplicated specifier (specs_to_type::repl)"
  in
  let set r = match !r with
    true -> failwith "duplicated specifier (specs_to_type::set)"
  | false -> r := true
  in
  let set_dupok r =
    r := true
  in
  List.iter 
    (function
	TypeQualifier Const -> set_dupok is_const
      |	TypeQualifier Volatile -> set_dupok is_volatile
      |	TypeSpec (PtypespecBuiltin Signed) -> repl signedness Tsigned
      |	TypeSpec (PtypespecBuiltin Unsigned) -> repl signedness Tunsigned
      |	TypeSpec (PtypespecBuiltin Void) -> set is_void
      |	TypeSpec (PtypespecBuiltin Long) ->
	  if !modifier = Some Tlong then
	    modifier := Some Tlonglong
	  else
	    repl modifier Tlong
      |	TypeSpec (PtypespecBuiltin Short) -> repl modifier Tshort
      |	TypeSpec (PtypespecBuiltin p) -> repl builtin_basetype p
      |	ExtendedDeclSpec (id, t) ->
	  extension := !extension @ [id, t];
      | StorageClass s -> repl storageclass s
      |	TypeSpec (PtypespecEnumByName ename) ->
	  declare_enum ~env ename;
	  repl full_basetype type_int
      |	TypeSpec (PtypespecEnumByDef (nameopt, decl, loc)) ->
	  parse_enum_definition ~env ~loc decl;
	  Option.iter (define_enum ~env) nameopt;
	  repl full_basetype type_int
      |	TypeSpec (PtypespecAlias ident) ->
	  begin
	    match lookup_var_type ~env ident with
	      (_, TypeDefName t) ->
		repl full_basetype (update_c_type ~env t)
	    | _ -> failwith "panic: non-type appeared as typedef-name"
	  end
      | TypeSpec (PtypespecStruct (is_union, Some name, None, ext, loc)) ->
	  let id = find_named_struct ~env ~is_union ~ext ~loc name in
	  repl full_basetype (make_c_type ~env (Tstruct id))
      |	TypeSpec (PtypespecStruct (is_union, Some name, Some def, ext, loc)) ->
	  if no_structdef then
	    failwith "struct definition here is not meaningful";
	  let id = create_named_struct_locally ~env ~is_union ~ext ~loc name in
	  let decl = parse_struct_definition ~env ~is_union ~loc def in
	  update_named_struct ~env ~is_union ~ext id decl;
	  repl full_basetype (make_c_type ~env (Tstruct id))
      |	TypeSpec (PtypespecStruct (is_union, None, Some def, ext, loc)) ->
	  if no_structdef then
	    failwith "struct definition here is not meaningful";
	  let decl = parse_struct_definition ~env ~is_union ~loc def in
	  let id = define_anonymous_struct ~env ~is_union ~ext decl in
	  repl full_basetype (make_c_type ~env (Tstruct id))
      |	TypeSpec (PtypespecStruct (_, None, None, _, _)) ->
	  failwith "struct declaration must have either definition or name"
    ) specs;
  if !is_void then begin
    if !builtin_basetype != None || 
       !signedness != None || !full_basetype != None
    then failwith "void should not be qualified"
    else 
      !storageclass, !extension,
      (make_c_type ~env ~const:!is_const
	 ~volatile:!is_volatile Tvoid)
  end else match !full_basetype with
    Some fb -> begin
      if !signedness != None 
      then failwith "non-builtin type should not be signedness-qualified"
      else if !builtin_basetype != None 
      then failwith "non-builtin type should not be used with basetypes"
      else
	!storageclass, !extension,
	{ fb with
	  ct_const_p = fb.ct_const_p || !is_const;
	  ct_volatile_p = fb.ct_volatile_p || !is_volatile;
	}
    end
  | None -> begin
    let typ = 
      match !signedness, !modifier, !builtin_basetype with
	None, None, Some Char -> Tbuiltin Tchar
      | Some Tsigned, None, Some Char -> Tbuiltin Tschar
      | Some Tunsigned, None, Some Char -> Tbuiltin Tuchar
      | None, None, Some Float -> Tbuiltin Tfloat
      | None, None, Some Double -> Tbuiltin Tdouble
      | None, Some Tlong, Some Double -> Tbuiltin Tlongdouble
      | Some Tunsigned, Some Tlonglong, (None | Some Int) -> Tbuiltin Tulonglong
      | _, Some Tlonglong, (None | Some Int) -> Tbuiltin Tlonglong
      | Some Tunsigned, Some Tlong, (None | Some Int) -> Tbuiltin Tulong
      | _, Some Tlong, (None | Some Int) -> Tbuiltin Tlong
      | Some Tunsigned, Some Tshort, (None | Some Int) -> Tbuiltin Tushort
      | _, Some Tshort, (None | Some Int) -> Tbuiltin Tshort
      | Some Tunsigned, None, (None | Some Int) -> Tbuiltin Tuint
      | _, None, (None | Some Int) -> Tbuiltin Tint
      | _ -> failwith "parse error: invalid combination of specifiers (specs_to_type::typ)"
    in
    !storageclass, !extension, (make_c_type ~env ~const:(!is_const) ~volatile:(!is_volatile) typ)
  end

and parse_argtypes_iter ~env acc = function
    [] -> List.rev acc, false
  | (PpdeclConcrete(ts,d) |
    PpdeclAbstract(ts,d)) :: tl ->
      let storageclass, ext, basetype = (specs_to_type ~env ~no_structdef:true ts) in
        (* TODO: pass this REGISTER to environment *)
      if ext <> [] then
	failwith "parse error: extension not allowed in argument type";
      if storageclass != None && storageclass <> Some C_abstree.Register then
	failwith "parse error: storage class not allowed in argument type"
      else
	let (id,ty,_) = decl_to_type ~env d basetype in
	parse_argtypes_iter ~env ((id, convert_funcarg_type ~env ty)::acc) tl
  | [PpdeclVariant] ->
      List.rev acc, true
  | PpdeclVariant :: _ ->
      raise (TypeError_local("\"...\" must be a final argument (parser error?)"))

and parse_argtypes acc ?(is_funcdef = false) ~env = function
    [] -> [], not is_funcdef
	(* K&R style any-arg function f() *)
	(* ANSI style no-arg function f(), only in function definition *)
  | [PpdeclAbstract([TypeSpec (PtypespecBuiltin Void)], PdeclAnonymous)]
      (* ANSI style no-arg function f(void) *)
    -> [], false
  | l -> parse_argtypes_iter ~env acc l

and decl_to_type ~env decl basetype =
  (* ret value = ident, type, [ident, type; ...] (inner bindings) *)
  match decl with
  | PdeclAnonymous -> "", basetype, None
  | PdeclIdent x -> x, basetype, None
  | PdeclPointer(ql,decl) ->
      decl_to_type ~env decl (make_c_type_ql ~env ql (Tpointer(basetype)))
  | PdeclArray(decl,None) ->
      if is_type_functype basetype then
	raise (TypeError_local "cannot declare array of functions");
      decl_to_type ~env decl (make_c_type ~env (Tarray(basetype,None)))
  | PdeclArray(decl,Some esz) ->
      let size =
	let e = (add_type_to_expr ~env esz : expr) in
	get_integer_constant_value e
      in
      if is_type_functype basetype then
	raise (TypeError_local "cannot declare array of functions");
      if size <=! zero_big_int then
	raise (TypeError_local "cannot declare non-positive sized array");
      decl_to_type ~env decl (make_c_type ~env (Tarray(basetype,Some size)))
  | PdeclFuncType(decl,ptypes) -> 
      let this_bindings, is_varargs = parse_argtypes [] ~env ptypes in
      let argtypes = List.map (fun (_nam,t) -> convert_funcarg_type ~env t) this_bindings in
      let ident, rettype, inner_binding = 
	decl_to_type ~env decl (make_c_type ~env (Tfunction(argtypes, is_varargs, basetype))) in
      check_type_as_function_return_type basetype;
      ident, rettype, if inner_binding <> None then inner_binding else Some this_bindings
  | PdeclFuncIdent(decl,[]) -> 
      decl_to_type ~env (PdeclFuncType(decl,[])) basetype
  | PdeclFuncIdent(decl,idents) -> 
      raise (TypeError_local "Function declaration with untyped arguments not allowed here")

and type_of_typename ~env (Ptypename(specs,decl)) =
  let (sclass, ext, basetype) = specs_to_type ~env ~no_structdef:false specs in
  if ext <> [] then
    failwith "extension is not allowed in typename";
  if sclass <> None then
    failwith "storage class is not allowed in typename";
  let (id, ty, _) = decl_to_type ~env decl basetype in
  assert(id = "");
  ty

and add_type_to_expr ~env ?(lvalue = false) ?(is_funcall = false) exp =
  let r = add_type_to_expr_simple ~env ~lvalue ~is_funcall exp in
  try
    fold_constants r
  with
    NotConstant -> r

and add_type_to_expr_simple ~env ~lvalue ~is_funcall (exp : C_abstree.expr) : expr =
  let loc = locget exp in
  let make_expr e t = make_expr ~loc e (if lvalue then t else remove_qualifier t) in
  try 
    match lvalue, locval exp with (* exp used for recursion *)
    | false, PexpComma(e1, e2) ->
	let e1' = add_type_to_expr ~env e1 in
	let e2' = add_type_to_expr ~env e2 in
	make_expr (CTTexpComma(cast_coerce ~env (e1') type_void, e2')) (type_of e2')
    | false,  PexpAssign(e1, e2) ->
	let e1' = add_type_to_expr ~env ~lvalue:true e1 in
	check_assignable ~env e1';
	let e2' = add_type_to_expr ~env e2 in
	make_expr (CTTexpAssign(e1', assign_coerce ~env e2' (type_of e1'))) (type_of e1')
    | false,  PexpBinAssign(PbinPlus, e1, e2) ->
	begin
	  let e1 = coerce_implicit_pointer ~env (add_type_to_expr ~env ~lvalue:true e1) in
	  check_assignable ~env e1;
	  let e2 = coerce_implicit_pointer ~env (add_type_to_expr ~env e2) in
	  if is_pointer_without_null_rule e1 then begin
	    ensure_integer e2;
	    let e2 = pointer_offset_coerce e2 type_ptrdiff_t in
	    ensure_sized_pointer ~env e1;
	    make_expr (CTTexpBinAssign(CTTbinPlusPV, e1, None, e2)) (type_of e1)
	  end 
	  else begin
	    let e1c, e2 = coerce_merge_numeric ~env e1 e2 in
	    (* omit coercion of e1 (as rhs), and coercion of result *)
	    let tcast = if e1c == e1 then None else Some (type_of e1c) in
	    make_expr (CTTexpBinAssign(CTTbinPlusVV, e1, tcast, e2)) (type_of e1)
	  end
	end
    | false, PexpBinAssign(PbinMinus, e1, e2) ->
	begin
	  let e1 = coerce_implicit_pointer ~env(add_type_to_expr ~lvalue:true ~env e1) in
	  check_assignable ~env e1;
	  let e2 = coerce_implicit_pointer ~env(add_type_to_expr ~env e2) in
	  if is_pointer_without_null_rule e1 then begin
	    ensure_integer e2;
	    let e1 = coerce_implicit_pointer ~env e1 in
	    let e2 = pointer_offset_coerce e2 type_ptrdiff_t in
	    ensure_sized_pointer ~env e1;
	    make_expr (CTTexpBinAssign(CTTbinMinusPV, e1, None, e2)) (type_of e1)
	  end 
	  else begin
	    let e1c, e2 = coerce_merge_numeric ~env e1 e2 in
	    let tcast = if e1c == e1 then None else Some (type_of e1c) in
	    make_expr (CTTexpBinAssign(CTTbinMinusVV, e1, tcast, e2)) (type_of e1)
	  end
	end
    | false, PexpBinAssign((PbinTimes | PbinDiv) as op, e1, e2) ->
	  let e1 = add_type_to_expr ~lvalue:true ~env e1 in
	  check_assignable ~env e1;
	  let e2 = add_type_to_expr ~env e2 in
 	  ensure_numeric e1;
	  ensure_numeric e2;
	  let e1c, e2 = coerce_merge_numeric ~env e1 e2 in
	  let tcast = if e1c == e1 then None else Some (type_of e1c) in
	  make_expr (CTTexpBinAssign(cvt_binop op, e1, tcast, e2)) (type_of e1)
    | false, PexpBinAssign((PbinModulo | PbinIntAnd | PbinIntOr | PbinIntXor) as op,
		    e1, e2) ->
	  let e1 = add_type_to_expr ~lvalue:true ~env e1 in
	  check_assignable ~env e1;
	  let e2 = add_type_to_expr ~env e2 in
 	  ensure_integer e1;
	  ensure_integer e2;
	  let e1c, e2 = coerce_merge_numeric ~env e1 e2 in
	  let tcast = if e1c == e1 then None else Some (type_of e1c) in
	  make_expr (CTTexpBinAssign(cvt_binop op, e1, tcast, e2)) (type_of e1)
    | false, PexpBinAssign((PbinLshift | PbinRshift) as op,
		    e1, e2) ->
	  let e1 = add_type_to_expr ~lvalue:true ~env e1 in
	  check_assignable ~env e1;
	  let e2 = add_type_to_expr ~env e2 in
 	  ensure_integer e1;
	  ensure_integer e2;
	  let e1c = coerce_at_least_integer e1 in
	  let tcast = if e1c == e1 then None else Some (type_of e1c) in
	  make_expr (CTTexpBinAssign(cvt_binop op, e1, tcast, e2)) (type_of e1)
    | _, PexpBinAssign(op, e1, e2) -> assert false
    | false, PexpConditional(e1, e2, e3) ->
	let e1 = add_type_to_expr ~env e1 in
	let e2 = add_type_to_expr ~env e2 in
	let e3 = add_type_to_expr ~env e3 in
	let e1 = coerce_to_boolean ~env e1 in
	let e2, e3 = coerce_merge ~env e2 e3 in
	make_expr (CTTexpConditional(e1, e2, e3)) (type_of e2)
    | false, PexpBinExpr(PbinPlus, e1, e2) -> 
	begin
	  let e1 = coerce_implicit_pointer ~env (add_type_to_expr ~env e1) in
	  let e2 = coerce_implicit_pointer ~env (add_type_to_expr ~env e2) in
	  match (is_pointer_without_null_rule e1, is_pointer_without_null_rule e2) with
	    true, true -> raise (TypeError_local "adding pointer to pointer")
	  | true, false ->
	      let e2 = pointer_offset_coerce e2 type_ptrdiff_t in
	      ensure_sized_pointer ~env e1;
	      make_expr (CTTexpBinExpr(CTTbinPlusPV, e1, e2)) (type_of e1)
	  | false, true ->
	      let e1 = pointer_offset_coerce e1 type_ptrdiff_t in
	      ensure_sized_pointer ~env e2;
	      make_expr (CTTexpBinExpr(CTTbinPlusPV, e2, e1)) (type_of e2)
	  | false, false ->
	      ensure_numeric e1;
	      ensure_numeric e2;
	      let e1, e2 = coerce_merge_numeric ~env e1 e2 in
	      make_expr (CTTexpBinExpr(CTTbinPlusVV, e1, e2)) (type_of e1)
	end
    | false, PexpBinExpr(PbinMinus, e1, e2) -> 
	begin
	  let e1 = coerce_implicit_pointer ~env(add_type_to_expr ~env e1) in
	  let e2 = coerce_implicit_pointer ~env(add_type_to_expr ~env e2) in
	  match (is_pointer_without_null_rule e1, is_pointer_without_null_rule e2) with
	    true, true -> 
	      ensure_same_type e1 e2;
	      ensure_sized_pointer ~env e1;
	      make_expr (CTTexpBinExpr(CTTbinMinusPP, e1, e2)) type_ptrdiff_t
	  | true, false ->
	      let e2 = pointer_offset_coerce e2 type_ptrdiff_t in
	      ensure_sized_pointer ~env e1;
	      make_expr (CTTexpBinExpr(CTTbinMinusPV, e1, e2)) (type_of e1)
	  | false, true ->
	      raise (TypeError_local "subtracting pointer from other types")
	  | false, false ->
	      ensure_numeric e1;
	      ensure_numeric e2;
	      let e1, e2 = coerce_merge_numeric ~env e1 e2 in
	      make_expr (CTTexpBinExpr(CTTbinMinusVV, e1, e2)) (type_of e1)
	end
    | false, PexpBinExpr((PbinTimes | PbinDiv) as op, e1, e2) ->
	let e1 = add_type_to_expr ~env e1 in
	let e2 = add_type_to_expr ~env e2 in
	ensure_numeric e1;
	ensure_numeric e2;
	let e1, e2 = coerce_merge_numeric ~env e1 e2 in
	make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) (type_of e1)
    | false, PexpBinExpr((PbinModulo | PbinIntAnd | PbinIntOr | PbinIntXor) as op,
			 e1, e2) ->
        let e1 = add_type_to_expr ~env e1 in
        let e2 = add_type_to_expr ~env e2 in
        ensure_integer e1;
        ensure_integer e2;
        let e1, e2 = coerce_merge_numeric ~env e1 e2 in
	make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) (type_of e1)
    | false, PexpBinExpr((PbinLshift | PbinRshift) as op,
			 e1, e2) ->
        let e1 = add_type_to_expr ~env e1 in
        let e2 = add_type_to_expr ~env e2 in
        ensure_integer e1;
	ensure_integer e2;
	let e1 = coerce_at_least_integer e1 in
	make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) (type_of e1)
    | false, PexpBinExpr((PbinLogAnd | PbinLogOr) as op, e1, e2) ->
	let e1 = add_type_to_expr ~env e1 in
	let e1 = coerce_to_boolean ~env e1 in
	let e2 = add_type_to_expr ~env e2 in
	let e2 = coerce_to_boolean ~env e2 in
	make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) type_boolean
    | false, PexpBinExpr((PbinLessThan | PbinGtrThan | PbinLessEqual | PbinGtrEqual) as op, e1, e2) ->
	let e1 = add_type_to_expr ~env e1 in
	let e2 = add_type_to_expr ~env e2 in
	let e1 = coerce_implicit_pointer ~env e1 in
	let e2 = coerce_implicit_pointer ~env e2 in
	if is_numeric e1 && is_numeric e2 then begin
	  let e1, e2 = coerce_merge_numeric ~env e1 e2 in
	  make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) type_boolean
	end
	else if is_pointer_without_null_rule e1 && is_pointer_without_null_rule e2 then begin
	  ensure_same_type e1 e2;
	  make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) type_boolean
	end
	else
	  raise (TypeError_local "")
    | false, PexpBinExpr((PbinEqual | PbinNotEqual) as op, e1, e2) ->
	let e1 = coerce_implicit_pointer ~env(add_type_to_expr ~env e1) in
	let e2 = coerce_implicit_pointer ~env(add_type_to_expr ~env e2) in
	if is_numeric e1 && is_numeric e2 then begin
	  let e1, e2 = coerce_merge_numeric ~env e1 e2 in
	  make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) type_boolean
	end
	else if is_pointer_or_null e1 && is_pointer_or_null e2 then begin
	  let e1, e2 = coerce_merge_pointer ~env e1 e2 in
	  make_expr (CTTexpBinExpr(cvt_binop op, e1, e2)) type_boolean
	end
	else
	  raise (TypeError_local "")
    | false, ((PexpPreInc e1 | PexpPostInc e1 | PexpPostDec e1 | PexpPreDec e1) as e) ->
	let tagv, tagp = match e with
	  PexpPreInc _  -> CTTbinPlusVV, CTTbinPlusPV
	| PexpPostInc _ -> CTTbinPostPlusVV, CTTbinPostPlusPV
	| PexpPreDec _  -> CTTbinMinusVV, CTTbinMinusPV
	| PexpPostDec _ -> CTTbinPostMinusVV, CTTbinPostMinusPV
	| _ -> assert false
	in
	let e1 = add_type_to_expr ~env e1 in
	if is_numeric e1 then
	  let e1c, e2 = coerce_merge_numeric ~env e1 (exp_constant_one ~loc) in
	  let tcast = if e1c == e1 then None else Some (type_of e1c) in
	  make_expr (CTTexpBinAssign(tagv, e1, tcast, e2)) (type_of e1)
	else if is_sized_pointer ~env e1 then
	  make_expr (CTTexpBinAssign(tagp, e1, None, exp_constant_one_ptrdiff_t ~loc)) (type_of e1)
	else
	  raise (TypeError_local "")
    | false, PexpCast (t, e1) ->
	let t = type_of_typename ~env t in
	let e1 = add_type_to_expr ~env e1 in
	let e1 = cast_coerce ~env e1 t in
	e1
    | false, PexpUnaryExpr(LogNot, e1) ->
	let e1 = add_type_to_expr ~env e1 in
	let e1 = coerce_to_boolean ~env e1 in
	make_expr (CTTexpUnaryExpr(LogNot,e1)) type_boolean
    | false, PexpUnaryExpr(IntNot, e1) ->
	let e1 = add_type_to_expr ~env e1 in
	ensure_integer e1;
	let e1 = coerce_at_least_integer e1 in
	make_expr (CTTexpUnaryExpr(IntNot,e1)) (type_of e1)
    | false, PexpUnaryExpr(uop, e1) ->
	let e1 = add_type_to_expr ~env e1 in
	ensure_numeric e1;
	let e1 = coerce_at_least_integer e1 in
	make_expr (CTTexpUnaryExpr(uop,e1)) (type_of e1)
    | false, PexpAddress(e1) ->
	let e1 = add_type_to_expr ~env ~lvalue:true e1 in
	ensure_not_void e1;
	let t1 = type_of e1 in
	let new_type = 
	  (make_c_type ~env (Tpointer (type_of e1)))
	in begin
	match t1.ct_ty with
	  Tarray _ ->
	    make_expr (CTTexpCoerce(new_type,e1)) new_type
	| _ ->
	    make_expr (CTTexpAddress(e1)) new_type
	end
    | _, PexpPtrDeref(e1) -> begin
	let e1 = add_type_to_expr ~env e1 in
	let e1 = coerce_implicit_pointer ~env e1 in
	let t1 = type_of e1 in
	match t1.ct_ty with
	  Tpointer ( {ct_ty = Tarray(_)} as t ) ->
	    make_expr (CTTexpCoerce(t,e1)) t
	| Tpointer t -> 
	    make_expr (CTTexpPtrDeref(e1)) t
	| _ -> raise (TypeError_local "dereferencing non-pointer")
    end
    | false, PexpSizeOfType(t) ->
	let t = type_of_typename ~env t in
	begin
	  match size_of_type ~env t with
	    Some s ->
	      let t = type_size_t in
	      make_expr (CTTexpConstant (CTTconstInteger s)) t
	  | None ->
	      raise (TypeError_local (sfprintf "taking size of incomplete type: %a" Ctt_formatter.pp_c_type t))
	end
    | false, PexpSizeOfExpr(e1) ->
	let e1 = add_type_to_expr ~env e1 in
	let t1 = type_of e1 in
	begin
	  match size_of_type ~env t1 with
	    Some s ->
	      let t = type_size_t in
	      make_expr (CTTexpConstant (CTTconstInteger s)) t
	  | None ->
	      raise (TypeError_local (sfprintf "taking size of incomplete type: %a" Ctt_formatter.pp_c_type t1))
	end
    | _, PexpArrayRef(e1,e2) -> begin
	let et = 
	  begin
	    let e1 = add_type_to_expr ~env e1 in
	    let e2 = add_type_to_expr ~env e2 in
	    let e1 = coerce_implicit_pointer ~env e1 in
	    let e2 = coerce_implicit_pointer ~env e2 in
	    match (is_pointer_without_null_rule e1, is_pointer_without_null_rule e2) with
	      true, true -> raise (TypeError_local "used pointer as array index")
	    | true, false ->
		let e2 = pointer_offset_coerce e2 type_ptrdiff_t in
		ensure_sized_pointer ~env e1;
		make_expr (CTTexpBinExpr(CTTbinPlusPV, e1, e2)) (type_of e1)
	    | false, true ->
		let e1 = pointer_offset_coerce e1 type_ptrdiff_t in
		ensure_sized_pointer ~env e2;
		make_expr (CTTexpBinExpr(CTTbinPlusPV, e2, e1)) (type_of e2)
	    | false, false ->
		raise (TypeError_local "both array and index are numeric type")
	  end
	in
	let tet = type_of et in
	match tet.ct_ty with
	  Tpointer ( {ct_ty = Tarray(_)} as t ) ->
	    make_expr (CTTexpCoerce(t,et)) t
	| Tpointer t -> make_expr (CTTexpPtrDeref(et)) t
	| _ -> raise (TypeError_local "dereferencing non-pointer")
    end
    | false, PexpInvoke(e1, es) ->
	let e1 = add_type_to_expr ~env ~is_funcall:true e1 in
	let e1 = 
	  if is_pointer_without_null_rule e1 then begin
	    let t1 = type_of e1 in
	    match t1.ct_ty with
	      Tpointer t -> make_expr (CTTexpPtrDeref(e1)) t
	    | _ -> assert false
	  end
	  else e1
	in
	let rettype, argtypes, is_varargs = resolv_function_type (type_of e1) in
	let argnum = List.length es in
	let fargnum = List.length argtypes in
	if argnum < fargnum then 
	  raise (TypeError_local 
		   (Printf.sprintf "insufficient number of arguments (%d should be %d)" argnum fargnum));
	if argnum > fargnum && not is_varargs then 
	  raise (TypeError_local 
		   (Printf.sprintf "too many arguments (%d should be %d)" argnum fargnum));
	let args_fixed, args_varargs = 
	  Util.split_at_nth fargnum es in
	assert(List.length args_fixed = List.length argtypes);
	let newargs_fixed = 
	  List.map2
	    (fun e t -> assign_coerce ~env ~check_qual:qual_dcare (add_type_to_expr ~env e) t)
	    args_fixed argtypes in
	let newargs_variable =
	  List.map
	    (fun e -> promote_KandR ~env (add_type_to_expr ~env e))
	    args_varargs
	in
	make_expr (CTTexpInvoke(e1,newargs_fixed @ newargs_variable)) rettype
    | _, PexpField(e1,id) ->
	let e1 = add_type_to_expr ~env ~lvalue:lvalue e1 in
	let t = get_field_type ~env (type_of e1) id in
	make_expr (CTTexpField(e1,id)) (merge_quals t ~from:(type_of e1))
    | _, PexpPtrField(e1,id) -> begin
	let star_e1 = 
	  let e1 = add_type_to_expr ~env e1 in
	  let e1 = coerce_implicit_pointer ~env e1 in
	  let t1 = type_of e1 in
	  match t1.ct_ty with
	    Tpointer t -> make_expr (CTTexpPtrDeref(e1)) t
	  | _ -> raise (TypeError_local "dereferencing non-pointer")
	in
	let t = get_field_type ~env (type_of star_e1) id in
	make_expr (CTTexpField(star_e1,id)) (merge_quals t ~from:(type_of star_e1))
    end
    | false, PexpConstant(c) ->
	let d, t = parse_constant c in
	make_expr (CTTexpConstant d) t
    | _, PexpVar(id) -> begin
	try
	  match lookup_var_type ~env id with
	    (_, EnumVal v) ->
	      make_expr (CTTexpConstant (CTTconstInteger v)) type_int
	  | (nid, Var t) -> begin
	      let e1 = make_expr (CTTexpVar(nid, t)) t
	      in
	      if is_type_loose_function t then begin
		dprintf 6 "warning: implicitly-handled variable %s used" id;
		let pt = make_c_type ~env (Tpointer t) in
		let e1 = make_expr (CTTexpAddress e1) (make_c_type ~env (Tpointer type_void)) in
		let e1 = make_expr (CTTexpCoerce (pt,e1)) pt in
		make_expr (CTTexpPtrDeref(e1)) t
	      end
	      else
		e1
	  end
	  | (_, TypeDefName t) ->
	      raise (TypeError_local(id ^ "is not a variable but a type"))
	with
	  Not_found ->
	    if is_funcall && !allow_calling_undefined_functions then begin
	      dprintf 0 "warning: undeclared function \"%s\" called." id;
	      let id, _ = add_global_binding_to_env ~env ~loc ~is_initialized:false 
		  id (Extern []) (Var type_of_undefined_functions)
	      in
	      ignore (lookup_var_type ~env id); (* assertion *)
	      assert(is_type_loose_function type_of_undefined_functions);
	      add_type_to_expr_simple ~env ~lvalue ~is_funcall:false exp
	    end
	    else
	      raise (TypeError_local ("var " ^ id ^ " not found"))
    end
      (* Fail-Safe C Extension *)
    | false, PexpTypeOfType(t) -> begin
	let t = type_of_typename ~env t in
	make_expr (CTTexpConstant (CTTconstTypeInfo t)) type_typeinfo_ptr
    end
    | true, _ -> raise (TypeError_local "using rvalue as lvalue")
  with
    TypeError_local s | Failure s -> raise (TypeError_untyped(exp,s))

let get_return_type ~env =
  match lookup_var_type ~env "return" with
    (_, Var t) -> t
  | _ -> assert false

type initdatum_desc = 
    Ilist of initstream
  | Isingle of expr
  | Istring of expr * string
and initdatum = initdatum_desc Locterm.t
and initstream = 
    { mutable ist_dat : initdatum list; ist_loc : location }

let fill_zero_initializer = ref true

let peek_stream stream = 
  match stream.ist_dat with
    [] -> locput ~loc:stream.ist_loc (Isingle (exp_constant_zero ~loc:stream.ist_loc))
  | hd::tl -> hd

let pop_stream stream = 
  match stream.ist_dat with
    [] -> locput ~loc:stream.ist_loc (Isingle (exp_constant_zero ~loc:stream.ist_loc))
  | hd::tl -> stream.ist_dat <- tl; hd

let is_next_list_stream stream = 
  match locval (peek_stream stream) with
    Ilist _ -> true
  | _ -> false

let is_next_string_stream stream = 
  match locval (peek_stream stream) with
    Istring _ -> true
  | _ -> false

let isempty_stream ( stream : initstream ) = 
  match stream.ist_dat with
    [] -> true
  | _ -> false

let rec parse_list_initializations ~env stream et size = 
  match size with
    None ->
      let rec iter acc = 
	if isempty_stream stream then List.rev acc
	else
	  let _, i = parse_initialization ~type_change_allowed:false ~env stream et in
	  iter (i :: acc)
      in
      iter []
  | Some sz ->
      let sz = int_of_big_int sz in
      let rec iter acc n = 
	if n >= sz || ((not !fill_zero_initializer) && n >= 1 && isempty_stream stream) 
	then List.rev acc
	else
	  let _, i = parse_initialization ~type_change_allowed:false ~env stream et in
	  iter (i :: acc) (n + 1)
      in
      iter [] 0

and parse_struct_initializations ~env stream struct_id =
  let desc = get_struct_desc ~env struct_id in
  let fields_to_be_initialized = 
    match desc.str_fields, desc.str_union_p with
      [], _ -> []
    | (hd::_), Union -> [hd]
    | l, Struct -> l
  in
  let rec iter_bfields = function
      [] -> []
    | (_,t,_,_)::tl ->
	let _, i = parse_initialization ~type_change_allowed:false ~env stream t in
	i :: iter_bfields tl
  in
  let rec iter = function
      [] -> []
    | (_, NormalField { sf_type = t }) :: tl ->
	let _, i = parse_initialization ~type_change_allowed:false ~env stream t in
	i :: (iter tl)
    | (_, BitField { s_bf_fields = bfl }) :: tl ->
	let l = iter_bfields bfl in
	l @ (iter tl)
  in
  CTTinitList (iter fields_to_be_initialized)

and parse_initialization ?(type_change_allowed = true) ~env (stream : initstream) typ = 
  match typ.ct_ty with
    Tvoid -> raise (TypeError_local("initializing void value?"))
  | Tbuiltin(_) | Tpointer(_) -> begin
      match locpair (pop_stream stream) with
	loc, Ilist st -> begin
	  match locval (pop_stream st) with
	    Isingle e | Istring (e, _) ->
	      if not (isempty_stream st) then
		raise (TypeError_local "initializing scalar with list (more than 2 elements)");
	      let e = assign_coerce ~env ~check_qual:qual_dcare e typ in
	      typ, locput ~loc (CTTinitExp e)
	  | Ilist _ ->
	      raise (TypeError_local("initializing scalar with list of list"))
	end
      |	loc, (Isingle e | Istring (e,_)) ->
	  let e = assign_coerce ~env ~check_qual:qual_dcare e typ in
	  typ, locput ~loc (CTTinitExp e)
  end
  | Tarray({ct_ty = Tbuiltin(Tchar | Tuchar | Tschar)} as et,sz)
    when is_next_string_stream stream -> begin
      match locpair (pop_stream stream) with
	loc, Istring(e,s) ->
	  let init_sz = String.length s in
	  assert (s.[String.length s - 1] = '\000');
	  let s = String.sub s 0 (String.length s - 1) (* strip trailer \000 *)
	  in
	  let new_typ, length = 
	    match sz with
	      None ->
		if type_change_allowed then
		  update_c_type ~env { typ with ct_ty = (Tarray(et, Some (big_int_of_int init_sz))) }, init_sz
		else
		  raise (TypeError_local "array of unspecified size not allowed here")
	    | Some sz ->
		if (big_int_of_int init_sz) <=! sz then
		  typ, if !fill_zero_initializer then int_of_big_int sz else init_sz
		else if (big_int_of_int (init_sz - 1)) ==! sz then
		  typ, init_sz - 1
		else
		  raise (TypeError_typed(e, "array size mismatch: initsz = " ^ string_of_int init_sz
					^ ", sz = " ^ string_of_big_int sz))
	  in
	  let inits = String.make length '\000' in
	  String.blit s 0 inits 0 (String.length s);
	  let charsinit = 
	    let rec iter p = 
	      if p >= length then [] else
	      let i = 
		locput ~loc
		  (CTTinitExp 
		     (make_expr ~loc
			(CTTexpConstant (CTTconstInteger (big_int_of_int (Char.code inits.[p]))))
			et))
	      in
	      i :: iter (p + 1)
	    in
	    iter 0
	  in
	  new_typ, locput ~loc (CTTinitList charsinit)
      |	_ -> assert false
    end
  | Tarray(et,sz) -> begin
      match locpair (peek_stream stream) with
	loc, Ilist inside_stream -> begin
	  ignore (pop_stream stream);
	  let inits = parse_list_initializations ~env inside_stream et sz in
	  match sz with
	    None ->
	      if type_change_allowed then
		let init_sz = List.length inits in
		update_c_type ~env { typ with ct_ty = Tarray(et, Some (big_int_of_int init_sz)) }, 
		locput ~loc (CTTinitList inits)
	      else
		raise (TypeError_local "array of unspecified size not allowed here")
	  | Some _ ->
	      if not (isempty_stream inside_stream) then
		raise (TypeError_local "too many initializers")
	      else
		typ, locput ~loc (CTTinitList inits)
	end
      |	loc, (Isingle e | Istring(e,_)) -> begin
	  match sz with
	    None ->
	      raise (TypeError_local "array of unspecified size not allowed here")
	  | 
	    Some _ ->
	      let inits = parse_list_initializations ~env stream et sz in
	      typ, locput ~loc (CTTinitList inits)
      end
  end
  | Tstruct(id) -> begin
      match locpair (peek_stream stream) with
	loc, Ilist inside_stream -> begin
	  ignore (pop_stream stream);
	  let inits = parse_struct_initializations ~env inside_stream id in
	  if not (isempty_stream inside_stream) then
	    raise (TypeError_local "too many initializers")
	  else
	    typ, locput ~loc inits
	end
      |	loc, (Isingle e | Istring(e,_)) -> begin
	  let inits = parse_struct_initializations ~env stream id in
	  typ, locput ~loc inits
      end
  end
  | Tfunction(_) -> 
      raise (TypeError_local "initializing function")
  | Tabstract(_) -> assert false

let rec make_stream ~env i =
  let loc = locget i in
  match locval i with
    PinitExp e -> begin
      let e = add_type_to_expr ~env e in
      match (locval e).expr_t with
	CTTexpConstant(CTTconstString s) ->
	  locput ~loc (Istring (e,s))
      |	_ -> locput ~loc (Isingle e)
    end
  | PinitList(l) ->
      let l = Util.list_map (make_stream ~env) l in
      locput ~loc (Ilist { ist_dat = l; ist_loc = loc })

let parse_initialization ~env initializers typ = 
  let loc = locget initializers in
  match typ.ct_ty, locval initializers with
    (Tbuiltin _ | Tpointer _), (PinitExp _ | PinitList _)
  | (Tarray _ | Tstruct _), PinitList _ ->
      let stream = make_stream ~env initializers in
      parse_initialization ~env { ist_dat = [stream]; ist_loc = locget initializers } typ
  | Tstruct _, PinitExp e ->
      (* special case: single element struct initializer *)
      let e = add_type_to_expr ~env e in
      let e = assign_coerce ~env e typ in
      typ, locput ~loc (CTTinitExp e)
  | Tarray(t, _), PinitExp e -> begin
      let stream = make_stream ~env initializers in
      match locval stream, t.ct_ty with
	Istring _, Tbuiltin (Tchar | Tschar | Tuchar) ->
	  parse_initialization ~env { ist_dat = [stream]; ist_loc = locget e } typ
      | _ ->
	  failwith "arrays cannot be initialized with a single expression"
  end
  | Tabstract _, _ -> assert false
  | (Tvoid | Tfunction _), _ -> failwith "this type cannot be initialized"

(* shared with parsing of K&R-style parameter declaration *)
let rec parse_local_declarators ~loc ~is_extern ~is_typedef ~is_krargs sclass basetype binds ~env = function
    [] -> binds
  | PinitDecl(decl,init) :: tl ->
      let id, ty, _ = decl_to_type ~env decl basetype in
      match is_extern, is_typedef, ty.ct_ty with
      | true, _, _ | false, false, Tfunction _ -> begin
	  if init <> None then
	    raise (TypeError_local "local declaration of extern values cannot be initialized");
	  if is_krargs then
	    raise (TypeError_local "extern declaration cannot appear in K&R function parameter declaration");
	  let translated_sclass = 
	    match is_extern, sclass with
	      false, LocalStatic -> ModuleStatic
	    | _, Auto | false, Register -> Extern []
	    | _ -> assert false
	  in
	  let _, _ = add_global_binding_to_env ~env ~loc ~is_initialized:false id translated_sclass (Var ty) in
	  parse_local_declarators ~loc ~is_extern ~is_typedef ~is_krargs sclass basetype binds ~env tl
      end
      | _ -> begin
	  let nid, leave_declaration =
	    add_local_binding_to_env id sclass 
	      (if is_typedef then TypeDefName ty else Var ty) ~env 
	  in
	  let ty, einit = 
	    match init with
	      None -> ty, None
	    | Some i ->
		if is_typedef || not leave_declaration then
		  raise (TypeError_local "typedef with initializer");
		let updated_ty, init = parse_initialization ~env i ty in
		update_local_binding ~env id ty updated_ty;
		updated_ty, Some init
	  in
	  let ty = if is_krargs then convert_funcarg_type ~env ty else ty in
	  parse_local_declarators ~loc ~is_extern ~is_typedef ~is_krargs sclass basetype
	    (if is_typedef || not leave_declaration then binds
	    else binds @ [sclass, ty, (if is_krargs then id else nid), einit])
	    ~env tl
      end

(* shared with parsing of K&R-style parameter declaration *)    
let rec parse_local_declarations ~env ~is_krargs l =
  Util.map_flatten
    (fun v -> 
      let loc = locget v in
      match locval v with
      PdeclVariable(specs,decls) ->
	let sclass, ext, basetype = specs_to_type ~env ~no_structdef:is_krargs specs in
	if is_krargs && sclass <> None && sclass <> Some C_abstree.Register then
	  failwith ("storage class not allowed here: " ^ C_pp.string_of_storage_class (Util.someof sclass));
	let () = match ext with
	  [] -> ()
	| (id, _)::_ -> failwith (id ^ " not allowed here")
	in
	let sclass, is_typedef, is_extern =
	  match sclass with
	    Some C_abstree.Static      -> LocalStatic, false, false
	  | Some C_abstree.Auto | None -> Auto, false, false
	  | Some C_abstree.Typedef     -> Auto, true, false
	  | Some C_abstree.Register    -> Register, false, false
	  | Some C_abstree.Extern      -> Auto, false, true
	  | Some st -> raise 
		(TypeError_local
		   (sprintf "bad specifier: %s" (C_pp.string_of_storage_class st)))
	in
	let binds = parse_local_declarators ~loc ~is_extern ~is_typedef ~is_krargs sclass basetype [] ~env decls in
	binds
    | PdeclFunction _ ->
	raise (TypeError_local "internal function definition not allowed")
    | PdeclPragmatic _ ->
	raise (TypeError_local "pragmatic declaration not allowed")) l

let parse_local_declarations ~env decl =
  parse_local_declarations ~env ~is_krargs:false decl
and parse_kandr_argument_declarations ~env decl = 
  parse_local_declarations ~env ~is_krargs:true decl

let rec add_type_to_statement ~env stmt = 
  try
    locmap
      (function
	  PstmtExpr None -> CTTstmtNull
	| PstmtExpr (Some e) ->
	    let e = add_type_to_expr ~env e in
	    let e = cast_coerce ~env e type_void in
	    CTTstmtExpr e
	| PstmtLabeled (l,s) ->
	    if Hashtbl.mem env.goto_labels l
		&& Hashtbl.find env.goto_labels l = true then
	      raise (TypeError_local ("duplicated label " ^ l));
	    Hashtbl.replace env.goto_labels l true;
	    CTTstmtLabeled (l, add_type_to_statement ~env s)
	| PstmtCase_Labeled (v,s) -> begin
	    match env.switch_frames with
	      [] -> raise (TypeError_local "case label without switch");
	    | (t,r,ht)::_ ->
		let v = add_type_to_expr ~env v in
		let v = arithmetic_coerce v t in
		let v = 
		  match v.locterm_v.expr_t with
		    CTTexpConstant (CTTconstInteger v) -> v
		  | _ -> raise (TypeError_local "non-constant in case label")
		in
		if BigInt_Ht.mem ht v then
		  raise (TypeError_local ("duplicated case label " ^ (string_of_big_int v)))
		else
		  BigInt_Ht.replace ht v ();
		CTTstmtCase_Labeled(v,add_type_to_statement ~env s)
	end
	| PstmtDefault_Labeled s -> begin
	    match env.switch_frames with
	      [] -> raise (TypeError_local "case label without switch");
	    | (t,r,ht)::_ ->
		if !r then
		  raise (TypeError_local ("duplicated default label"))
		else r := true;
		CTTstmtDefault_Labeled(add_type_to_statement ~env s)
	end
	| PstmtCompound(ds,ss) ->
	    unwind_protect
	      (fun () -> extend_local_frame_env ~env)
	      (fun () -> 
		let binds = parse_local_declarations ~env ds in
		let ss = Util.list_map (add_type_to_statement ~env) ss
		in
		CTTstmtCompound(binds,ss))
	      (fun () -> shrink_local_frame_env ~env)
	| PstmtIf(e1,s1,s2) ->
	    let e1 = add_type_to_expr ~env e1 in
	    let e1 = coerce_to_boolean ~env e1 in
	    let s1 = add_type_to_statement ~env s1 in
	    let s2 = Option.map (add_type_to_statement ~env) s2 in
	    CTTstmtIf(e1,s1,s2)
	| PstmtSwitch(e,s) ->
	    let e = add_type_to_expr ~env e in
	    ensure_integer e;
	    let e = coerce_at_least_integer e in
	    let t = type_of e in
	    unwind_protect
	      (fun () -> env.switch_frames, env.break_allowed)
	      (fun _ ->
		env.switch_frames <- 
		  (t, ref false, BigInt_Ht.create 16)::env.switch_frames;
		env.break_allowed <- true;
		let s = add_type_to_statement ~env s in
		CTTstmtSwitch(e,s))
	      (fun (s, b) ->
		env.switch_frames <- s; env.break_allowed <- b)
	| PstmtWhile(e1,s1) ->
	    let e1 = add_type_to_expr ~env e1 in
	    let e1 = coerce_to_boolean ~env e1 in
	    let s1 = add_type_to_loopbody ~env s1 in
	    CTTstmtWhile(e1,s1)
	| PstmtDoWhile(s1,e1) ->
	    let s1 = add_type_to_loopbody ~env s1 in
	    let e1 = add_type_to_expr ~env e1 in
	    let e1 = coerce_to_boolean ~env e1 in
	    CTTstmtDoWhile(s1,e1)
	| PstmtFor(e1,e2,e3,s1) ->
	    let e1 = Option.map (fun e -> cast_coerce ~env (add_type_to_expr ~env e) type_void) e1 in
	    let e2 = Option.map (fun e -> coerce_to_boolean ~env (add_type_to_expr ~env e)) e2 in
	    let e3 = Option.map (fun e -> cast_coerce ~env (add_type_to_expr ~env e) type_void) e3 in
	    let s1 = add_type_to_loopbody ~env s1 in
	    CTTstmtFor(e1,e2,e3,s1)
	| PstmtGoto(l) -> 
	    if not (Hashtbl.mem env.goto_labels l) then
	      (* forward-referencing goto *)
	      Hashtbl.replace env.goto_labels l false;
	    CTTstmtGoto l
	| PstmtContinue ->
	    if not env.continue_allowed then
	      raise (TypeError_local "no continue allowed here");
	    CTTstmtContinue
	| PstmtBreak ->
	    if not env.break_allowed then
	      raise (TypeError_local "no break allowed here");
	    CTTstmtBreak
	| PstmtReturn(e) ->
	    let rettype = get_return_type ~env in
	    match e with
	      None ->
		if rettype.ct_ty <> Tvoid then
		  raise (TypeError_local "no return value allowed in void-returning function")
		else
		  CTTstmtReturn(None)
	    | Some e' ->
		if rettype.ct_ty = Tvoid then
		  raise (TypeError_local "return value not expected")
		else begin
		  let e' = add_type_to_expr ~env e' in
		  let e' = assign_coerce ~env e' rettype in
		  CTTstmtReturn(Some e')
		end)
      stmt
  with
    TypeError_local(s) | Failure s ->
      let loc = locget stmt in
      raise (TypeError_located(loc, s))

and add_type_to_loopbody ~env s1 = 
  unwind_protect
    (fun () -> env.break_allowed, env.continue_allowed)
    (fun _ ->
      env.break_allowed <- true; env.continue_allowed <- true;
      add_type_to_statement ~env s1)
    (fun (b, c) -> env.break_allowed <- b; env.continue_allowed <- c)

let rec parse_global_declarators_rev ~loc
    ((sclass, is_typedef) as sclass')
    basetype binds_rev ~env =
  function
      [] -> binds_rev
    | PinitDecl(decl,init) :: tl ->
	let id, ty, _ = decl_to_type ~env decl basetype in

	let ty, einit, is_initialized = 
	  match init with
	    None -> ty, None, false
	  | Some i ->
	      let _, _ = 
		(add_global_binding_to_env ~loc id sclass
		   (if is_typedef then TypeDefName ty else Var ty) ~is_initialized:false ~env)
	      in
	      let ty, init = parse_initialization ~env i ty in
	      ty, Some init, true
	in
	let sclass = match sclass with
	  Extern ((_::_) as ext) ->
	    Extern(translate_external_extension ~loc ~ext ~sclass ~ty ~is_initialized)
	| _ -> sclass
	in
	let nid, keep_newer_initialization = 
	  (add_global_binding_to_env ~loc id sclass
	     (if is_typedef then TypeDefName ty else Var ty) ~is_initialized ~env)
	in
	(* Format.eprintf "%s: keep newer = %b@." nid keep_newer_initialization; *)
	let binds_rev = 
	  if keep_newer_initialization then
	    (CTTdeclVariable(sclass, ty, nid, einit)) :: binds_rev
	  else
	    binds_rev
	in
	parse_global_declarators_rev ~loc sclass' basetype binds_rev ~env tl
	  
let decl_to_type_function ~env decl argdecls basetype = 
  let binds = unwind_protect
      (fun () -> extend_local_frame_env ~env)
      (fun () -> parse_kandr_argument_declarations ~env argdecls)
      (fun () -> shrink_local_frame_env ~env)
  in
  let argtypelist =
    List.map
      (fun (_, cty, id, initopt) ->
	if initopt <> None then
	  failwith
	    "no initializer allowed in K&R-style argument declaration";
	id, cty)
      binds in
  (*List.iter (fun (id, (_, cty)) -> 
    prerr_endline (id ^ " :: " ^ Ctt_formatter.pp_c_type cty))
    argtypelist;*)
  let rec iter ~env decl basetype = 
    match decl with
  | PdeclAnonymous -> "", basetype, None
  | PdeclIdent x -> x, basetype, None
  | PdeclPointer(ql,decl) ->
      iter ~env decl (make_c_type_ql ~env ql (Tpointer(basetype)))
  | PdeclArray(decl,None) ->
      if is_type_functype basetype then
	raise (TypeError_local "cannot declare array of functions");
      iter ~env decl (make_c_type ~env (Tarray(basetype,None)))
  | PdeclArray(decl,Some esz) ->
      let size =
	let e = (add_type_to_expr ~env esz : expr) in
	get_integer_constant_value e
      in
      if is_type_functype basetype then
	raise (TypeError_local "cannot declare array of functions");
      if size <=! zero_big_int then
	raise (TypeError_local "cannot declare non-positive sized array");
      iter ~env decl (make_c_type ~env (Tarray(basetype,Some size)))
  | PdeclFuncType(decl,ptypes) -> 
      let this_bindings, is_varargs = parse_argtypes ~is_funcdef:true [] ~env ptypes in
      let argtypes = List.map (fun (_nam,t) -> convert_funcarg_type ~env t) this_bindings in
      let ident, rettype, inner_binding = 
	iter ~env decl (make_c_type ~env (Tfunction(argtypes, is_varargs, basetype))) in
      check_type_as_function_return_type basetype;
      ident, rettype, if inner_binding <> None then inner_binding else Some this_bindings
  | PdeclFuncIdent(decl,[]) -> 
      iter ~env (PdeclFuncType(decl,[])) basetype
  | PdeclFuncIdent(PdeclIdent _ as decl,idents) ->
      let bindings = 
	List.fold_right
	  (fun id bind ->
	    if List.mem_assoc id bind then
	      failwith ("argument name " ^ id ^ " appeared twice (K&R)");
	    let cty = 
	      try List.assoc id argtypelist
	      with Not_found ->
		if !strictly_follow_C90_weakness then
		  type_int
		else
		  failwith ("type of " ^ id ^ " is not declared in K&R style definition")
	    in
	    (id, cty) :: bind)
	  idents []
      in
      let argtypes = List.map (fun x -> convert_funcarg_type ~env (snd x)) bindings in
      let ident, rettype, _ = 
	iter ~env decl (make_c_type ~env (Tfunction(argtypes, false, basetype))) in
      ident, rettype, Some bindings
  | PdeclFuncIdent _ ->
      raise (TypeError_local "Function declaration with untyped arguments not allowed here")
  in
  iter ~env decl basetype

let add_type_to_funcbody ~env stmt = 
  unwind_protect
    (fun () -> ())
    (fun () ->
      assert (not env.break_allowed);
      assert (not env.continue_allowed);
      assert (Hashtbl.length env.goto_labels = 0);
      assert (env.switch_frames = []);
      let r = add_type_to_statement ~env stmt in
      Hashtbl.iter
	(fun l defined ->
	  if not defined then
	    raise (TypeError_local ("undefined label " ^ l)))
	env.goto_labels;
      r)
    (fun () ->
      env.break_allowed <- false;
      env.continue_allowed <- false;
      Hashtbl.clear env.goto_labels;
      env.switch_frames <- [])

let rec parse_global_declarations acc ~env =
  Util.map_flatten
    (fun d ->
      try 
	let loc = locget d in 
	match locval d with
	  PdeclVariable(specs,decls) ->
	    let sclass, ext, basetype = specs_to_type ~env ~no_structdef:false specs in
	    let (_,is_typedef) as sclass = match sclass, ext with
	      Some C_abstree.Static, [] -> ModuleStatic, false
	    | Some C_abstree.Extern, ext -> Extern ext, false
	    | Some C_abstree.Typedef, [] -> ModuleStatic, true
	    | None, [] -> Global [], false
	    | Some s, [] -> failwith ("bad specifier: " ^ C_pp.string_of_storage_class s)
	    | _, ((id,_)::_) -> failwith (id ^ " cannot be used here")
	    in
	    let binds = parse_global_declarators_rev ~loc:(locget d) sclass basetype [] ~env decls in
	    (* Format.eprintf "  binding: %d\n" (List.length binds); *)
	    if is_typedef then [] else list_map (locput ~loc) binds
	| PdeclFunction(specs,decl,argdecls,body) ->
	    let sclass, ext, basetype = specs_to_type ~env ~no_structdef:false specs in
	    let sclass = match sclass, ext with
	      Some C_abstree.Static, [] -> ModuleStatic
	    | Some C_abstree.Extern, [] -> Global []
	    | None, [] -> Global []
	    | Some s, [] -> failwith ("bad specifier: " ^ C_pp.string_of_storage_class s)
	    | _, ((id,_)::_) -> failwith (id ^ " cannot be used here")
	    in
	    let id, ty, args = 
	      decl_to_type_function ~env decl argdecls basetype
	    in
	    let rt = match ty.ct_ty with
	      Tfunction(at,v,rt) -> rt
	    | _ -> raise (TypeError_local "defining function body to non-function type")
	    in
	    let args = Option.get args in
	    let nid, keep_newer_initialization = add_global_binding_to_env ~loc id sclass (Var ty)
		~is_initialized:true ~env in
	    assert (keep_newer_initialization);
	    extend_local_frame_env ~env;
	    let nids = 
	      List.map
		(fun (id, ty) ->
		  fst (add_local_binding_to_env ~env id FuncArgs (Var ty))) args in
	    ignore (add_local_binding_to_env ~env "return" FuncArgs (Var rt));
	    let body = add_type_to_funcbody ~env body in
	    shrink_local_frame_env ~env;
	    let o = CTTdeclFunction(sclass,ty,nid,nids,body) in
	    list_map (locput ~loc) [o]
	| PdeclPragmatic s ->
	    raise (TypeError_local "pragmatic declaration is not supported")
      with
	TypeError_local s | Failure s -> failwith_loc ~loc:(locget d) s)

let empty_environment t = 
  {
   module_name = ""; (* todo *)
   var_seqno = 0;
   struct_counter = 0;
   global_binding = Hashtbl.create 16;
   struct_table = Earray.empty ();
   struct_name_table = [];
   local_struct_name_table = [];
   local_binding = [];
   continue_allowed = false;
   break_allowed = false;
   goto_labels = Hashtbl.create 16;
   switch_frames = [];
 }

let filter_environment env t = 
  Ctt_abstree.reconstruct_gdecl_table
    {
     Ctt_abstree.module_hash = Printf.sprintf "%08x" (Hashtbl.hash_param Pervasives.max_int Pervasives.max_int t);
     Ctt_abstree.struct_table = env.struct_table;
     Ctt_abstree.struct_name_table = 
     Util.map_flatten 
       (function 
	   (name, StructName id) -> [ name, id ]
	 | (name, EnumName) -> [])
       env.struct_name_table;
     Ctt_abstree.global_declarations = [] }
    t

let filter_definitions ~env t = 
  (* remove all duplicate definitions, except for the last one. *)
  let acc = ref [] in
  let known_name = Hashtbl.create 16 in
  dprintf 5 "filtering results...";
  List.iter
    (fun d -> match locval d with
	CTTdeclFunction(gsclass, typ, ident, argnames, body) -> begin
	  dprintf 5 "Func: %s" ident;
	  assert (not (Hashtbl.mem known_name ident));
	  Hashtbl.add known_name ident ();
	  let gb = Hashtbl.find env.global_binding ident in
	  match gb.gbind_type with
	    Var ({ ct_ty = Tfunction _ } as t) ->
	      acc := 
		loccopy ~orig:d
		  (CTTdeclFunction (gb.gbind_storage_class, t, ident, argnames, body))
		:: !acc;
	  | _ -> assert false
	end
      | CTTdeclVariable(gsclass, typ, ident, initopt) ->
	  dprintf 5 "Var: %s" ident;
	  if Hashtbl.mem known_name ident then
	    ()
	  else begin
	    Hashtbl.add known_name ident ();
	    let gb = Hashtbl.find env.global_binding ident in
	    match gb.gbind_type with
	      Var t ->
		acc :=
		  loccopy ~orig:d
		    (CTTdeclVariable (gb.gbind_storage_class, t, ident, initopt))
		  :: !acc;
	    | _ -> assert false
	  end)
    (List.rev t);
  (* add locally-declared extern functions *)
  dprintf 5 "scanning for local declaration.";
  Hashtbl.iter
    (fun nam gb ->
      dprintf 5 "  %s" nam;
      if not (Hashtbl.mem known_name nam) then begin
	dprintf 5 "    found non-emitted decl %s" nam;
	assert (gb.gbind_is_initialized = false);
	match gb.gbind_type with
	  Var t ->
	    acc := locput ~loc:(gb.gbind_loc) (CTTdeclVariable(gb.gbind_storage_class, t, nam, None)) :: !acc;
	| TypeDefName _ | EnumVal _ -> ()
      end)
    env.global_binding;
	
  dprintf 5 "Done.";
  !acc

let parse_global_declarations decl = 
  let env = empty_environment decl in
  try
    (* Format.eprintf "Before Parse: %d.@." (List.length decl); *)
    let t = parse_global_declarations [] ~env decl in
    (* Format.eprintf "After Parse: %d.@." (List.length t); *)
    let t = filter_definitions ~env t in
    let genv = filter_environment env t in
    let genv, t = Update_struct_types.reduce_program ~genv t in
    genv, t
  with
    TypeError_typed(e, msg) ->
      let l = strof_location (locget e) in
      Format.eprintf "%s: %a: %s@." l
	Ctt_formatter.pp_expr e msg;
      flush stderr;
      failwith "type check failed (1)"
  | TypeError_untyped(e, msg) ->
      let l = strof_location (locget e) in
      Format.eprintf "%s: %a: %s@." l 
	C_pp.pp_print_expression e 
	msg;
      flush stderr;
      failwith "type check failed (2)"
  | TypeError_located(loc, msg) ->
      let l = strof_location loc in
      Format.eprintf "%s: %s@." l msg;
      flush stderr;
      failwith "type check failed (3)"
