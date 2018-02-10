(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2003-2009. *)

open Util
open Big_int_infix
open Locterm
open Ctt_abstree
open Cttm_abstree
open Ils
open Ilc
open Il
open Il0
open Il3
open Transutil
open Record_globalnames

include Debug.Install (struct let category = 150 end)

module VarMap = Map.Make(struct type t = int 
  let compare (x:t) (y:t) = compare x y end)

let formatter f = fun pp v -> Format.pp_print_string pp (f v)

let clear_castflag_at_memop = Transutil.clear_castflag_at_memop (* ref true *)

type compiler_mode = Transutil.compiler_mode = 
    MultiModule | MultiLinker | StandAlone | TrustInterModule | StdlibImplementation

let compiler_mode = Transutil.compiler_mode (* ref StandAlone *)

let max_local_var_size = Transutil.max_local_var_size (* ref (big_int_of_int 0x2000000) *)

let use_signed_compare_for_offset = Transutil.use_signed_compare_for_offset (* ref false *)

let force_emit_stack_unwind = Transutil.force_emit_stack_unwind (* ref false *)

let use_optimized_narrow_cast = Transutil.use_optimized_narrow_cast (* ref true *)

let use_boundary_t = Transutil.use_boundary_t (* ref true *)

type decl_type = Known | Unknown_monomorphic | Unknown_polymorphic

type decl_environment = (identifier, decl_type) Hashtbl.t

let make_il0 = Il0.make_il0

type e_or_r =
  | IEXP of temp_id il3_irreducible_exp
  | REXP of temp_id il3_reducible_exp
  | MOVE of temp_id
  | RF of primitive_reducible * temp_id list

let emit_e_or_r ~loc target ttype = function
  | IEXP e -> 
      make_il3 ~loc (IL3stmtIexp(target, ttype, e))
  | REXP e -> 
      make_il3 ~loc (IL3stmtRexp(target, ttype, e))
  | RF (rf, args) ->
      make_il3 ~loc (IL3stmtCallReducibleFun(target, rf, args))
  | MOVE i ->
      make_il3 ~loc (IL3stmtMove(target, ttype, i))

type size_info = 
    Size_byte | Size_hword | Size_word | Size_dword
  | Size_other of big_int option

type convfuncs = (c_type * temp_id il_lvalue option) list

type environment =
    {
     mutable max_var : int;
     f : ilc_function;
     is_polymorphic : bool;
     global_env : Ctt_abstree.environment;
     decl_env : decl_environment;
     rettype : c_type;
     arguments_b : int earray;
     arguments_v : int earray;
     arguments_n : int earray;
     prologue0 : il0 Glist.t;
     prologue1 : temp_id il3 Glist.t;
     epilogue : temp_id il3 Glist.t;
     mutable exit_code : temp_id il3_desc list; (* location is put afterward *)
     struct_is_continuous : bool earray;
     mutable boundary_cache : temp_id VarMap.t;
     name : identifier;
   }

let constant_zero = CTTconstInteger(zero_big_int)
let constant_one = CTTconstInteger(unit_big_int)

let rettype_of_functype t =
  match t.ct_ty with
    Tfunction(_,_,rt) -> rt
  | _ -> assert false

let new_tempid ~env =
  let v = env.max_var in
  env.max_var <- v + 1;
  v

let new_temp_label tid l =
  Printf.sprintf "LL_%d_%d" tid l

let type_of_tempvar ~env t =
  try
    env.f.Il.variable_environment.(t).variable_type
  with
    Invalid_argument("Array.get") ->
      Printf.eprintf "panic: tries to get type of var #%d (max is %d)"
	t (Array.length env.f.Il.variable_environment);
      assert false

let sizeof_type_genv ~genv t =
  match Ctt_abstree.size_of_type ~genv t with
    Some s -> s
  | None ->
      dprintf (-1) "panic: tries to get size of %a" Il_formatter.pp_ctt_type t;
      assert false

let sizeof_type ~env t =
  sizeof_type_genv ~genv:env.global_env t

let is_numeric t = 
  match t.ct_ty with
    Tbuiltin _ -> true
  | _ -> false

let is_pointer t = 
  match t.ct_ty with
    Tpointer _ -> true
  | _ -> false

let size_is_dword s =
  eq_big_int s
    (big_int_of_int Fsc_config.sizeof_longlong)

let type_boundary_t = make_c_type (Tabstract "boundary_info_t")
let type_base_t = make_c_type (Tabstract "base_t")
let type_ofs_t = make_c_type (Tabstract "ofs_t")
let type_signed_ofs_t = make_c_type (Tabstract "signed_ofs_t")
let type_vaddr_t = make_c_type (Tabstract "vaddr_t")
let type_word = make_c_type (Tabstract "word")
let type_dword = make_c_type (Tabstract "dword")

let type_byte = make_c_type (Tabstract "byte")
let type_hword = make_c_type (Tabstract "hword")
let type_value = make_c_type (Tabstract "value")
let type_dvalue = make_c_type (Tabstract "dvalue")
let type_ptrvalue = make_c_type (Tabstract "ptrvalue")
let type_ptrvalue_ptr = make_c_type (Tpointer type_ptrvalue)

let type_void_ptr = make_c_type (Tpointer type_void)

let type_ptrinit = make_c_type (Tabstract "union initU_ptrvalue")
let type_valinit = make_c_type (Tabstract "union initU_value")
let type_dvalinit = make_c_type (Tabstract "union initU_dvalue")

let type_fsc_error = make_c_type (Tabstract "enum fsc_error")
let type_typeinfo_init = make_c_type (Tabstract "struct typeinfo_init")
let type_typeinfo_struct = make_c_type (Tabstract "struct typeinfo_s")
let type_function_stub_init = make_c_type (Tabstract "struct fsc_function_stub_init")
let type_function_stub = make_c_type (Tabstract "struct fsc_function_stub")
let type_genfunc = make_c_type (Tfunction ([type_base_t; type_base_t], false, type_dvalue))

let type_stack_frame = make_c_type (Tabstract "struct fsc_stack_frame")

let type_frame_variables_entry = make_c_type (Tpointer (make_c_type ~volatile:true Tvoid))

let type_frame_variables = make_c_type (Tarray (type_frame_variables_entry, None))

let type_frame_variables_n n = make_c_type (Tarray (type_frame_variables_entry, Some (big_int_of_int n)))

let id_local_frame = "fsc_local_stack_frame"
let id_local_frame_variables = "fsc_local_stack_frame_variables"

let lv_local_frame_variables = ILlvVar(id_local_frame_variables, type_frame_variables)

(* predefined macros and types for constant initilizers *)

let type_fsc_header =
  make_c_type (Tabstract "struct fsc_header")

let make_cttm_func_lv name argtype rettype =
  CTTMlvVar(name, make_c_type (Tfunction(argtype, false, rettype)))

let lv_cttm_macro_emit_header =
  make_cttm_func_lv "EMIT_FSC_HEADER"
    [type_typeinfo_struct; type_size_t] type_fsc_header

let encoded_name_of_btype = function
    Tchar | Tschar | Tuchar -> "c"
  | Tshort | Tushort -> "s"
  | Tint | Tuint -> "i"
  | Tlong | Tulong -> "l"
  | Tlonglong | Tulonglong -> "q"
  | Tfloat -> "f"
  | Tdouble | Tlongdouble -> "d"

let encoded_name_of_struct ~genv id = 
  let sd = get_struct_desc ~genv id in
  try
    match List.assoc "named" sd.str_extension with
      C_abstree.Estring n ->
	"Sn" ^ string_of_int(String.length n) ^ n ^ "_"
    | _ -> assert false
  with
    Not_found -> begin
      match !compiler_mode with
	TrustInterModule -> begin
	let p = get_struct_desc ~genv id in
	match p.str_loc with
	  Located (fname, line, col, _) ->
	    Printf.sprintf "Sl_%s_%d_%d_" (String.sub (Digest.to_hex (Digest.string fname)) 0 8) line col
	| Unlocated ->
	    failwith "panic 223: no location information for struct"
	end 
      | StdlibImplementation -> failwith "all structs must be explicitly named in stdlib_impl mode"
      | StandAlone | MultiModule | MultiLinker ->
	Printf.sprintf "Sh_%s_%d_" genv.module_hash id
    end

let rec encoded_name_of_type_genv ~genv ?(array_squash = false) t =
  match t.ct_ty with
    Tvoid -> "v"
  | Tbuiltin bt -> encoded_name_of_btype bt
  | Tpointer t -> "P" ^ encoded_name_of_type_genv ~genv t
  | Tfunction(at,varp,rt) -> 
      "F" ^ String.concat "" (list_map (encoded_name_of_type_genv ~genv) at)
      ^ (if varp then "V" else "") ^ "_" ^ encoded_name_of_type_genv ~genv rt
  | Tstruct(id) -> encoded_name_of_struct ~genv id
  | Tarray(t,_) when array_squash ->
      "A" ^ encoded_name_of_type_genv ~genv t ^ "_" ^ "0"
  | Tarray(t,Some size) -> 
      "A" ^ encoded_name_of_type_genv ~genv t ^ "_" ^ 
      string_of_big_int size
  | _ -> failwith_p "panic translate:345: %b: %a" array_squash Il_formatter.pp_ctt_type t

let encoded_name_of_type ~env t = 
  encoded_name_of_type_genv ~genv:env.global_env t

let encoded_typeinfo_name ~genv t = 
  "fsc_typeinfo_" ^ encoded_name_of_type_genv ~genv t

let _ =
  Ctt_to_ptree.typeinfo_converter_hook :=
    (fun ~genv ~make t -> 
      require_name (GNtypeinfo t);
      (C_abstree.PexpAddress
	 (make
	    (C_abstree.PexpField
	       (make (C_abstree.PexpVar(encoded_typeinfo_name ~genv t)),
		"val")))))

let get_storage_type_array ~genv t s = 
  require_name (GNstorage(t, s));
  let ss = match s with
    None -> "s"
  | Some s -> string_of_big_int s
  in
  make_c_type (Tabstract ("struct fsc_storage_"
			  ^ encoded_name_of_type_genv ~genv t
			  ^ "_" ^ ss))

let size_and_elemtype_of_onmemory_type ?(incomplete_ok = false) t = 
  match t.ct_ty with
    Tarray(et, Some s) -> Some s, et
  | Tarray(et, None) ->
      if incomplete_ok then None, et else assert false
  | _ -> None, t

let storage_for_typeinfo_constant ~genv t = 
  let _, elemtype = size_and_elemtype_of_onmemory_type t in
  Record_globalnames.require_name (GNtypeinfo elemtype);
  let id = encoded_typeinfo_name ~genv elemtype in
  type_typeinfo_struct, type_typeinfo_init, ILlvVar(id, type_typeinfo_init),
  ["val", type_typeinfo_struct]

let make_compound ~env ~loc t t1 t2 =
  let temp0 = new_tempid ~env in
  let typ = 
    match t with
      ({ct_ty = Tbuiltin bt} as t') -> begin
	match bt with
	  Tdouble | Tfloat -> assert false
	| Tlonglong | Tulonglong -> Dvalue
	| _ -> Value
      end
    | ({ct_ty = Tpointer pt} as t') -> begin
	PtrValue
    end
    | _ -> assert false
  in
  temp0, 
  [make_il3 ~loc
     (IL3stmtCallReducibleFun
	(temp0, IL3pr_cons typ, [t1; t2]))]

type type_properties = {
    size : size_info;
    is_struct : bool;
    is_pointer : bool;
    is_fat : bool;
    is_floating : bool;
    is_indirect_ret : bool;
    
    packed_t : c_type;
    cdr_t : c_type;
    unpacked_types : c_type list;
    initializer_type : c_type;
(*    packed_inj_cdr : convfuncs; *)

    generic_t : c_type; (* the return type of read_****  *)
    samesized_word_t : c_type;

    packed_to_generic : Il3.primitive_reducible option;
    generic_to_packed : Il3.primitive_reducible option;
    rw_type : Il3.rw_targets;
    generic_cdr_t : c_type;
    
(*    gen_inj_car : convfuncs;
    gen_inj_cdr : convfuncs;*)
  }

let parse_type_genv ~genv t = 
  match t.ct_ty with
    Tpointer _ ->
      assert (Fsc_config.sizeof_int = Fsc_config.sizeof_pointer);
      { size = Size_word;
	is_struct = false;
	is_pointer = true;
	is_fat = true;
	is_floating = false;
	is_indirect_ret = false;
	packed_t = type_ptrvalue;
	cdr_t = type_ofs_t;
	unpacked_types = [type_base_t; type_ofs_t];
(*	packed_inj_cdr = [type_ofs_t, Some lv_ofs_of_ptrvalue]; *)
	initializer_type = type_ptrinit;
	generic_t = type_value;
	packed_to_generic = Some (IL3pr_convert Value_of_ptrvalue);
	samesized_word_t = type_size_t;
	generic_to_packed = begin
	  require_name (GNbasecast t);
	  Some (IL3pr_convert (Ptrvalue_of_value t))
	end;
	generic_cdr_t = type_vaddr_t;
        rw_type = Word;
(*	gen_inj_car = [type_base_t, Some lv_base_of_value];
	gen_inj_cdr = [type_ofs_t, Some lv_ofs_of_value]; *)
      }
  | Tbuiltin bt -> begin
      let s = sizeof_type_genv ~genv t in
      assert (is_int_big_int s);
      let s = int_of_big_int s in
      let is_real = 
	match bt with
	  Tfloat | Tdouble | Tlongdouble -> true
	| _ -> false
      in
      let can_be_fat =
	s >= Fsc_config.sizeof_pointer && not is_real
      in
      let size = 
	if s = Fsc_config.sizeof_char then Size_byte
	else if s = Fsc_config.sizeof_short then Size_hword
	else if s = Fsc_config.sizeof_long then Size_word
	else if s = Fsc_config.sizeof_longlong then Size_dword
	else assert false
      in
      let genconv, genunconv, is_floating =
	match bt with
	  Tdouble -> Some (IL3pr_convert Double_of_dvalue), Some (IL3pr_convert Dvalue_of_double), true
	| Tfloat -> Some (IL3pr_convert Float_of_value), Some (IL3pr_convert Value_of_float), true
	| Tlongdouble -> assert false
	| _ -> None, None, false (* cast *)
      in
      let t = { t with ct_const_p = false } in
      match size, can_be_fat with
	Size_byte, false ->
	  { size = Size_byte;
	    is_struct = false;
	    is_pointer = false;
	    is_fat = false;
	    is_floating = is_floating;
	    is_indirect_ret = false;
	    packed_t = t;
	    cdr_t = t;
(*	    packed_inj_cdr = []; *)
	    unpacked_types = [t];
	    initializer_type = t;
	    generic_t = type_byte;
	    samesized_word_t = type_byte;
	    packed_to_generic = genunconv;
	    generic_to_packed = genconv;
	    generic_cdr_t = type_byte;
	    rw_type = Byte;
(*	    gen_inj_car = [];
	    gen_inj_cdr = [t, genconv]; *)
	  }
      |	Size_hword, false ->
	  { size = Size_hword;
	    is_struct = false;
	    is_pointer = false;
	    is_fat = false;
	    is_floating = is_floating;
	    is_indirect_ret = false;
	    packed_t = t;
	    cdr_t = t;
(*	    packed_inj_cdr = []; *)
	    unpacked_types = [t];
	    initializer_type = t;
	    packed_to_generic = genunconv;
	    generic_to_packed = genconv;
	    generic_t = type_hword;
	    samesized_word_t = type_hword;
	    generic_cdr_t = type_hword;
            rw_type = Hword;
(*	    gen_inj_car = [];
	    gen_inj_cdr = [t, genconv];*)
	}
      |	Size_word, false ->
	  assert (is_floating);
	  { size = Size_word;
	    is_struct = false;
	    is_pointer = false;
	    is_fat = false;
	    is_floating = is_floating;
	    is_indirect_ret = false;
	    packed_t = t;
	    cdr_t = t;
(*	    packed_inj_cdr = []; *)
	    unpacked_types = [t];
	    initializer_type = t;
	    packed_to_generic = genunconv;
	    generic_to_packed = genconv;
	    generic_t = type_value;
	    samesized_word_t = type_word;
	    generic_cdr_t = type_vaddr_t;
            rw_type = Word;
(*	    gen_inj_car = [];
	    gen_inj_cdr =
	    (if genconv = None then [t, genconv; type_word, Some lv_vaddr_of_value]
	    else [t, genconv]);*)
	}
      |	Size_word, true ->
	  assert (genconv = None);
	  { size = Size_word;
	    is_struct = false;
	    is_pointer = false;
	    is_fat = true;
	    is_floating = is_floating;
	    is_indirect_ret = false;
	    packed_t = type_value;
	    initializer_type = type_valinit;
	    packed_to_generic = genunconv;
	    generic_to_packed = genconv;
	    cdr_t = t;
(*	    packed_inj_cdr = [t, genconv; type_word, Some lv_vaddr_of_value]; *)
	    unpacked_types = [type_base_t; t];
	    generic_t = type_value;
	    samesized_word_t = type_word;
	    generic_cdr_t = type_vaddr_t;
            rw_type = Word;
(*	    gen_inj_car = [type_base_t, Some lv_base_of_value];
	    gen_inj_cdr = [t, genconv; type_word, Some lv_vaddr_of_value];*)
	}
      |	Size_dword, false ->
	  assert (is_floating);
	  { size = Size_dword;
	    is_pointer = false;
	    is_struct = false;
	    is_fat = false;
	    is_floating = is_floating;
	    is_indirect_ret = false;
	    packed_t = t;
	    initializer_type = t;
	    packed_to_generic = genunconv;
	    generic_to_packed = genconv;
	    cdr_t = t;
(*	    packed_inj_cdr = []; *)
	    unpacked_types = [t];
	    generic_t = type_dvalue;
	    samesized_word_t = type_dword;
	    generic_cdr_t = type_dword;
            rw_type = Dword;
(*	    gen_inj_car = [];
	    gen_inj_cdr = 
	    if genconv = None then [t, genconv; type_dword, Some lv_vaddr_of_dvalue]
	    else [t, genconv];*)
	  }
      |	Size_dword, true ->
	  assert (genconv = None);
	  { size = Size_dword;
	    is_struct = false;
	    is_pointer = false;
	    is_fat = true;
	    is_floating = is_floating;
	    is_indirect_ret = false;
	    packed_t = type_dvalue;
	    initializer_type = type_dvalinit;
	    cdr_t = t;
(*	    packed_inj_cdr = [t, genconv; type_dword, Some lv_vaddr_of_dvalue]; *)
	    unpacked_types = [type_base_t; t];
	    generic_t = type_dvalue;
	    samesized_word_t = type_dword;
	    packed_to_generic = genunconv;
	    generic_to_packed = genconv;
	    generic_cdr_t = type_dword;
            rw_type = Dword;
(*	    gen_inj_car = [type_base_t, Some lv_base_of_dvalue];
	    gen_inj_cdr = [t, genconv; type_dword, Some lv_vaddr_of_dvalue];*)
	  }
      |	_ -> assert false

  end
  | Tstruct id ->
      let sid = encoded_name_of_struct ~genv id in
      let et = make_c_type (Tabstract ("struct struct_" ^ sid)) in
      { size = Size_other (t.ct_size);
	is_pointer = false;
	is_struct = true;
	is_fat = false;
	is_floating = false;
	is_indirect_ret = true;
	packed_t = et;
	initializer_type = et;
(*	packed_inj_cdr = []; *)
	cdr_t = et;
	unpacked_types = [et];
	generic_t = t;
	samesized_word_t = et;
	packed_to_generic = None;
	generic_to_packed = None;
	generic_cdr_t = et;
        rw_type = RWstruct id;
(*	gen_inj_car = [];
	gen_inj_cdr = [];*)
      }
  | Tfunction _ ->
      let t = type_function_stub in
      {
       size = Size_other None;
       is_pointer = false;
       is_struct = false;
       is_fat = false;
       is_floating = false;
       is_indirect_ret = false;
       packed_t = t;
       initializer_type = t;
(*       packed_inj_cdr = []; *)
       cdr_t = t;
       unpacked_types = [t];
       generic_t = t;
       samesized_word_t = t;
       packed_to_generic = None;
       generic_to_packed = None;
       generic_cdr_t = t;
       rw_type = RWnone;
(*       gen_inj_car = [];
       gen_inj_cdr = [];*)
     }
  | _ -> failwith_p "unimp 366 %a" Il_formatter.pp_ctt_type t

let parse_type ~env =
  parse_type_genv ~genv:env.global_env

let rec is_type_continous ~env t = 
  match t.ct_ty with
    Tvoid -> false
  | Tbuiltin _ ->
      not ((parse_type ~env t).is_fat)
  | Tpointer t -> false
  | Tarray(t,_) -> is_type_continous ~env t
  | Tabstract _ -> assert false
  | Tfunction _ -> false
  | Tstruct id -> begin
      match Earray.get_option env.struct_is_continuous id with
	Some b -> b
      | None ->
	  let st = get_struct_desc ~genv:env.global_env id in
	  let b = 
	    List.for_all
	      (function
		  (_, NormalField { sf_type = t }) -> is_type_continous ~env t
		| (_, BitField _) -> assert false)
	      st.str_fields
	  in
	  Earray.set env.struct_is_continuous id b;
	  b
  end

let rf_get_realoffset ~env ty =
  match ty.ct_ty with
    Tpointer _ -> type_ptrvalue_ptr, IL3pr_misc Get_realoffset_pointer
  | Tbuiltin _ | Tstruct _ ->
      let p = parse_type ~env ty in
      (*require_name (GNgetrealofs ty);*)
      let resulttype = make_c_type (Tpointer p.packed_t) in
      resulttype, IL3pr_misc (Get_realoffset ty)
  | _ -> assert false

let get_destructors ~env ttype = (* TODO: to be deprecated, but now active *)
  let p = parse_type ~env ttype in
  if p.packed_t == type_value then
    type_value, Value, type_vaddr_t
  else if p.packed_t == type_dvalue then
    type_dvalue, Dvalue, type_dword
  else if p.packed_t == type_ptrvalue then
    type_ptrvalue, PtrValue, type_ofs_t
  else
    (dprintf (-1) "panic: get_destructors: %a" Il_formatter.pp_ctt_type p.packed_t; assert false)

let get_pair_type ~env t = (parse_type ~env t).packed_t
let get_base_type ~env t = type_base_t
let get_ofs_or_value_type ~env t = 
  if is_pointer t then type_ofs_t else t

let get_generic_readers ~env ttype =
  let s = sizeof_type ~env ttype in
  assert (is_int_big_int s);
  let s = int_of_big_int s in
  if s = Fsc_config.sizeof_char then
    Byte, type_byte, None
  else if s = Fsc_config.sizeof_short then
    Hword, type_hword, None
  else if s = Fsc_config.sizeof_long then
    Word, type_value,
    Some (type_word, Value)
  else if s = Fsc_config.sizeof_longlong then
    Dword, type_dvalue, 
    Some (type_dword, Dvalue)
  else assert false

let get_generic_writers ~env ttype =
  let s = sizeof_type ~env ttype in
  assert (is_int_big_int s);
  let s = int_of_big_int s in
  if s = Fsc_config.sizeof_char then
    Byte, type_byte, None
  else if s = Fsc_config.sizeof_short then
    Hword, type_hword, None
  else if s = Fsc_config.sizeof_long then
    Word, type_value,
    Some (type_word, IL3pr_convert Value_of_int)
  else if s = Fsc_config.sizeof_longlong then
    Dword, type_dvalue, 
    Some (type_dword, IL3pr_convert Dvalue_of_dword)
  else assert false

let check_size_of_local_variable ~genv id cty =
  match size_of_type ~genv cty with
    None -> failwith "panic 2228: allocating size_unknown local variable"
  | Some s -> 
      if s >! !max_local_var_size then
	failwith_p "error 2229: too big local variable allocated: %s: %s" id (string_of_big_int s)

let make_coerce_from_vaddr typ from =
  match typ.ct_ty with
    Tbuiltin bt -> begin
      match bt with
	Tdouble ->
	  RF (IL3pr_convert Double_of_dword, [from])
      | Tfloat ->
	  RF (IL3pr_convert Float_of_word, [from])
      | _ ->
	  REXP (IL3RexCoerce(typ, from))
    end
  | _ ->
      dprintf (-1) "make_coerce_from_vaddr: from %a unimplemented\n" Il_formatter.pp_ctt_type typ;
      assert false

let make_coerce_to_vaddr typ typ_to from =
  match typ.ct_ty with
    Tbuiltin bt -> begin
      match bt with
	Tdouble ->
	  RF (IL3pr_convert Dword_of_double, [from])
      | Tfloat ->
	  RF (IL3pr_convert Word_of_float, [from])
      | _ ->
	  REXP (IL3RexCoerce(typ_to, from))
    end
  | _ -> assert false

let make_coerce_to_value2 typ =
  match typ.ct_ty with
    Tbuiltin bt ->
      let f = 
	match bt with
	  Tdouble | Tfloat -> assert false
	| Tlonglong | Tulonglong -> IL3pr_cons Dvalue
	| _ -> IL3pr_cons Value
      in
      f
  | Tpointer p ->
      IL3pr_convert Value_of_base_ofs
  | _ -> assert false

let make_specific_read1 ~env ~loc target ttype addr fields =
  let p = parse_type ~env ttype in
  let typ = p.packed_t in
  if p.is_struct then
    [ make_il3 ~loc (IL3stmtReadRaw(target, typ, ILlvPtr(addr), fields))  ]
  else
    let temp = new_tempid ~env in
    [ make_il3 ~loc (IL3stmtReadRaw(temp, typ, ILlvPtr(addr), fields)); (* TODO: ttype *)
      make_il3 ~loc (IL3stmtRexp(target, typ, IL3RexCoerce(typ, temp)))
    ]

let make_specific_write1 ~env ~loc source stype addr fields =
  [ make_il3 ~loc (IL3stmtWriteRaw(ILlvPtr(addr), fields, source)) ]

let make_specific_read2 ~env ~loc (target1,target2) ttype addr fields =
  let pairtype, injT, cdrtype = get_destructors ~env ttype in
  let typeB = get_base_type ~env ttype in
  let typeV = get_ofs_or_value_type ~env ttype in
  let tid0 = new_tempid ~env in
  let tid1 = new_tempid ~env in
  [ make_il3 ~loc (IL3stmtReadRaw(tid0, pairtype,ILlvPtr(addr), fields));
    make_il3 ~loc (IL3stmtCallReducibleFun (target1, IL3pr_car injT, [tid0]));
    make_il3 ~loc (IL3stmtCallReducibleFun (tid1, IL3pr_cdr injT, [tid0]));
    make_il3 ~loc (IL3stmtRexp(target2, typeV, IL3RexCoerce(typeV, tid1)));
  ]

let make_specific_write2 ~env ~loc (source1,source2) stype addr fields =
  let id, insn = make_compound ~env ~loc stype source1 source2 in
  (insn @ [make_il3 ~loc (IL3stmtWriteRaw (ILlvPtr(addr), fields, id))])

let make_generic_read1 ~env ~loc target ttype sttype base ofs =
  let p = parse_type ~env ttype in
  if p.is_struct then begin
    require_name (GNrwmeth ttype);
    [ make_il3 ~loc (IL3stmtCallReaderHelper(target, p.rw_type, base, ofs)) ]
  end else
  let rw_targets, t_tid, inject = 
    get_generic_readers ~env ttype
  in
  let tid = new_tempid ~env in
  let s1 = 
    [ make_il3 ~loc (IL3stmtCallReaderHelper(tid, rw_targets, base, ofs)) ]
  in
  let tid2, s2 = match inject with
    Some (typ, rwtype) ->
      let tid2 = new_tempid ~env in 
      tid2, [
      make_il3 ~loc (IL3stmtCallReducibleFun(tid2, IL3pr_cdr rwtype, [tid]))
    ]
  | None -> 
      tid, []
  in
  let s3 = emit_e_or_r ~loc target ttype (make_coerce_from_vaddr ttype tid2)
  in
  s1 @ s2 @ [s3]

let make_generic_read2 ~env ~loc (t1,t2) ttype sttype base ofs =
  let rw_targets, t_tid, inject = 
    get_generic_readers ~env ttype
  in
  let tid = new_tempid ~env in
  let s1 = 
    [
     make_il3 ~loc (IL3stmtCallReaderHelper(tid, rw_targets, base, ofs));
   ]
  in
  let s2 =
    if is_pointer ttype then begin
      assert (rw_targets == Word);
      let tid1 = new_tempid ~env in 
      [
       make_il3 ~loc (IL3stmtCallReducibleFun(tid1, IL3pr_car Value, [tid]));
       make_il3 ~loc (IL3stmtCallReducibleFun(t2, IL3pr_convert Ofs_of_value, [tid]));
       make_il3 ~loc (IL3stmtCallReducibleFun(t1, IL3pr_misc (Set_base_castflag ttype), [tid1; t2]))
     ]
    end
    else if is_numeric ttype then
      let tid2 = new_tempid ~env in 
      match inject with
	Some (typ, rwtype) ->
	  [
	   make_il3 ~loc (IL3stmtCallReducibleFun(t1, IL3pr_car rwtype, [tid]));
	   make_il3 ~loc (IL3stmtCallReducibleFun(tid2, IL3pr_cdr rwtype, [tid]));
	   emit_e_or_r ~loc t2 ttype (make_coerce_from_vaddr ttype tid2)
	 ]
      | None -> assert false
    else assert false
  in
  s1 @ s2

let make_generic_write1 ~env ~loc source ttype sttype base ofs =
  let p = parse_type ~env ttype in
  if p.is_struct then begin
    require_name (GNrwmeth ttype);
    [ make_il3 ~loc
	(IL3stmtCallWriterHelper(p.rw_type, base, ofs, source, None)) ]
  end else
  let tnull = new_tempid ~env in
  let rw_targets, t_tid, proj = 
    get_generic_writers ~env ttype
  in
  let tid1_t = match proj with None -> t_tid | Some(t,_) -> t in 
  let tid1 = new_tempid ~env in
  let s3 = 
    emit_e_or_r ~loc tid1 tid1_t (make_coerce_to_vaddr ttype t_tid source)
  in
  let tids, s2 = match proj with
    Some (typ, vfunc) ->
      let tid2 = new_tempid ~env in 
      tid2, [
      make_il3 ~loc (IL3stmtCallReducibleFun(tid2, vfunc, [tid1]))
    ]
  | None -> 
      tid1, []
  in
  let exp_strtinfo =
    match sttype with
      None -> IL3RexConstant(CTTconstNull)
    | Some st ->
	let t_elem, t_st, lv, fl = storage_for_typeinfo_constant ~genv:env.global_env st in
	IL3RexAddress(lv,fl)
  in
  let s1 = 
    [
     make_il3 ~loc (IL3stmtRexp(tnull, type_typeinfo_ptr, exp_strtinfo));
     make_il3 ~loc
       (IL3stmtCallWriterHelper
	  (rw_targets, base, ofs, tids, Some tnull));
   ]
  in
  s3 :: (s2 @ s1)

let make_generic_write2 ~env ~loc (src1,src2) ttype sttype base ofs =
  let tnull = new_tempid ~env in
  let rw_targets, t_tid, proj = 
    get_generic_writers ~env ttype
  in
  let tid1 = new_tempid ~env in
  let s3 = 
    make_il3 ~loc
      (IL3stmtCallReducibleFun(tid1, make_coerce_to_value2 ttype, [src1; src2]))
  in
  let exp_strtinfo =
    match sttype with
      None -> IL3RexConstant(CTTconstNull)
    | Some st ->
	let t_elem, t_st, lv, fl = storage_for_typeinfo_constant ~genv:env.global_env st in
	IL3RexAddress(lv,fl)
  in
  let s1 = 
    [
     make_il3 ~loc (IL3stmtRexp(tnull, type_typeinfo_ptr, exp_strtinfo));
     make_il3 ~loc 
       (IL3stmtCallWriterHelper(rw_targets, base, ofs, tid1, Some tnull))
   ]
  in
  s3 :: s1
	   
let emit_exit_code ~env ~loc = 
  list_map (make_il3 ~loc) env.exit_code

let rettype ~env = env.rettype

let translate_type ~env t =
  match t with
    ILRtypeVal ({ ct_ty = (Tbuiltin _ | Tvoid) } as t', _) -> t'
  | ILRtypeVal ({ ct_ty = Tstruct _ } as t', _) -> 
      (parse_type env t').packed_t
  | ILRtypeVal (t', _) ->
      failwith_p "panic: translate_type val %a" Il_formatter.pp_ctt_type t';
      (* assert false *)
  | ILRtypeOfs (t,_) -> type_ofs_t
  | ILRtypeBase (t,_) -> type_base_t
  | ILRtypeBaseTemp (t,_) -> type_base_t

let translate_ilstype ~env t =
  match t with
    ILStypeVal ({ ct_ty = Tbuiltin bt } as t') -> t'
  | ILStypeVal _ -> assert false
  | ILStypeOfs (t) -> type_ofs_t
  | ILStypeBase (t) -> type_base_t
  | ILStypeBaseTemp (t) -> type_base_t

let original_type_from_val_type = function
    ILRtypeVal(t,_) | ILRtypeOfs(t,_) -> t
  | _ -> assert false

let is_type_polymorphic_func ~genv t = 
    match t.ct_ty with
      Tfunction(_,_,{ ct_ty = Tpointer { ct_ty = Tvoid }}) -> true
    | Tfunction(_,_,_) -> false
    | _ -> assert false

let rec translate_c_type_genv ~genv t = 
  match t.ct_ty with
    Tvoid -> t
  | Tbuiltin _
  | Tpointer _
  | Tstruct _ ->
      (parse_type_genv ~genv t).packed_t
  | Tfunction(at, varp, rt) ->
      let new_at =
	List.fold_right (translate_c_type_funcarg ~genv)
	  at (if varp then [type_base_t; type_ofs_t] else [])
      in
      let new_at = 
	if is_type_polymorphic_func ~genv t then type_base_t :: new_at else new_at
      in
      make_c_type (Tfunction(new_at, false, translate_c_type_genv ~genv rt))
  | Tarray _ -> assert false
  | Tabstract _ -> assert false

and translate_c_type_funcarg ~genv t acc = 
  match t.ct_ty with
    Tvoid -> assert false
  | Tbuiltin _
  | Tpointer _
  | Tstruct _ -> 
      ((parse_type_genv ~genv t).unpacked_types) @ acc
  | Tfunction _
  | Tarray _
  | Tabstract _ -> assert false

let translate_c_type ~env =
  translate_c_type_genv ~genv:env.global_env

let foldr_with_argtypes ~env t ~varargsf ~widef ~narrowf l r = 
  match t.ct_ty with
    Tfunction(at, vp, _) -> begin
      let rec loop l at = 
	match at, l with
	  [], [] ->
	    if vp then varargsf [] r else r
	| [], _::_ ->
	    if vp then varargsf l r
	    else failwith "fold_with_argtypes: too many args"
	| _::_, [] ->
	    failwith "fold_with_argtypes: not enough args"
	| t::ts, e::rest ->
	    let p = parse_type ~env t in
	    if p.is_fat then
	      widef t e (loop rest ts)
	    else
	      narrowf t e (loop rest ts)
      in
      loop l at
    end
  | _ -> assert false

let flatten_mapi_with_argtypes ~env t ~varargsf ~widef ~narrowf l = 
  let varargsf l r = (varargsf (list_map snd l)) @ r in
  let widef t (i, x) r = (widef i t x) @ r in
  let narrowf t (i, x) r = (narrowf i t x) @ r in
  let rec loop i = function
      [] -> []
    | h::t -> (i, h) :: (loop (i + 1) t)
  in
  let l = loop 0 l in
  foldr_with_argtypes ~env t ~varargsf ~widef ~narrowf l []

let rec translate_func_concrete_args ~env ft args = 
  let rest_insns = ref [] in
  let narrowf _ argt arg = 
    match arg with
      ILSFuncArgNarrow n -> [n]
    | ILSFuncArgWide _ -> assert false
  in
  let widef _ argt arg = 
    match arg with
      ILSFuncArgNarrow n -> assert false
    | ILSFuncArgWide (t1,t2) -> [t1; t2]
  in
  let varargsf l = assert (l = []); [] in
  let args = flatten_mapi_with_argtypes ~env ft ~varargsf ~widef ~narrowf args
  in
  !rest_insns, args

let rec translate_func_varargs ~env ~loc args = 
  let length = ref 0 in
  let base_id = new_tempid ~env in
  let arglen_id = new_tempid ~env in
  let ofs_id = new_tempid ~env in
  let insns = ref [] in
  let add_insns l = insns := !insns @ l in
  List.iter
    (fun t ->
      let tidpos = new_tempid ~env in
      let pos_insn = 
	make_il3 ~loc (IL3stmtRexp(tidpos, type_size_t, 
			      IL3RexConstant(CTTconstInteger(big_int_of_int !length)))) in
      let is_struct, tid1, typ, insns = 
	match t with
	  ILSFuncArgWide (t1, t2) ->
	    let tid1 = new_tempid ~env in
	    let typ =
	      match type_of_tempvar ~env t1 with
		ILRtypeBase (t, _) -> t | _ -> assert false in
	    let p = parse_type ~env typ in
	    let arg_insn = 
	      make_il3 ~loc (IL3stmtCallReducibleFun
			  (tid1, make_coerce_to_value2 typ, [t1; t2]))
	    in
	    false, tid1, typ, [arg_insn]
	| ILSFuncArgNarrow t1 ->
	    let typ = match type_of_tempvar ~env t1 with ILRtypeVal (t, _) -> t | _ -> assert false in
	    let p = parse_type ~env typ in
	    if p.is_struct then
	      true, t1, typ, []
	    else
	    let tid2 = new_tempid ~env in
	    let tid1 = new_tempid ~env in
	    let arg_insn1 = 
	      emit_e_or_r ~loc tid2 p.samesized_word_t
		(make_coerce_to_vaddr typ p.samesized_word_t t1)
	    in
	    let tid0 = new_tempid ~env in
	    let nullbase_insn = 
	      make_il3 ~loc (IL3stmtRexp(tid0, type_base_t, 
					 IL3RexConstant(constant_zero))) in
	    let arg_insn2 =
	      let f = match p.size with
		Size_byte | Size_hword | Size_word -> Value
	      | Size_dword -> Dvalue
	      | Size_other _ -> assert false
	      in
	      make_il3 ~loc
		(IL3stmtCallReducibleFun(tid1, IL3pr_cons f, [tid0; tid2]))
	    in
	    false, tid1, typ, [arg_insn1; nullbase_insn; arg_insn2]
      in
      let p = parse_type ~env typ in
      let arglength = int_of_big_int (someof typ.ct_size) in
      if is_struct then begin
	(* TODO: directly-written *)
	require_name (GNrwmeth typ);
	let words = (arglength + Fsc_config.sizeof_pointer - 1) / Fsc_config.sizeof_pointer in
	let pos_insn = 
	  make_il3 ~loc
	    (IL3stmtRexp(tidpos, type_size_t, 
			 IL3RexConstant
			   (CTTconstInteger(big_int_of_int (!length * Fsc_config.sizeof_pointer))))) in
	add_insns [ pos_insn;
		    make_il3 ~loc
		      (IL3stmtCallWriterHelper
			 (p.rw_type, base_id, tidpos, tid1, None)) ];
	length := !length + words;
      end
      else 
      let pos_insn = 
	make_il3 ~loc
	  (IL3stmtRexp(tidpos, type_size_t, 
		       IL3RexConstant(CTTconstInteger(big_int_of_int !length)))) in
      if arglength <= Fsc_config.sizeof_pointer then begin
	(* one-word argument *)
	(* arglength 1, 2 may appear in code invoking cast function pointers *)
	add_insns (insns @ 
		   [pos_insn;
		    make_il3 ~loc
		      (IL3stmtCallImmobileOp
			 (Put_varargs, [base_id; tidpos; tid1]))]);
	length := !length + 1;
      end
      else if arglength = Fsc_config.sizeof_pointer * 2 then begin
	add_insns (insns @
		   [pos_insn;
		    make_il3 ~loc
		      (IL3stmtCallImmobileOp
			 (Put_varargs_2, [base_id; tidpos; tid1]))]);
	length := !length + 2;
      end
      else failwith_p "panic968: varargs size %d" arglength)
    args;
  if !length = 0 then
    [
     make_il3 ~loc (IL3stmtRexp(base_id, type_base_t, IL3RexConstant(CTTconstNull)));
     make_il3 ~loc (IL3stmtRexp(ofs_id, type_ofs_t, IL3RexConstant(constant_zero)))
   ], [base_id; ofs_id], []
  else
    let insns = 
      [
       make_il3 ~loc (IL3stmtRexp(arglen_id, type_size_t, 
			     IL3RexConstant(CTTconstInteger(big_int_of_int !length))));
       make_il3 ~loc (IL3stmtCallImmobileFun(base_id, Alloc_varargs, [arglen_id]));
       make_il3 ~loc (IL3stmtRexp(ofs_id, type_ofs_t, IL3RexConstant(constant_zero)))
     ] @ !insns
    in	      
    let post_insns =
      [ make_il3 ~loc
	  (IL3stmtCallImmobileOp
	     (Dealloc_varargs_finished, [base_id])) ]
    in
    [ enclose_il3_sequence ~loc insns ], [base_id; ofs_id], post_insns

let translate_funcargs ~env ~loc ~tptr_id t args = 
  let argtypes, varargs_p, rettype = 
    match t.ct_ty with
      Tfunction(a,v,r) -> a, v, r
    | _ -> assert false
  in
  let concreteargs, varargs = 
    if varargs_p then 
      Util.split_at_nth (List.length argtypes) args
    else begin
      assert(List.length argtypes = List.length args);
      args, []
    end
  in
  let insns1, new_concrete_args =
    translate_func_concrete_args ~env t concreteargs in
  let insns2, new_var_args, post_insns =
    if varargs_p then
      translate_func_varargs ~env ~loc varargs
    else
      [], [], []
  in
  let tptr_arg = if is_type_polymorphic_func ~genv:env.global_env t then [tptr_id] else [] in
  insns1 @ insns2, tptr_arg @ new_concrete_args @ new_var_args, post_insns

let type_of_union_initializer ~env elemtype = 
  make_c_type (Tabstract ("union fsc_initU" ^ encoded_name_of_type ~env elemtype))

let get_storage_type ~genv ?(incomplete_ok = false) t = 
  let elemnum, elemtype = size_and_elemtype_of_onmemory_type ~incomplete_ok t in
  let st = get_storage_type_array ~genv elemtype elemnum in
  { st with ct_volatile_p = t.ct_volatile_p; ct_const_p = false }

let translate_global_function_name ~genv s t = 
  if is_type_polymorphic_func ~genv t then
    "FSP_" ^ encoded_name_of_type_genv ~genv t ^ "_" ^ s 
  else "FS_" ^ encoded_name_of_type_genv ~genv t ^ "_" ^ s 

let translate_global_bridge_function_name ~genv s t = 
  "FG_" ^ s 

let translate_global_variable_name ~known ~genv s t = 
  if known = Unknown_polymorphic then
    "GV_Xuf__" ^ s
  else
    "GV_" ^ encoded_name_of_type_genv ~genv ~array_squash:true t ^ "_" ^ s

let translate_ptr_local_heap_variable_name ~genv s t = 
  "LHVP_" ^ encoded_name_of_type_genv ~genv ~array_squash:false t ^ "_" ^ s

let translate_local_stack_variable_name ~genv s t = 
  "LSV_" ^ encoded_name_of_type_genv ~genv ~array_squash:false t ^ "_" ^ s

let translate_field_name fn = "fld_" ^ fn

let translate_fields ~genv fl =
  match List.rev fl with
    [] -> []
  | (_,elemt)::_ ->
      let fl = list_map 
	  (fun (name,t) -> 
	    let p = parse_type_genv ~genv t in
	    (translate_field_name name, p.initializer_type)) fl in
      let p_elem = parse_type_genv ~genv elemt in
      if p_elem.is_fat then fl @ ["cv", p_elem.packed_t]
      else fl

let get_type_of_field vty fl = 
  let rec iter l = 
    match l with
      [] -> vty
    | [_, t] -> t
    | _::t -> iter t
  in
  iter fl

let storage_for_variable_internal ~genv f ~global vid vty fl = 
  let sz, top_t =
    (size_and_elemtype_of_onmemory_type ~incomplete_ok:true vty)
  in
  let ps = parse_type_genv ~genv top_t in
  let elemt = get_type_of_field top_t fl in
  let name = f ~genv vid vty in
  let sty = get_storage_type ~genv (if global then elemt else vty) in
  let field_add =
    ("val", ps.initializer_type) :: 
    if fl = [] && sz = None then begin
      if ps.is_fat then ["cv", ps.packed_t] else []
    end
    else
      translate_fields ~genv fl
  in
  elemt, sty, name, field_add

let get_global_string_storage_name ~genv id = 
  "GSTR_" ^ string_of_int id

let storage_for_global_string_internal ~genv s = 
  let id = Record_globalnames.get_string_constant_id s in
  let elemt = type_char in
  let ps = parse_type_genv ~genv elemt in
  let sty = get_storage_type ~genv elemt in
  let field = ["val", make_c_type(Tarray(ps.initializer_type, Some (big_int_of_int (String.length s))))] in
  elemt, sty, get_global_string_storage_name ~genv id, field

let cttm_storage_for_global_string ~genv s = 
  let elemt, sty, name, fl = storage_for_global_string_internal ~genv s in
  elemt, sty, CTTMlvVar(name, sty), fl

let storage_for_global_string ~genv s = 
  let elemt, sty, name, fl =
    storage_for_global_string_internal ~genv s in
  elemt, sty, ILlvVar(name, sty), fl

let get_field_offset ~genv =
  Separate_fatpointer.get_field_offset ~genv

let cttm_storage_for_global_variable ~genv ~known vid vty fl = 
  let elemt, sty, name, fl = 
    storage_for_variable_internal ~genv ~global:true 
      (translate_global_variable_name ~known)
      vid vty fl in
  elemt, sty, CTTMlvVar(name, sty), fl

let storage_for_variable ~env ~loc vt vid ct fl = 
  let storage_for_global_variable ~env ~loc vid vty fl = 
    let elemt, sty, name, fl =
      storage_for_variable_internal ~genv:env.global_env ~global:true
	(translate_global_variable_name ~known:(Hashtbl.find env.decl_env vid))
	vid vty fl in
    [], elemt, sty, ILlvVar(name, sty), fl
  in
  let storage_for_local_heap_variable ~env ~loc vid vty fl = 
    let elemt, sty, name, fl =
      storage_for_variable_internal 
	~genv:env.global_env ~global:false translate_ptr_local_heap_variable_name vid vty fl in
    let tid = new_tempid ~env in
    dprintf 7 "%d: elemt = %a, sty = %a vty = %a" tid
      Il_formatter.pp_ctt_type elemt Il_formatter.pp_ctt_type sty Il_formatter.pp_ctt_type vty;
    dprintf 7 "%d: volatile = %b %b %b" tid elemt.ct_volatile_p sty.ct_volatile_p vty.ct_volatile_p;
    [ make_il3 ~loc
	(IL3stmtReadRaw(tid, make_c_type (Tpointer sty), ILlvVar(name, make_c_type (Tpointer sty)),[])) ],
    elemt, sty, ILlvPtr(tid), fl
  in
  let storage_for_local_stack_variable ~env ~loc vid vty fl = 
    let elemt, sty, name, fl =
      storage_for_variable_internal ~genv:env.global_env ~global:false translate_local_stack_variable_name vid vty fl in
    [], elemt, sty, ILlvVar(name, sty), fl
  in
  match vt with
    GlobalVar -> storage_for_global_variable ~env ~loc vid ct fl
  | HeapVar -> storage_for_local_heap_variable ~env ~loc vid ct fl
  | StackVar -> storage_for_local_stack_variable ~env ~loc vid ct fl
  | RegVar -> assert false


let make_cttm_expr ~loc body t =
  Locterm.locput ~loc { mexpr_type = t; mexpr_t = body }
let emit_cttminit ~loc e = 
  Locterm.locput ~loc (CTTMinitExp e)
let emit_cttminitlist ~loc e = 
  Locterm.locput ~loc (CTTMinitList e)

let cttm_macro_for_blockheader ~genv ~loc elemtype elemnum = 
  let elemsize = sizeof_type_genv ~genv elemtype in
  let storage_type = get_storage_type_array elemtype elemnum in
  let allocsize = Big_int.mult_big_int elemsize (Option.default unit_big_int elemnum) in
  let allocsize_e = 
    make_cttm_expr ~loc (CTTMexpConstant (CTTconstInteger allocsize)) type_size_t in
  let typeinfo = 
    (*make_cttm_expr (CTTMexpConstant (CTTconstTypeInfo elemtype)) type_typeinfo_ptr *)
    make_cttm_expr ~loc
      (CTTMexpRead
	 (CTTMlvVar(encoded_typeinfo_name ~genv elemtype, type_typeinfo_init),
	  ["val", type_typeinfo_struct])) type_typeinfo_struct
  in
  let header =
    make_cttm_expr ~loc
      (CTTMexpInvoke(lv_cttm_macro_emit_header,
		     [typeinfo; allocsize_e]))
      type_fsc_header
  in
  header

let exp_of_error_constant error = 
  let name = match error with
    ILSerrPtrMinus -> "ERR_PTRMINUS"
  | ILSerrPtrComp -> "ERR_PTRCOMP"
  | ILSerrAborted Il.ILabortNotReached -> "ERR_NOTREACHED"
  | ILSerrAborted Il.ILabortOthers s -> "ERR_UNKNOWN"
  in
  IL3RexConstant(CTTconstAbstract name)

let translate_rettype_hint ~env ~loc hint = 
  let tptr_id = new_tempid ~env in
  let exp, cast = 
    match hint with
      Bunanalyzed | Bunknown | Bscalar ->
	IL3RexConstant constant_zero, false
    | Bused t | Bcast t ->
	let t_elem, t_st, lv, fl = storage_for_typeinfo_constant ~genv:env.global_env t in
	IL3RexAddress(lv,fl), true
    | Brettype ->
	if env.is_polymorphic then
	  IL3RexArgument 0, false
	else
	  let t_elem, t_st, lv, fl = storage_for_typeinfo_constant ~genv:env.global_env env.rettype in
	  IL3RexAddress(lv,fl), true
  in
  if cast then begin
    let tptr_id2 = new_tempid ~env in
    tptr_id2, [ make_il3 ~loc (IL3stmtRexp(tptr_id, type_typeinfo_ptr, exp));
		make_il3 ~loc (IL3stmtRexp(tptr_id2, type_base_t, IL3RexCoerce(type_base_t, tptr_id)))
	      ]
  end 
  else
    tptr_id, [ make_il3 ~loc (IL3stmtRexp(tptr_id, type_base_t, exp)) ]

let resolve_reference_type ty = 
  match ty with
    ILRtypeBase({ ct_ty = (Tpointer t | Tarray(t,_)) } as tp, binfo) -> 
      make_c_type (Tpointer t), t
  | t -> 
      failwith_p "failure1468: %a@." Ils_formatter.pp_ilr_type t

let translate_expr1 is_wide ~env ~loc td e =
  match is_wide, e with
  | false, ILCexpCoerce1(td,vs) -> begin
      let ts = type_of_tempvar ~env vs in
      match ts, td.ct_ty with
	ILRtypeVal ({ ct_ty = Tbuiltin bts }, _), Tbuiltin btd ->
	  [], REXP (IL3RexCoerce(td, vs)), []
      | _ -> failwith "unimp1"
  end
  | false, ILCexpConstant((CTTconstNull | CTTconstInteger _ | CTTconstFloat _) as c) ->
      [], REXP (IL3RexConstant(c)), []
  | false, ILCexpConstant(CTTconstString s) ->
      let t_elem, t_st, lv, fl = storage_for_global_string ~genv:env.global_env s in
      let temp0 = new_tempid ~env in
      [ make_il3 ~loc (IL3stmtRexp (temp0, type_void_ptr, IL3RexAddress(lv,fl))) ],
      REXP (IL3RexCoerce(type_base_t, temp0)), []
  | false, ILCexpConstant(CTTconstTypeInfo t) ->
      let t_elem, t_st, lv, fl = storage_for_typeinfo_constant ~genv:env.global_env t in
      let temp0 = new_tempid ~env in
      [ make_il3 ~loc (IL3stmtRexp (temp0, type_void_ptr, IL3RexAddress(lv,fl))) ],
      REXP (IL3RexCoerce(type_base_t, temp0)), []
  | false, ILCexpConstant(_) -> failwith "unimp963"
  | false, ILCexpUndefined -> [], REXP (IL3RexUndefined), []
  | false, ILCexpBinop(bop,t1,t2) -> begin
      match bop, (type_of_tempvar ~env t1), (type_of_tempvar ~env t2) with
	_, ILRtypeBase (_, { ilr_base_cast = bc1 }), ILRtypeBase (_, { ilr_base_cast = bc2 }) ->
	  dprintf 6 "comparing bases (%a, %a)..." 
	    Ils_formatter.pp_trivalue bc1 Ils_formatter.pp_trivalue bc2;
	  let i1, temp1 = 
	    match bc1 with
	      Never -> [], t1
	    | Maybe | Always ->
		let t = new_tempid ~env in
		[ make_il3 ~loc
		    (IL3stmtCallReducibleFun(t, IL3pr_misc Base_remove_castflag, [t1])) ], t;
	  in
	  let i2, temp2 = 
	    match bc2 with
	      Never -> [], t2
	    | Maybe | Always ->
		let t = new_tempid ~env in
		[ make_il3 ~loc
		    (IL3stmtCallReducibleFun(t, IL3pr_misc Base_remove_castflag, [t2])) ], t;
	  in
	  (i1 @ i2), REXP (IL3RexBinop(bop,temp1,temp2)), []
      | (ILbinLessThan | ILbinLessEqual | ILbinGtrThan | ILbinGtrEqual), ILRtypeOfs _, ILRtypeOfs _
	when !use_signed_compare_for_offset
	-> begin
	  let t01 = new_tempid ~env in
	  let t02 = new_tempid ~env in
	  [ make_il3 ~loc (IL3stmtRexp(t01, type_signed_ofs_t,
				  IL3RexCoerce(type_signed_ofs_t, t1)));
	    make_il3 ~loc (IL3stmtRexp(t02, type_signed_ofs_t,
				  IL3RexCoerce(type_signed_ofs_t, t2))) ],
	  REXP (IL3RexBinop(bop, t01, t02)), []
	end
      | _, ILRtypeOfs _, (ILRtypeOfs _ | ILRtypeVal _)
      | _, ILRtypeVal _, ILRtypeVal _ ->
	  [], REXP (IL3RexBinop(bop,t1,t2)), []
      | _ -> assert false
  end
  | false, ILCexpUnaryop(uop,t1) -> [], REXP (IL3RexUnaryop(uop,t1)), []
  | false, ILCexpIdent(id) -> [], MOVE id, []
	
  | _, ILCexpInvoke(ILSlvVar (vt,f,t),args,cattr,rettype_hint) -> begin
      if vt <> GlobalVar then
	failwith "unimp1306: calling local function";
      if cattr <> Insert_check.attr_nocheck then
	failwith "unimp2";
      let tptr_id, tptr_insn = translate_rettype_hint ~env ~loc rettype_hint in
      let pre_insns, new_args, post_insns =
	translate_funcargs ~env ~loc ~tptr_id t args in
      let new_name = 
	translate_global_function_name ~genv:env.global_env f t in
      let new_type =
	translate_c_type ~env t in
      pre_insns @ tptr_insn, 
      IEXP (IL3IexInvoke(ILlvVar (new_name, new_type), new_args)),
      post_insns
  end
  | is_wide, ILCexpInvoke(ILSlvPtrToFunc (b,o),args,cattr,rettype_hint) -> begin
      let td = original_type_from_val_type td in
      let is_void_ret = td.ct_ty = Tvoid in
      let p = parse_type ~env (if is_void_ret then type_char else td) in (* TODO: dirty workaround *)
      let func_type =
	match (type_of_tempvar ~env b) with
	  ILRtypeBase({ct_ty = Tpointer(t)}, _) -> t
	| _ -> assert false
      in
      let tptr_id, tptr_insn = translate_rettype_hint ~env ~loc rettype_hint in
      let concrete_pre_insns, concrete_exp, concrete_post_insns = 
	let pre_insns, new_args, post_insns =
	  translate_funcargs ~env ~loc ~tptr_id func_type args in
	let new_type =
	  translate_c_type ~env func_type
	in
	let fptrid0 = new_tempid ~env in
	let fptrid = new_tempid ~env in
	let new_type_ptr = 
	  make_c_type (Tpointer new_type) in
	let fptr_insns = 
	  [ make_il3 ~loc
	      (IL3stmtCallReducibleFun (fptrid0, IL3pr_misc Get_realoffset_funcptr, [b]));
	    make_il3 ~loc
	      (IL3stmtRexp(fptrid, new_type_ptr, IL3RexCoerce(new_type_ptr, fptrid0))) ]
	in
	fptr_insns @ pre_insns, 
	IEXP (IL3IexInvoke(ILlvPtr (fptrid), new_args)),
	post_insns
      in
      let dynamic_insns, dynamic_exp, dynamic_posinsns = 
	let args_insns, new_args, post_insns = 
	  translate_func_varargs ~env ~loc args in
	let dval_resid = new_tempid ~env in
	let tptr2_id, tptr2_vid, tptr2_insn = 
	  if p.is_indirect_ret then
	    let tid = new_tempid ~env in
	    let tid_v = new_tempid ~env in
	    let tid_v1 = new_tempid ~env in
	    let tid_t = new_tempid ~env in
	    let ptrtyp = make_c_type (Tpointer p.packed_t) in
	    tid, tid_v,
	    [
	     make_il3 ~loc (IL3stmtRexp (tid_t, type_typeinfo_ptr, IL3RexConstant(CTTconstTypeInfo td)));
	     make_il3 ~loc (IL3stmtCallImmobileFun (tid_v1, Alloc_valtempoline, [tid_t]));
	     make_il3 ~loc (IL3stmtRexp (tid_v, ptrtyp, IL3RexCoerce(ptrtyp, tid_v1)));
	     make_il3 ~loc (IL3stmtRexp (tid, type_base_t, IL3RexCoerce(type_base_t, tid_v)))
	   ]
	  else
	    tptr_id, -1, []
	in
	let invoke_insn = make_il3 ~loc
	    (IL3stmtCallImmobileFun(dval_resid, Invoke_generic_func,  [tptr2_id; b; o] @ new_args)) in
	if is_void_ret then begin
	  [ enclose_il3_sequence ~loc (args_insns @ tptr2_insn @ [invoke_insn]) ], 
	  REXP (IL3RexCoerce(type_void, dval_resid)), post_insns
	end else if p.is_indirect_ret then begin
	  let tid = new_tempid ~env in
	  let tid_zero = new_tempid ~env in
	  let indirect_insns = 
	    [ 
	      make_il3 ~loc (IL3stmtRexp(tid_zero, type_void, IL3RexCoerce(type_void, dval_resid)));
	      make_il3 ~loc (IL3stmtReadRaw(tid, p.packed_t, ILlvPtr(tptr2_vid), [])) ] in
	  [ enclose_il3_sequence ~loc (args_insns @ tptr2_insn @ [invoke_insn] @ indirect_insns) ], 
	  MOVE tid, post_insns
	end else begin
	  let is_dvalue = p.generic_t == type_dvalue in
	  let generic_insns, generic_id =
	    match is_dvalue, is_wide with
	      false, false ->
		let val_id = new_tempid ~env in
		let word_id = new_tempid ~env in
		[ make_il3 ~loc
		    (IL3stmtCallReducibleFun(val_id, IL3pr_convert Value_of_dvalue, [dval_resid])); 
		  make_il3 ~loc
		    (IL3stmtCallReducibleFun(word_id, IL3pr_cdr Value, [val_id])) ],
		word_id
	    | false, true ->
		let val_id = new_tempid ~env in
		[ make_il3 ~loc
		    (IL3stmtCallReducibleFun(val_id, IL3pr_convert Value_of_dvalue, [dval_resid])) ],
		val_id
	    | true, false ->
		let dword_id = new_tempid ~env in
		[ make_il3 ~loc
		    (IL3stmtCallReducibleFun(dword_id, IL3pr_cdr Dvalue, [dval_resid])) ],
		dword_id
	    | true, true ->
		[], dval_resid
	  in
	  let result_exp = match p.generic_to_packed with
	    Some pr -> RF (pr, [generic_id])
	  | None -> REXP (IL3RexCoerce(p.packed_t, generic_id))
	  in
	  [enclose_il3_sequence ~loc (args_insns @ tptr2_insn @ (invoke_insn :: generic_insns))],
	  result_exp, post_insns
	end
      in
      let result_t = if is_void_ret then type_void else (parse_type ~env td).packed_t in
      let merge_id = new_tempid ~env in
      let temp0 = new_tempid ~env in
      (* this invocation shall be strictly sequential *)
      [ enclose_il3_sequence ~loc
	  (tptr_insn @
	   [ make_il3 ~loc (IL3stmtCallReducibleFun (temp0, IL3pr_misc Is_cast, [b]));
	     make_il3 ~loc
	       (IL3stmtConditional
		  ([merge_id], temp0, 
		   enclose_il3_sequence ~loc
		     (dynamic_insns @
		      [ emit_e_or_r ~loc merge_id result_t dynamic_exp
		      ] @ dynamic_posinsns),
		   enclose_il3_sequence ~loc
		     (concrete_pre_insns @
		      [ emit_e_or_r ~loc merge_id result_t concrete_exp ]
		      @ concrete_post_insns))) ])
	   ],
      MOVE merge_id, []
  end
  | false, ILCexpAddress(ILSlvVar (vt, n, ct), []) -> begin
      let i0, t_elem, t_st, lv, fl = storage_for_variable ~env ~loc vt n ct [] in
      let temp0 = new_tempid ~env in
      (i0 @ [ make_il3 ~loc (IL3stmtRexp(temp0, type_void_ptr, IL3RexAddress(lv,fl))) ]),
      REXP (IL3RexCoerce(type_base_t, temp0)), []
  end
  | false, ILCexpArgument(i) -> begin
      try [], REXP (IL3RexArgument(Earray.get env.arguments_n i)), []
      with Not_found -> assert false
  end
  | false, ILCexpArgumentB(i) -> begin
      try [], REXP (IL3RexArgument(Earray.get env.arguments_b i)), []
      with Not_found -> assert false
  end
  | false, ILCexpArgumentV(i) -> begin
      try [], REXP(IL3RexArgument(Earray.get env.arguments_v i)), []
      with Not_found -> assert false
  end
  | false, ILCexpCoerce2(td, vb, vo) -> begin
      let ts = type_of_tempvar ~env vb in
      match ts, td.ct_ty with
	ILRtypeBase ({ ct_ty = Tpointer tp }, _), Tbuiltin bt ->
	  (* e.g. char * -> short (narrowing) *)
	  let temp0 = new_tempid ~env in
	  [ make_il3 ~loc
	      (IL3stmtCallReducibleFun
		 (temp0, IL3pr_convert Vaddr_of_base_ofs, [vb; vo])) ],
	  REXP (IL3RexCoerce(td, temp0)), []
      | _ -> failwith_p "unimp857 : %a" Ilc_formatter.pp_ilc_expr e
  end
  | false, e -> failwith_p "unimp4N : %a" Ilc_formatter.pp_ilc_expr e
  | true, e -> assert false
;;

let translate_expr2 ~env ~loc (ty1,ty2) e =
  match e with
  | ILCexpBinop21((ILbinPlusPV | ILbinMinusPV) as bop,(t1b,t1v),t2) -> begin
      let is_minus = (bop = ILbinMinusPV) in
      let t_t1b = match type_of_tempvar ~env t1b with
	ILRtypeBase(t, _) -> t
      | _ -> assert false
      in
      let p = parse_type ~env t_t1b in
      let tid2 = new_tempid ~env in
      let addofs_v, addofs_insns = 
	if is_minus then
	  let tid0 = new_tempid ~env in
	  tid0, [ make_il3 ~loc (IL3stmtRexp(tid0, type_ofs_t, IL3RexUnaryop(UnaryMinus, t2))) ]
	else
	  t2, []
      in
      let (tt,rwt,cdrtype) = get_destructors ~env t_t1b in
      let tid = new_tempid ~env in
      ( addofs_insns @ 
	[ make_il3 ~loc (IL3stmtCallReducibleFun(tid, IL3pr_misc (Add_fat_pointer t_t1b), [t1b; t1v; addofs_v])) ],
	RF (IL3pr_car rwt, [tid]),
	RF (IL3pr_cdr rwt, [tid]),
	[] )
  end
  | ILCexpCoerce2(td, vb, vo) -> begin
      (* e.g. char * -> int *)
      let ts = type_of_tempvar ~env vb in
      match ts, td.ct_ty with
	ILRtypeBase ({ ct_ty = Tpointer tp }, _), Tbuiltin bt ->
	  let temp0 = new_tempid ~env in
	  [ 
	    make_il3 ~loc
	      (IL3stmtCallReducibleFun
		 (temp0, IL3pr_convert Vaddr_of_base_ofs, [vb; vo])) ],
	  RF (IL3pr_misc Base_remove_castflag, [vb]),
	  REXP (IL3RexCoerce(td, temp0)), []
      | ILRtypeBase ({ ct_ty = (Tpointer tsp | Tarray(tsp,_)) }, _), (Tpointer tdp | Tarray(tdp, _)) ->
	  if C_typing.equal_type tsp tdp then
	    [], MOVE vb, MOVE vo, []
	  else begin
	    [], RF (IL3pr_misc (Set_base_castflag td), [vb; vo]), MOVE vo, []
	  end
      | ILRtypeBase ({ ct_ty = Tbuiltin bt }, _), (Tpointer tdp | Tarray(tdp,_)) -> begin
	  let temp0 = new_tempid ~env in
	  [ 
	    make_il3 ~loc
	      (IL3stmtCallReducibleFun
		 (temp0, IL3pr_convert Ofs_of_base_vaddr, [vb; vo]))
	  ],
	  RF (IL3pr_misc (Set_base_castflag td), [vb; temp0]), MOVE temp0, []
      end
      | _ -> failwith_p
	    "unimp1012: ptrcast wide: %a -> %a"
	    Ils_formatter.pp_ilr_type ts Il_formatter.pp_ctt_type td
  end
  | ILCexpAddress(ILSlvVar (vt, n, ct), (_::_ as fl)) -> begin
      let sz, top_t =
	size_and_elemtype_of_onmemory_type ~incomplete_ok:true ct
      in
      let ofs = match get_field_offset ~genv:env.global_env ct fl with
	StrOfsNormal o -> o
      | StrOfsBitfield _ -> failwith "T1094: taking offset of bitfields"
      in
      let i0, t_elem, t_st, lv, fl = storage_for_variable ~env ~loc vt n ct [] in
      let temp0 = new_tempid ~env in
      let temp1 = new_tempid ~env in

      let targetpointertype, pointedtargettype = resolve_reference_type ty1 in
      let is_cont_opt_ok = 
	!use_optimized_narrow_cast &&
	is_type_continous ~env top_t &&
	is_type_continous ~env pointedtargettype &&
	top_t.ct_size <> None &&
	pointedtargettype.ct_size <> None &&
	mod_big_int (Option.get top_t.ct_size) (Option.get pointedtargettype.ct_size) ==! zero_big_int
      in
      i0 @ [
	   make_il3 ~loc (IL3stmtRexp(temp0, type_void_ptr, IL3RexAddress(lv,fl)));
	   make_il3 ~loc (IL3stmtRexp(temp1, type_base_t, IL3RexCoerce(type_base_t, temp0)))
	 ],
      (if is_cont_opt_ok then
	MOVE temp1
      else
	RF (IL3pr_misc Base_put_castflag, [temp1])),
      REXP (IL3RexConstant(CTTconstInteger ofs)), []
  end
  | ILCexpAddress(ILSlvPtr(vb,vo), fields) -> begin
      let pointedsourcetype = match type_of_tempvar ~env vb with
	ILRtypeBase({ ct_ty = Tpointer t}, _) -> t
      | _ -> assert false
      in
      let targetpointertype, pointedtargettype = resolve_reference_type ty1 in
      let offset = match get_field_offset ~genv:env.global_env pointedsourcetype fields with
	StrOfsNormal o -> o
      | StrOfsBitfield _ -> failwith "SF299: taking address of bitfield"
      in
      let tempc = new_tempid ~env in
      let tempo = new_tempid ~env in
      [
       make_il3 ~loc (IL3stmtRexp (tempc, type_ofs_t, IL3RexConstant(CTTconstInteger offset)));
       make_il3 ~loc (IL3stmtRexp (tempo, type_ofs_t, IL3RexBinop(ILbinPlusVV,vo,tempc)))
     ],
      RF (IL3pr_misc (Set_base_castflag targetpointertype), [vb;tempo]),
      MOVE tempo,
      []
  end
  | ILCexpInvoke _ -> begin
      (* bollow translate_expr1 *)
      let add_insn, new_exp, post_insns = translate_expr1 true ~env ~loc ty2 e in
      let original_type = original_type_from_val_type ty2 in
      let p = parse_type ~env original_type in
      let tid = new_tempid ~env in
      let tid2 = new_tempid ~env in
      let (tt,rwt,cdrtype) = get_destructors ~env original_type in
      if is_numeric original_type then
	(add_insn @
	 [emit_e_or_r ~loc tid tt new_exp;
	  make_il3 ~loc (IL3stmtCallReducibleFun(tid2, IL3pr_cdr rwt, [tid]))
	]),
	RF(IL3pr_car rwt,[tid]),
	make_coerce_from_vaddr p.cdr_t tid2,
	post_insns
      else
	(add_insn @ [emit_e_or_r ~loc tid tt new_exp]),
	RF(IL3pr_car rwt,[tid]),
	RF(IL3pr_cdr rwt,[tid]),
	post_insns
  end
  | _ -> failwith_p "unimp4W : %a" Ilc_formatter.pp_ilc_expr e
      
let make_ptr_operations ~env ~loc ~idb ~ido ~genf ~specf ~tyd ~fl args targets =
  let needgen, needspec, needcheck, tys, typ = match type_of_tempvar ~env idb with
    ILRtypeBase ({ ct_ty = Tpointer tp } as typ, _) ->
      true, true, true, tp, typ
  | ILRtypeBase ({ ct_ty = Tarray (tp, _) } as typ, _) ->
      true, true, true, tp, typ
  | ILRtypeBaseTemp ({ ct_ty = Tstruct _ } as typ, _) ->
      false, true, false, typ, typ
  | _ -> assert false
  in
  let temp0 = new_tempid ~env in
  let temp1 = new_tempid ~env in
  let temp2 = new_tempid ~env in
  let temp3 = new_tempid ~env in
  (*let i_checkcast = [
    make_il3_raw (IL0stmtDefTemp (temp0, type_boolean, 
			      ILexpInvoke(lv_check_cast, [idb])));
    make_il3_raw (IL0stmtIf (IFTRUE, temp0, label0))
  ] in*)
  let i_spec1 = 
    (if needcheck then begin
      if !use_boundary_t then begin
	let temp4, insn = 
	  try 
	    let bc = VarMap.find idb env.boundary_cache in
	    dprintf 6 "boundary_cache: found %d for %d" bc idb;
	    bc, []
	  with Not_found -> begin
	    let temp4 = new_tempid ~env in
	    dprintf 6 "boundary_cache: not found for %d: allocating %d" idb temp4;
	    env.boundary_cache <- VarMap.add idb temp4 env.boundary_cache;
	    temp4, 
	    [make_il3 ~loc (IL3stmtCallImmobileFun(temp4, Get_boundary, [idb]))]
	  end
	in
	(if insn = [] then 
	  Debug.dprintf ~category:101 1 "Boundary info %d reused for %d" temp4 idb else ());
	insn @ 
	[ make_il3 ~loc (IL3stmtCallImmobileFun
		      (temp2, Is_boundary_offset_ok, [temp4; ido])) ]
      end else
	[make_il3 ~loc (IL3stmtCallImmobileFun
		     (temp2, Is_offset_ok, [idb; ido])) ]
    end else [])
  in
  let i_spec2 = 
    let fl = translate_fields ~genv:env.global_env fl in
    let _tsp, rf = rf_get_realoffset ~env tys in
    [ make_il3 ~loc (IL3stmtCallReducibleFun (temp3, rf, [idb; ido])) ]
    @ specf ~env ~loc args tyd temp3 fl
  in
  let i_gen =
    if fl = [] then
      genf ~env ~loc args tyd (Some tyd) idb ido
    else begin
      let tempc = new_tempid ~env in
      let tempo = new_tempid ~env in
      let ofs = 
	match get_field_offset ~genv:env.global_env tys fl with
	  StrOfsNormal ofs -> ofs
	| StrOfsBitfield _ -> failwith "unimp1033: bitfield access"
      in
      make_il3 ~loc (IL3stmtRexp (tempc, type_ofs_t, 
			     IL3RexConstant(CTTconstInteger ofs))) ::
      make_il3 ~loc (IL3stmtRexp (tempo, type_ofs_t, 
			     IL3RexBinop(ILbinPlusVV, ido, tempc))) ::
      genf ~env ~loc args tyd (Some tys) idb tempo
    end
  in
  let i_clearflag = 
    if !clear_castflag_at_memop && needgen then
      [make_il3 ~loc
	 (IL3stmtCallReducibleFunOverwriting 
	    (idb, IL3pr_misc (Set_base_castflag typ), [idb; ido]))]
    else []
  in
  let l = 
    if needgen || needcheck then
      if needspec then 
	if Fsc_config.size_check_subsumes_cast_check then
	  i_spec1 @ 
	  [ make_il3 ~loc
	      (IL3stmtConditional(targets, temp2, 
				  enclose_il3_sequence ~loc i_spec2, 
				  enclose_il3_sequence ~loc (i_clearflag @ i_gen))) ]
	else 
	  (if needgen then assert false (*@ i_checkcast*) else []) @ i_spec1 @
	  [ make_il3 ~loc
	      (IL3stmtConditional(targets, temp2, 
				  enclose_il3_sequence ~loc i_spec2,
				  enclose_il3_sequence ~loc (i_clearflag @ i_gen))) ]
      else
	i_gen
    else
      i_spec2
  in
  enclose_il3_sequence ~loc l
;;

let rec translate_global_initializer_base ~genv ~denv ~loc e = 
  let d, t = match e with
    ILSinitbaseNone -> 
      (CTTMexpConstant constant_zero), None
  | ILSinitbaseVar(vt,vid,ct) -> begin
      assert (vt = GlobalVar);
      let _, t_str, lv, fl = 
	cttm_storage_for_global_variable ~genv ~known:(Hashtbl.find denv vid) vid ct [] in
      let e = 
	make_cttm_expr ~loc (CTTMexpAddress (lv, fl))
	  (make_c_type (Tpointer t_str))
      in
      (CTTMexpCoerce(type_base_t, e)), Some ct
  end
  | ILSinitbaseString(s) ->
      let _, t_str, lv, fl = cttm_storage_for_global_string ~genv s in
      let e = 
	make_cttm_expr ~loc (CTTMexpAddress (lv, fl))
	  (make_c_type (Tpointer t_str))
      in
      (CTTMexpCoerce(type_base_t, e)), Some (make_c_type (Tpointer type_char))
  | ILSinitbaseTypeInfo t -> 
      let e = 
	make_cttm_expr ~loc (CTTMexpConstant (CTTconstTypeInfo t)) (type_void_ptr)
      in
      (CTTMexpCoerce(type_base_t, e)), None
  in
  make_cttm_expr ~loc d type_base_t, t

let constant_of_ilsinitofs = function
    ILSinitofsInt i -> CTTconstInteger i
  | ILSinitofsFloat f -> CTTconstFloat f

let is_pointer_cast ~region_target_type ~value_target_type ~unknown_p ~offset = 
  let r = 
  if C_typing.is_type_loose_function value_target_type && unknown_p
  then
    (dprintf 5 "emitting value of type %a as constant"
       Il_formatter.pp_ctt_type value_target_type;
     true)
  else if C_typing.equal_type region_target_type value_target_type then
    match region_target_type.ct_size with
      None -> not (eq_big_int offset zero_big_int)
    | Some s -> not (eq_big_int (mod_big_int offset s) zero_big_int)
  else
    true
  in
  dprintf 6 "is_ptr_cast: r=%a v=%a o=%s -> %b"
    Il_formatter.pp_ctt_type region_target_type
    Il_formatter.pp_ctt_type value_target_type
    (string_of_big_int offset) r;
  r

let select_macro_emit =
  let emittype_ptr_G = make_c_type (Tabstract ("union fsc_initUptr")) in
  let emitfunc_ptr_G = make_cttm_func_lv "EMIT_INITPTR"
      [type_base_t; type_ofs_t; type_boolean] emittype_ptr_G
  in
  let emitfunc_ptr_L = make_cttm_func_lv "init_ptrvalue"
      [type_base_t; type_ofs_t; type_boolean] emittype_ptr_G
  in
  fun ~genv ~is_global t ->
    let p = parse_type_genv ~genv t in
    match t.ct_ty with
      Tbuiltin bt ->
	if p.is_fat then begin
	  let tname = encoded_name_of_btype bt in
	  if is_global then begin
	    let emitname = "EMIT_INIT_" ^ tname in
	    let emittypename = "fsc_initU_" ^ tname in
	    let emittype = make_c_type (Tabstract ("union " ^ emittypename)) in
	    let emitfunc = make_cttm_func_lv emitname [type_base_t; t] emittype
	    in
	    (None, emittype, Some emitfunc, t)
	  end else begin
	    let emitname = "initialize_" ^ tname in
	    let emitfunc = make_cttm_func_lv emitname [type_base_t; t] p.packed_t
	    in
	    (None, p.packed_t, Some emitfunc, t)
	  end
	end else
	  (None, t, None, t)
    | Tpointer t ->
	if is_global then
	  (Some t,emittype_ptr_G, Some emitfunc_ptr_G, type_ofs_t)
	else
	  (Some t,type_ptrvalue, Some emitfunc_ptr_L, type_ofs_t)
    | _ -> assert false

let translate_global_initializer_value ~genv ~denv ~loc ~is_global t b o = 
  let region_targettype, emittype, emitfunc, ofs_type = 
    select_macro_emit ~genv ~is_global t in
  let exp_o = make_cttm_expr ~loc (CTTMexpConstant(constant_of_ilsinitofs o)) ofs_type in
  match emitfunc with
    Some emitfunc ->
      let exp_base, value_targettype = translate_global_initializer_base ~genv ~denv ~loc b in
      let value_targettype = Option.map
	  (fun t -> snd (size_and_elemtype_of_onmemory_type ~incomplete_ok:true t)) value_targettype in
      let cast_flag = match region_targettype,value_targettype,o with
	None, _, _ -> None
      | Some expected, Some real, ILSinitofsInt o ->
	  let unknown_p = match b with
	    ILSinitbaseVar(_vt, vid, _vty) ->
	      (Hashtbl.find denv vid <> Known)
	  | ILSinitbaseTypeInfo _ -> true
	  | ILSinitbaseString _ -> false
	  | ILSinitbaseNone -> assert false
	  in
	  Some
	    (is_pointer_cast
	       ~region_target_type:expected
	       ~value_target_type:real
	       ~unknown_p
	       ~offset:o)
      | Some _, _, _ -> Some true
      in
      let cast_flag = match cast_flag with
	None -> []
      | Some false ->
	  [make_cttm_expr ~loc (CTTMexpConstant constant_zero) type_boolean]
      | Some true ->
	  [make_cttm_expr ~loc (CTTMexpConstant constant_one) type_boolean]
      in
      make_cttm_expr ~loc
	(CTTMexpInvoke(emitfunc, (exp_base :: exp_o :: cast_flag))) emittype
  | None -> 
      assert (b = ILSinitbaseNone);
      exp_o

let translate_local_initializer ~env ~safe i =
  if (not safe) then
    failwith "unimp1155:translate_local_initializer write to unsafe region (use indirect)";
  let rec loop fields_rev i = 
    let loc = locget i in
    match locval i with
      ILSinitPostFill ->
	[]
    | ILSinitArray(t, b) ->
	let elemt = match t.ct_ty with
	  Tarray(t,_) -> t | _ -> assert false
	in
	let p = parse_type ~env elemt in
	Util.map_flatten_i
	  (fun i x ->
	    loop ((Iindexed (big_int_of_int i), p.initializer_type) :: fields_rev) x) b
    | ILSinitConstant(t, b, o) ->
	let p = parse_type ~env t in
	let e = 
	  translate_global_initializer_value ~genv:env.global_env ~is_global: false t b o
	in
	let add_fields = if p.is_fat then [Inamed "cv", p.packed_t] else [] in 
	[List.rev (add_fields @ fields_rev), e]
    | ILSinitStruct(t, b) ->
	let p = parse_type ~env t in
	Util.map_flatten
	  (fun (t, x) ->
	    loop ((Inamed (translate_field_name t), p.initializer_type) :: fields_rev) x) b
  in
  loop [] i

let is_empty_initialization i = 
  let rec loop i = match locval i with
      ILSinitPostFill -> ()
    | ILSinitConstant _ -> raise Exit
    | ILSinitArray(_, l) ->
	List.iter loop l
    | ILSinitStruct(_, l) ->
	List.iter (fun (_,i) -> loop i) l
  in
  try
    loop i;
    true
  with
    Exit -> false

let translate_local_const_initialization_value ~env ~loc t addr fields b o = 
  let p = parse_type ~env t in
  let t_b = new_tempid ~env in
  let t_o = new_tempid ~env in
  let ins_b = match b with
    ILSinitbaseNone -> 
      [ make_il3 ~loc (IL3stmtRexp(t_b, type_base_t, IL3RexConstant(CTTconstNull))) ]
  | ILSinitbaseVar(vt, vid, vty) ->
      assert (vt = GlobalVar);
      let i0, elemt, sty, lv, fl =
	storage_for_variable ~env ~loc vt vid vty []
      in
      assert (i0 = []);
      let sty_ptr = make_c_type (Tpointer sty) in
      let sty_id = new_tempid ~env in
      failwith "unimp1673"; (* TODO unimp castflag *)
      (*[ make_il3_raw (IL0stmtDefTemp(sty_id, sty_ptr, ILexpAddress(lv, fl)));
	make_il3_raw (IL0stmtDefTemp(t_b, type_base_t, ILexpCoerce(type_base_t, sty_id))) ]*)
  | ILSinitbaseString s ->
      let t_elem, t_st, lv, fl = storage_for_global_string ~genv:env.global_env s in
      let temp0 = new_tempid ~env in
      [ make_il3 ~loc (IL3stmtRexp(temp0, type_void_ptr, IL3RexAddress(lv,fl)));
	make_il3 ~loc (IL3stmtRexp(t_b, type_base_t, IL3RexCoerce(type_base_t, temp0))) ]
  | _ -> failwith_p "unimp1682initbase : %a" Ils_formatter.pp_ils_initializer_value (b, o)
  in
  let ins_o = match o with
    ILSinitofsInt i ->
      [ make_il3 ~loc (IL3stmtRexp(t_o, p.cdr_t, IL3RexConstant(CTTconstInteger i))) ]
  | ILSinitofsFloat f ->
      [ make_il3 ~loc (IL3stmtRexp(t_o, p.cdr_t, IL3RexConstant(CTTconstFloat f))) ]
  in
  if p.is_pointer then begin
    let pv = new_tempid ~env in
    let fields = 
      if fields = [] then []
      else fields @ [ "cv", type_ptrvalue ]
    in
    ins_b @ ins_o @ 
    [ make_il3 ~loc (IL3stmtCallReducibleFun(pv, IL3pr_cons PtrValue, [t_b; t_o]));
      make_il3 ~loc (IL3stmtWriteRaw(ILlvPtr addr, fields, pv)) ]
  end
  else begin
    let ins_v, t_v = match b with
      ILSinitbaseNone -> [], t_o
    | _ ->
	let t_v = new_tempid ~env in
	[ make_il3 ~loc (IL3stmtCallReducibleFun(t_v, IL3pr_convert Vaddr_of_base_ofs, [t_b; t_o])) ],
	t_v
    in
    if p.is_fat then begin
      let cons_ptype, initt = match p.size with
	Size_dword -> Dvalue, type_dvalue
      | Size_word -> Value, type_value
      | _ -> assert false
      in
      let v = new_tempid ~env in
      let fields = 
	if fields = [] then []
	else fields @ [ "cv", initt ]
      in
      ins_b @ ins_o @ ins_v @ 
      [ make_il3 ~loc (IL3stmtCallReducibleFun(v, IL3pr_cons cons_ptype, [t_b; t_v]));
	make_il3 ~loc (IL3stmtWriteRaw(ILlvPtr addr, fields, v)) ]
    end else begin
      ins_b @ ins_o @ ins_v @ 
      [ make_il3 ~loc (IL3stmtWriteRaw(ILlvPtr addr, fields, t_v)) ]
    end
  end

let rec translate_local_const_initialization ~env t inits target_base flds = 
  dprintf 6
    "translate_local_const_initialization : type = %a, init = %a"
    Il_formatter.pp_ctt_type t
    Ils_formatter.pp_ils_initializer inits;
  let loc = locget inits in
  match t.ct_ty, locval inits with
    Tarray(elemtyp, _), ILSinitArray(_t, l) ->
      let p = parse_type ~env elemtyp in
      let target_ptr_t = make_c_type (Tpointer p.packed_t) in
      let tempid = new_tempid ~env in
      let i0s = 
	if flds = [] then
	  [ make_il3 ~loc (IL3stmtRexp(tempid, target_ptr_t, IL3RexCoerce(target_ptr_t, target_base))) ]
	else
	  if p.is_fat then
	    let tid2 = new_tempid ~env in
	    [
	     make_il3 ~loc (IL3stmtRexp(tid2, make_c_type (Tpointer p.initializer_type),
				      IL3RexAddress(ILlvPtr target_base, flds)));
	     make_il3 ~loc (IL3stmtRexp(tempid, target_ptr_t, IL3RexCoerce(target_ptr_t, tid2)))
	    ]
	  else
	    let tid2 = new_tempid ~env in
	    [ make_il3 ~loc (IL3stmtRexp(tid2, type_void_ptr, IL3RexAddress(ILlvPtr target_base, flds)));
	     make_il3 ~loc (IL3stmtRexp(tempid, target_ptr_t, IL3RexCoerce(target_ptr_t, tid2)))
	    ]
      in
      let f n init = 
	let nid = new_tempid ~env in
	let pid = new_tempid ~env in
	let i0 = make_il3 ~loc:(locget init)
	    (IL3stmtRexp (nid, type_size_t, IL3RexConstant(CTTconstInteger (big_int_of_int n)))) in
	let i1 = make_il3 ~loc:(locget init)
	    (IL3stmtRexp (pid, target_ptr_t, IL3RexBinop(ILbinPlusPV, tempid, nid))) in
	i0 :: i1 :: translate_local_const_initialization ~env elemtyp init pid []
      in
      i0s @ Util.map_flatten_i f l
  | Tstruct sid, ILSinitStruct(_t, inits) ->
      let sd = get_struct_desc ~genv:env.global_env sid in
      Util.map_flatten
	(fun (id,init) ->
	  let (elemt,_ofs) = List.assoc id sd.str_fields_byname in
	  let elempackt = 
	    match elemt.ct_ty with
	      Tarray (t,sz) ->
		make_c_type (Tarray ((parse_type ~env t).initializer_type, sz))
	    | _ -> (parse_type ~env elemt).initializer_type
	  in
	  translate_local_const_initialization ~env elemt init target_base
	    (flds @ [translate_field_name id, elempackt])) inits
  | _, ILSinitPostFill -> []
  | (Tbuiltin _ | Tpointer _), ILSinitConstant(_t, b, o) ->
      translate_local_const_initialization_value ~env ~loc t target_base flds b o
  | _ -> assert false

let rec translate_stmt ~env s = 
  dprintf 6 "traslate_stmt: %a" Ilc_formatter.pp_ilc s;
  let loc = locget s in
  let desc = match locval s with
  | ILCstmtIf(ift,id,tgt) -> make_il3 ~loc (IL3stmtIf(ift,id,tgt))
  | ILCstmtSwitch(tid,list) -> make_il3 ~loc (IL3stmtSwitch(tid,list))
  | ILCstmtGoto(tgt) -> make_il3 ~loc (IL3stmtGoto tgt)
  | ILCstmtReturn0 -> 
      enclose_il3_sequence ~loc
	(emit_exit_code ~env ~loc @ [make_il3 ~loc (IL3stmtReturn None)])
  | ILCstmtReturn1(id) ->
      enclose_il3_sequence ~loc
	(emit_exit_code ~env ~loc @ [make_il3 ~loc (IL3stmtReturn (Some id))])
  | ILCstmtReturn2(i1,i2) ->
      let t = rettype ~env in
      let id, insn = make_compound ~env ~loc t i1 i2 in
      enclose_il3_sequence ~loc
	(insn @ emit_exit_code ~env ~loc @ [make_il3 ~loc (IL3stmtReturn (Some id))])
  | ILCstmtAssign(tid,exp) -> begin
      let ty = type_of_tempvar ~env tid in
      let add_insn, new_exp, post_insns = translate_expr1 false ~env ~loc ty exp in
      let insn = emit_e_or_r ~loc tid (translate_type ~env ty) new_exp in
      match add_insn, post_insns with
	[], [] -> insn
      |	add_insn, [] ->
	  enclose_il3_sequence ~loc [enclose_il3_parallel ~loc add_insn; insn]
      | [], post_insns ->
	  enclose_il3_sequence ~loc (insn :: post_insns)
      | add_insn, post_insns ->
	  enclose_il3_sequence ~loc
	    (enclose_il3_parallel ~loc add_insn :: insn :: post_insns)
  end
  | ILCstmtAssign2(t1,t2,exp) -> begin
      let ty1 = type_of_tempvar ~env t1 in
      let ty2 = type_of_tempvar ~env t2 in
      let add_insn, new_exp1, new_exp2, post_insns = translate_expr2 ~env ~loc (ty1,ty2) exp in
      enclose_il3_parallel ~loc
	(add_insn @
	 [ emit_e_or_r ~loc t1 (translate_type ~env ty1) new_exp1;
	   emit_e_or_r ~loc t2 (translate_type ~env ty2) new_exp2 ]
	 @ post_insns)
  end
  | ILCstmtSequence l ->
      enclose_il3_sequence ~loc (Util.list_map (translate_stmt ~env) l)
  | ILCstmtParallel l ->
      enclose_il3_parallel ~loc (Util.list_map (translate_stmt ~env) l)
  | ILCstmtRead1(tid,ILSlvVar (vt,vid,ct),fl,attr) -> begin
      let tyd = type_of_tempvar ~env tid in
      if attr <> Insert_check.attr_nocheck then 
	failwith "unimp6";
      let tyd = translate_type ~env tyd in
      let i0, t_elem, t_str, lv, fl = storage_for_variable ~env ~loc vt vid ct fl in
      let i1 = IL3stmtReadRaw(tid,tyd,lv,fl) in
      if i0 = [] then make_il3 ~loc i1
      else enclose_il3_parallel ~loc (i0 @ [make_il3 ~loc i1])
  end
  | ILCstmtRead1(tid,ILSlvTemp ts,fl,attr) -> begin
      let tyd = type_of_tempvar ~env tid in
      if attr <> Insert_check.attr_nocheck then 
	failwith "unimp6";
      let tyd = translate_type ~env tyd in
      let fl = translate_fields ~genv:env.global_env fl in
      make_il3 ~loc (IL3stmtReadRaw(tid,tyd,ILlvTemp ts,fl))
  end
  | ILCstmtRead2(t1, t2, ILSlvVar (vt, vid, ty), fl, attr) ->
      if attr <> Insert_check.attr_nocheck then 
	failwith "unimp6W";
      let tid0 = new_tempid ~env in
      let tid1 = new_tempid ~env in
      let i0, t_elem, t_str, lv, fl = storage_for_variable ~env ~loc vt vid ty fl in
      let p = parse_type ~env t_elem in
      let (tt,rwt,cdrtype) = get_destructors ~env t_elem in
      enclose_il3_parallel ~loc
	(i0 @ 
         [
	  make_il3 ~loc (IL3stmtReadRaw(tid0,p.packed_t,lv,fl));
	  make_il3 ~loc (IL3stmtCallReducibleFun(t1, IL3pr_car rwt, [tid0]));
	  make_il3 ~loc (IL3stmtCallReducibleFun(tid1,IL3pr_cdr rwt,[tid0]));
	  make_il3 ~loc (IL3stmtRexp(t2,p.cdr_t,IL3RexCoerce(p.cdr_t,tid1)));
	])
  | ILCstmtRead2(t1, t2, ILSlvTemp ts, fl, attr) ->
      if attr <> Insert_check.attr_nocheck then 
	failwith "unimp6W";
      let tid0 = new_tempid ~env in
      let tid1 = new_tempid ~env in
      let t_elem = snd (ExtList.List.last fl) in
      let p = parse_type ~env t_elem in
      let fl = translate_fields ~genv:env.global_env fl in
      let (tt,rwt,cdrtype) = get_destructors ~env t_elem in
      enclose_il3_parallel ~loc
         [
	  make_il3 ~loc (IL3stmtReadRaw(tid0,p.packed_t,ILlvTemp ts,fl));
	  make_il3 ~loc (IL3stmtCallReducibleFun(t1, IL3pr_car rwt,[tid0]));
	  make_il3 ~loc (IL3stmtCallReducibleFun(tid1, IL3pr_cdr rwt,[tid0]));
	  make_il3 ~loc (IL3stmtRexp(t2,p.cdr_t,IL3RexCoerce(p.cdr_t,tid1)));
	]
  | ILCstmtRead1(tid,ILSlvPtr (idb,ido),fl,attr) ->
      let tyd' = type_of_tempvar ~env tid in
      let tyd = original_type_from_val_type tyd' in
      make_ptr_operations ~env ~loc ~idb ~ido ~tyd ~fl
	~genf:make_generic_read1 ~specf:make_specific_read1
	tid [tid]
  | ILCstmtRead2(t1,t2,ILSlvPtr (idb,ido),fl,attr) ->
      let tyd = original_type_from_val_type
	  (type_of_tempvar ~env t2) in
      make_ptr_operations ~env ~loc ~idb ~ido ~tyd ~fl
	~genf:make_generic_read2 ~specf:make_specific_read2
	(t1,t2) [t1; t2]
  | ILCstmtWrite1(ILSlvPtr (idb,ido),fl,tid,attr) ->
      let tyd' = type_of_tempvar ~env tid in
      let new_tyd = translate_type ~env tyd' in
      let tyd = original_type_from_val_type tyd' in
      make_ptr_operations ~env ~loc ~idb ~ido ~tyd ~fl
	~genf:make_generic_write1 ~specf:make_specific_write1
	tid []
  | ILCstmtWrite2(ILSlvPtr (idb,ido),fl,t1,t2,attr) ->
      let tyd = original_type_from_val_type
	  (type_of_tempvar ~env t2) in
      make_ptr_operations ~env ~loc ~idb ~ido ~tyd ~fl
	~genf:make_generic_write2 ~specf:make_specific_write2
	(t1,t2) []
  | ILCstmtWrite1(ILSlvVar (vt,vid,ty),fl,tid,attr) -> begin
      let tys = type_of_tempvar ~env tid in
      if attr <> Insert_check.attr_nocheck then 
	failwith "unimp6o";
      let i0, t_elem, t_str, lv, fl = storage_for_variable ~env ~loc vt vid ty fl in
      let i1 = make_il3 ~loc (IL3stmtWriteRaw (lv, fl, tid)) in
      if i0 = [] then i1
      else enclose_il3_parallel ~loc (i0 @ [ i1 ])
  end
  | ILCstmtWrite2(ILSlvVar (vt,vid,ty),fl,t1,t2,attr) -> begin
      let tys = type_of_tempvar ~env t2 in
      if attr <> Insert_check.attr_nocheck then
	failwith "unimp6oW";
      let i00, t_elem, t_str, lv, fl = storage_for_variable ~env ~loc vt vid ty fl in
      let p = parse_type ~env t_elem in
      assert p.is_fat;
      let tid, i0 = make_compound ~env ~loc t_elem t1 t2 in
      enclose_il3_parallel ~loc (i0 @ i00 @ [ make_il3 ~loc (IL3stmtWriteRaw (lv, fl, tid)) ])
  end
  | ILCstmtAbort error -> begin
      let tid1 = new_tempid ~env in
      let tid_error = new_tempid ~env in
      enclose_il3_sequence ~loc
	[
	 make_il3 ~loc (IL3stmtRexp (tid1, type_base_t,
				IL3RexConstant(constant_zero)));
	 make_il3 ~loc (IL3stmtRexp (tid_error, type_fsc_error,
				exp_of_error_constant error));
	 make_il3 ~loc (IL3stmtCallAbort(tid1, tid_error));
       ]
  end
  | ILCstmtAbortIf(expr, error) -> begin
      let tid = new_tempid ~env in
      let tid1 = new_tempid ~env in
      let tid_error = new_tempid ~env in
      let add_insn, new_exp, post_insns = 
	translate_expr1 false ~env ~loc (ILRtypeVal (type_boolean, Value_any)) expr
      in
      let insn = emit_e_or_r ~loc tid type_boolean new_exp in
      let icond = 
	match add_insn, post_insns with
	  [], [] -> insn
	| add_insn, post_insns ->
	    enclose_il3_sequence ~loc (add_insn @ [insn] @ post_insns)
      in
      enclose_il3_sequence ~loc
	[ icond;
	  make_il3 ~loc
	    (IL3stmtConditional
	       ([], tid, 
		enclose_il3_sequence  ~loc
		  [
		   make_il3 ~loc (IL3stmtRexp (tid1, type_base_t,
					  IL3RexConstant(constant_zero)));
		   make_il3 ~loc (IL3stmtRexp (tid_error, type_fsc_error,
					  exp_of_error_constant error));
		   make_il3 ~loc (IL3stmtCallAbort(tid1, tid_error))],
		enclose_il3_sequence ~loc []))
	]
  end
  | ILCstmtDeclScalar (HeapVar, cty, id)
  | ILCstmtDeclBulk (HeapVar, cty, id)
  | ILCstmtDeclScalar (StackVar, cty, id)
  | ILCstmtDeclBulk (StackVar, cty, id) ->
      enclose_il3_parallel ~loc [] (* now in prescan *)
  | ILCstmtInitialize((HeapVar | StackVar (* TODO *)) as vt, cty, id, inits) -> begin
      let wholetyp, elemtyp, size = match cty.ct_ty with
	Tarray(t, Some s) -> cty, t, s
      | Tstruct _ -> cty, cty, unit_big_int
      | _ -> assert false
      in
      dprintf 6 "BULKINIT: %a" Il_formatter.pp_ctt_type cty;
      let inits = 
	begin
	  dprintf 6 ">>";
	  let t1 = new_tempid ~env in 
	  let p = parse_type ~env elemtyp in
	  let pt = make_c_type (Tpointer p.packed_t) in
	  dprintf 6 "  cty=%a" Il_formatter.pp_ctt_type cty;
	  let i0, _elemt, sty, lv, fl = storage_for_variable ~env ~loc vt id cty [] in
	  let dt = get_type_of_field sty fl in
	  dprintf 6 "  storage type=%a" Il_formatter.pp_ctt_type dt;
	  let ptarget = new_tempid ~env in 
	  let i1 = make_il3 ~loc (IL3stmtRexp (ptarget, make_c_type (Tpointer dt), IL3RexAddress(lv, fl))) in
	  (* let ctarget = new_tempid ~env in 
	  let i2 = make_il3_raw (IL0stmtDefTemp (ctarget, pt, ILexpCoerce(pt, ptarget))) in *)
	  let ctarget = ptarget in
	  let r = i0 @ (i1 (* :: i2 *) :: translate_local_const_initialization ~env wholetyp inits ctarget []) in
	  dprintf 6 "<<"; r
	end
      in
      enclose_il3_parallel ~loc inits
  end
  | ILCstmtDeclScalar ((GlobalVar | RegVar), _, _)
  | ILCstmtDeclBulk ((GlobalVar | RegVar), _, _)
  | ILCstmtInitialize ((GlobalVar | RegVar), _, _, _) -> assert false

  | ILCstmtRead1
      (_, (ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)), _, _)
  | ILCstmtRead2
      (_, _, (ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)), _, _)
  | ILCstmtWrite1
      ((ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)|ILSlvTemp _), _, _, _)
  | ILCstmtWrite2
      ((ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)|ILSlvTemp _), _, _, _, _)
    -> failwith_p "unimp5 %a" Ilc_formatter.pp_ilc s
  in
  desc

(* prescan phase: pickup all local variable declarations *)

type prescan_env = 
    { heapvars : (c_type * identifier * location) Glist.t;
      stackvars : (c_type * identifier * location) Glist.t;
    }

let rec prescan_stmt ~penv s = 
  dprintf 6 "prescan: %a" Ilc_formatter.pp_ilc s;
  let loc = locget s in
  match locval s with
  | ILCstmtIf(ift,id,tgt) -> ()
  | ILCstmtSwitch(tid,list) -> ()
  | ILCstmtGoto(tgt) -> ()
  | ILCstmtReturn0 -> ()
  | ILCstmtReturn1(id) -> ()
  | ILCstmtReturn2(i1,i2) -> ()
  | ILCstmtAssign(tid,exp) -> ()
  | ILCstmtAssign2(t1,t2,exp) -> ()
  | ILCstmtSequence l ->
      List.iter (prescan_stmt ~penv) l
  | ILCstmtParallel l ->
      List.iter (prescan_stmt ~penv) l
  | ILCstmtRead1(tid,ILSlvVar (vt,vid,ct),fl,attr) -> ()
  | ILCstmtRead1(tid,ILSlvTemp ts,fl,attr) -> ()
  | ILCstmtRead2(t1, t2, ILSlvVar (vt, vid, ty), fl, attr) -> ()
  | ILCstmtRead2(t1, t2, ILSlvTemp ts, fl, attr) -> ()
  | ILCstmtRead1(tid,ILSlvPtr (idb,ido),fl,attr) -> ()
  | ILCstmtRead2(t1,t2,ILSlvPtr (idb,ido),fl,attr) -> ()
  | ILCstmtWrite1(ILSlvPtr (idb,ido),fl,tid,attr) -> ()
  | ILCstmtWrite2(ILSlvPtr (idb,ido),fl,t1,t2,attr) -> ()
  | ILCstmtWrite1(ILSlvVar (vt,vid,ty),fl,tid,attr) -> ()
  | ILCstmtWrite2(ILSlvVar (vt,vid,ty),fl,t1,t2,attr) -> ()
  | ILCstmtAbort error -> ()
  | ILCstmtAbortIf(expr, error) -> ()

  | ILCstmtDeclScalar (HeapVar, cty, id)
  | ILCstmtDeclBulk (HeapVar, cty, id) ->
      Glist.put penv.heapvars (cty, id, loc);
      ()

  | ILCstmtDeclScalar (StackVar, cty, id)
  | ILCstmtDeclBulk (StackVar, cty, id) ->
      Glist.put penv.stackvars (cty, id, loc);
      ()

  | ILCstmtInitialize((HeapVar | StackVar (* TODO *)) as vt, cty, id, inits) -> ()
  | ILCstmtDeclScalar ((GlobalVar | RegVar), _, _)
  | ILCstmtDeclBulk ((GlobalVar | RegVar), _, _)
  | ILCstmtInitialize ((GlobalVar | RegVar), _, _, _) -> assert false

  | ILCstmtRead1
      (_, (ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)), _, _)
  | ILCstmtRead2
      (_, _, (ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)), _, _)
  | ILCstmtWrite1
      ((ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)|ILSlvTemp _), _, _, _)
  | ILCstmtWrite2
      ((ILSlvSVar (_, _, _)|ILSlvPtrToFunc (_, _)|ILSlvTemp _), _, _, _, _)
    -> failwith_p "unimp5p %a" Ilc_formatter.pp_ilc s

let prescan ~make_frame ~env ~function_loc body = 
  let penv =
    { heapvars = Glist.empty ();
      stackvars = Glist.empty ();
    } in
  Array.iter
    (fun b ->
      List.iter (prescan_stmt ~penv) b.code) body;
  
  let heapvars, stackvars = Glist.to_list penv.heapvars, Glist.to_list penv.stackvars in

  List.iter
    (fun (cty, id, loc) ->
      let elemtyp, size = match cty.ct_ty with
	Tarray(t, Some s) -> t, s
      | Tarray(t, None) -> failwith "panic 2063: allocating unsized local array"
      | _ -> cty, unit_big_int
      in
      check_size_of_local_variable ~genv:env.global_env id cty;
      let stype = get_storage_type ~genv:env.global_env cty in
      let tid_p = new_tempid ~env in
      let tid_t = new_tempid ~env in
      let tid_s = new_tempid ~env in
      let ptrval_name = translate_ptr_local_heap_variable_name ~genv:env.global_env id cty in
      let ptrval_type = 
	make_c_type (Tpointer { stype with ct_volatile_p = cty.ct_volatile_p }) in
      Glist.put env.prologue0
	(make_il0 ~loc
	   (IL0stmtDeclAutoScalar (Auto, ptrval_type, ptrval_name, None)));
      Glist.put env.prologue1
	(enclose_il3_parallel ~loc
	   [make_il3 ~loc (IL3stmtRexp (tid_s, type_size_t, IL3RexConstant(CTTconstInteger size)));
	    make_il3 ~loc (IL3stmtRexp (tid_t, type_typeinfo_ptr, IL3RexConstant(CTTconstTypeInfo elemtyp)));
	    make_il3 ~loc (IL3stmtCallImmobileFun (tid_p, Alloc_heapvar, [tid_t; tid_s]));
	    make_il3 ~loc (IL3stmtWriteRaw (ILlvVar(ptrval_name, ptrval_type), [], tid_p))
	  ]);
      if not make_frame then
	(* in make_frame mode, variables are released by stack_frame management. *)
	env.exit_code <-
	  env.exit_code @ [(IL3stmtCallImmobileOp (Dealloc_heapvar, [tid_p]))];
      ())
    heapvars;

  if make_frame then begin
    let n_hvar = List.length heapvars in
    let inst_fvar = 
      if n_hvar > 0 then begin
	Glist.put env.prologue0
	  (make_il0 ~loc:function_loc
	     (IL0stmtDeclBulk
		(Auto, type_frame_variables_n (n_hvar + 1), id_local_frame_variables, 
		 Some
		   (locput ~loc:function_loc
		      (ILinitArray
			 (type_frame_variables_entry, 
			  [locput ~loc:function_loc
			     (ILinitAbstractCtt
				(make_cttm_expr ~loc:function_loc
				   (CTTMexpConstant constant_zero)
				   type_frame_variables_entry))]))))));
	Util.list_iteri
	  (fun i (cty, id, loc) ->
	    let tid_p = new_tempid ~env in
	    let tid_pp = new_tempid ~env in
	    let tid_i = new_tempid ~env in
	    let tid_v = new_tempid ~env in
	    
	    Glist.put env.prologue1
	      (enclose_il3_parallel ~loc
		 [
		  make_il3 ~loc
		    (IL3stmtRexp(tid_pp, type_frame_variables,
				 IL3RexAddress(lv_local_frame_variables, [])));
		  make_il3 ~loc
		    (IL3stmtRexp(tid_i, type_int,
				 IL3RexConstant(CTTconstInteger (big_int_of_int i))));
		  make_il3 ~loc
		    (IL3stmtRexp(tid_p, make_c_type (Tpointer type_frame_variables_entry),
				 IL3RexBinop(ILbinPlusPV, tid_pp, tid_i)));
		  make_il3 ~loc
		    (IL3stmtReadRaw(tid_v, type_frame_variables_entry,
				    ILlvVar(translate_ptr_local_heap_variable_name ~genv:env.global_env id cty,
					    type_frame_variables_entry), []));
		  make_il3 ~loc
		    (IL3stmtWriteRaw(ILlvPtr tid_p, [], tid_v))]))
	  heapvars;
	(fun tid -> 
	  make_il3 ~loc:function_loc
	    (IL3stmtReadRaw(tid, make_c_type (Tpointer type_frame_variables_entry), 
			    lv_local_frame_variables, []))) (* type ok? *)
      end
      else
	(fun tid -> 
	  make_il3 ~loc:function_loc
	    (IL3stmtRexp(tid, make_c_type (Tpointer type_frame_variables_entry), 
			 IL3RexConstant(constant_zero))))
    in

    Glist.put env.prologue0
      (make_il0 ~loc:function_loc
	 (IL0stmtDeclAutoScalar(Auto, type_stack_frame, id_local_frame, None)));

    let tid_f = new_tempid ~env in
    let tid_fv = new_tempid ~env in
    let tid_s = new_tempid ~env in

    Glist.put env.prologue1
      (enclose_il3_parallel ~loc:function_loc
	    [make_il3 ~loc:function_loc
	       (IL3stmtRexp(tid_f, make_c_type (Tpointer type_stack_frame),
			    IL3RexAddress(ILlvVar(id_local_frame, type_stack_frame), [])));
	     inst_fvar tid_fv;
	     make_il3 ~loc:function_loc
	       (IL3stmtRexp (tid_s, make_c_type (Tpointer type_char),
			     IL3RexConstant(CTTconstString env.name)));
	     make_il3 ~loc:function_loc
	       (IL3stmtCallImmobileOp(Enter_stack_unwind_area, [tid_f; tid_s; tid_fv]));
	   ]);
    env.exit_code <-
      env.exit_code @ [(IL3stmtCallImmobileOp (Exit_stack_unwind_area, []))]
  end else ();

  List.iter
    (fun (cty, id, loc) ->
      let elemtyp, size = match cty.ct_ty with
	Tarray(t, Some s) -> t, s
      | Tarray(t, None) -> failwith "panic 2063: allocating unsized local array"
      | _ -> cty, unit_big_int
      in
      check_size_of_local_variable ~genv:env.global_env id cty;
      let stype = get_storage_type ~genv:env.global_env cty in
      
      let val_name = translate_local_stack_variable_name ~genv:env.global_env id cty in
      
      let header =
	cttm_macro_for_blockheader ~genv:env.global_env ~loc elemtyp (Some size)
      in
      
      let init = 
	locput ~loc:function_loc
	  (ILinitStruct 
	     (stype,
	      ["header", locput ~loc:function_loc (ILinitAbstractCtt header)])) in
      
      Glist.put env.prologue0
	(make_il0 ~loc:function_loc
	   (IL0stmtDeclBulk (Auto, stype, val_name, Some init)));
      ())
    stackvars;
  ()

let translate_basic_block ~env ?(boundmap = VarMap.empty) b = 
  env.boundary_cache <- boundmap;
  let b = { b with code = Util.list_map (translate_stmt ~env) b.code } in
  let bc = env.boundary_cache in
  env.boundary_cache <- VarMap.empty;
  b, bc

let rec translate_basic_blocks ~env n blocks = 
  let donemap = Earray.empty () in
  let cachemap = Earray.empty () in
  let rec iter n =
    if Earray.mem donemap n then 
      ()
    else begin
      dprintf 6 "processing block %d" n;
      let idom = blocks.(n).immediate_dominator in
      let bc =
	if idom = n then
	  VarMap.empty
	else begin
	  if Earray.mem donemap idom = false then begin
	    dprintf 6 "... we need to process block %d" idom;
	    iter idom;
	    dprintf 6 "... done. coming back to block %d" n;
	  end;
	  Earray.get cachemap idom
	end
      in
      let b, bc = translate_basic_block ~env ~boundmap:bc blocks.(n) in
      Earray.set donemap n b;
      Earray.set cachemap n bc
    end
  in
  for i = 0 to Array.length blocks - 1 do
    iter i
  done;
  Array.init (Array.length blocks) (Earray.get donemap)

let translate_function_body ~genv ~loc:function_loc ~denv ~name ct args
    ({ Il.body = body;
       Il.max_variable_number = max_variable_number;
       Il.variable_environment = variable_environment;
       Il.arguments = arguments } as f) =
  let arguments_b = Earray.empty () in
  let arguments_v = Earray.empty () in
  let arguments_n = Earray.empty () in
  let is_polymorphic = is_type_polymorphic_func ~genv ct in
  let env =
    { max_var = max_variable_number;
      f = f;
      is_polymorphic = is_polymorphic;
      global_env = genv;
      decl_env = denv;
      rettype = rettype_of_functype ct;
      arguments_b = arguments_b;
      arguments_v = arguments_v;
      arguments_n = arguments_n;
      prologue0 = Glist.empty ();
      prologue1 = Glist.empty ();
      epilogue = Glist.empty ();
      exit_code = [];
      struct_is_continuous = Earray.empty ();
      boundary_cache = VarMap.empty;
      name = name;
    } in
  let arglen = List.length args in
  let arg_is_wide = Array.create arglen false in
  let widef i t name =
    arg_is_wide.(i) <- true;
    ["FAB_" ^ name; "FAV_" ^ name]
  in
  let narrowf i t name = 
    arg_is_wide.(i) <- false;
    ["FA_" ^ name]
  in
  let varargsf l = ["FAva_b"; "FAva_v"] in
  let args = 
    let args = flatten_mapi_with_argtypes ~env ~widef ~narrowf ~varargsf ct args
    in
    if is_polymorphic then "FAtptr_b" :: args else args
  in
  let _ = 
    let n = ref (if is_polymorphic then 1 else 0) in
    for i = 0 to arglen - 1 do
      if arg_is_wide.(i) then begin
	Earray.set arguments_b i !n;
	incr n;
	Earray.set arguments_v i !n;
	incr n;
      end else begin
	Earray.set arguments_n i !n;
	incr n
      end
    done;
  in
  
  prescan ~function_loc
    ~make_frame:(f.function_attributes.setjmp_called || !force_emit_stack_unwind)
    ~env body;

  let body = (* Array.map (translate_basic_block ~env) body *) 
    translate_basic_blocks ~env 0 body
  in
  assert (Glist.to_list env.epilogue = []);

  let body = Il3.add_code_to_top ~function_loc (Glist.to_list env.prologue1) body in
  let ctnew = translate_c_type ~env ct in 
  let argtypes = 
    match ctnew.ct_ty with Tfunction(a,_,_) -> a | _ -> assert false
  in
  let new_varenv = 
    let ve = Earray.empty () in
    Array.iteri
      (fun i v ->
	dprintf 9 "new_varenv %d: %s" i (Option.default "-" v.original_name);
	Earray.set ve i 
	  {v with variable_type = translate_type ~env v.variable_type})
      f.Il.variable_environment;
    ve
  in
  let f = { body = body;
	    max_variable_number = env.max_var;
	    variable_environment = new_varenv;
	    prologue = Glist.to_list env.prologue0;
	    more_info = ()
	  } in
  ctnew, args, f

let translate_global_storage_class gs = 
  match gs with
    Extern _ | Global _ -> Global []
  | ModuleStatic -> ModuleStatic

let translate_function ~genv ~denv ~loc gs ct id args f = 
  let new_id = 
    translate_global_function_name ~genv id ct in
  let ct, args, f = translate_function_body ~genv ~denv ~loc ~name:id ct args f in
  locput ~loc (IL3declFunction(translate_global_storage_class gs, ct, new_id, args, f))

let rec translate_global_initializer_list ~genv ~denv i =
  let loc = locget i in
  match locval i with
    ILSinitArray(t,l) ->
      locput ~loc
	(CTTMinitList 
	   (Util.list_map (translate_global_initializer_list ~genv ~denv) l))
  | ILSinitConstant(t,b,o) -> 
      locput ~loc
	(CTTMinitExp (translate_global_initializer_value ~genv ~denv ~loc ~is_global:true t b o))
  | ILSinitPostFill -> assert false
  | ILSinitStruct(t,l) -> 
      locput ~loc
	(CTTMinitList
	   (Util.list_map (fun (name, i) -> translate_global_initializer_list ~genv ~denv i) l))

let translate_global_initializer ~genv ~denv ~loc t i =
  let elemnum, elemtype = size_and_elemtype_of_onmemory_type t in
  let header = cttm_macro_for_blockheader ~genv ~loc elemtype elemnum
  in
  let inits = match i with
    None -> []
  | Some l -> [translate_global_initializer_list ~genv ~denv l]
  in
  locput ~loc (CTTMinitList (locput ~loc (CTTMinitExp header) :: inits))

exception FG_not_supported (* TODO: unimplemented *)

let enclose_instr_to_function max_var ~function_loc i = 
  { body = [| { location = function_loc;
		predecessor = []; successor = []; immediate_dominator = 0; nest_level = 0;
		phi_function = []; code = i } |];
    max_variable_number = max_var;
    variable_environment = Earray.empty ();
    prologue = []; more_info = ()
  }

let generate_bridge_function ~genv ~loc gs id ct args = 
  let at, varf, rt = match ct.ct_ty with Tfunction(a,v,r) -> a,v,r | _ -> assert false in
  let env = { max_var = 0; f = Obj.magic 0; is_polymorphic = false; global_env = genv; decl_env = Obj.magic 0;
	      rettype = rettype_of_functype ct; 
	      arguments_b = Earray.empty (); arguments_n = Earray.empty (); arguments_v = Earray.empty ();
	      prologue0 = Glist.empty (); prologue1 = Glist.empty (); epilogue = Glist.empty ();
	      exit_code = [];
	      struct_is_continuous = Earray.empty ();
	      boundary_cache = VarMap.empty;
	      name = ""
	    } in
  let new_id = translate_global_bridge_function_name ~genv id ct in
  let spec_id = translate_global_function_name ~genv id ct in
  let argofs = ref 0 in
  let args_rev = ref [] in
  let base_tid = new_tempid ~env in
  let instr_rev = ref [ make_il3 ~loc (IL3stmtRexp(base_tid, type_base_t, IL3RexArgument 1)) ] in
  let put v l = v := List.rev_append l !v in
  List.iter2
    (fun ty name ->
      let p = parse_type_genv ~genv ty in
      let incr, ofs = match p.size with
	Size_byte | Size_hword | Size_word -> Fsc_config.sizeof_pointer, !argofs
      | Size_dword -> Fsc_config.sizeof_pointer * 2, !argofs
      | Size_other (Some s) -> (int_of_big_int s + Fsc_config.sizeof_pointer - 1) / Fsc_config.sizeof_pointer * Fsc_config.sizeof_pointer, !argofs
      | Size_other None ->
	  let s = sizeof_type_genv ~genv ty in
	  dprintf 0 "oop: size of %a unknown (%s)?" Il_formatter.pp_ctt_type ty (string_of_big_int s);
	  raise FG_not_supported (* TODO? *)
      in
      argofs := !argofs + incr;
      let tido = new_tempid ~env in
      put instr_rev [ make_il3 ~loc
			(IL3stmtRexp(tido, type_size_t,
				     IL3RexConstant (CTTconstInteger (big_int_of_int ofs)))) ];
      match p.is_fat with
	true ->
	  let tid1, tid2 = new_tempid ~env, new_tempid ~env in
	  put instr_rev (make_generic_read2 ~env ~loc (tid1, tid2) ty None base_tid tido);
	  put args_rev [tid1; tid2];
      | false -> begin
	  let b_name = "FAB_" ^ name in
	  match p.size with
	    Size_byte | Size_hword -> begin
	      assert (not p.is_fat);
	      if p.is_floating then
		  failwith "unimp2015";
	      (* read by word size *)
	      let tidv = new_tempid ~env in
	      let tidw = new_tempid ~env in
	      let tid1 = new_tempid ~env in
	      put instr_rev [make_il3 ~loc
			       (IL3stmtCallReaderHelper
				  (tidv, Word, base_tid, tido));
			     make_il3 ~loc
			       (IL3stmtCallReducibleFun
				  (tidw, IL3pr_cdr Value, [tidv]));
			     make_il3 ~loc
			       (IL3stmtRexp
				  (tid1, type_word,
				   IL3RexCoerce (p.packed_t, tidw)))];
	      put args_rev [tid1]
	    end
	  | _ -> begin
	      let tid1 = new_tempid ~env in
	      put instr_rev (make_generic_read1 ~env ~loc tid1 ty None base_tid tido);
	      put args_rev [tid1]
	  end
      end)
    at args;
  if varf then begin
    let tido = new_tempid ~env in
    put instr_rev [ make_il3 ~loc
		      (IL3stmtRexp(tido, type_size_t,
				   IL3RexConstant (CTTconstInteger (big_int_of_int !argofs)))) ];
    put args_rev [base_tid; tido];
  end;
  if is_type_polymorphic_func ~genv ct then begin
    let tid_tptr = new_tempid ~env in
    put instr_rev [ make_il3 ~loc
		      (IL3stmtRexp(tid_tptr, type_base_t,
				   IL3RexArgument 0)) ];
    args_rev := !args_rev @ [ tid_tptr ] (* put to the top *)
  end;
  let va_end_insn = 
    if not varf then begin
      [ make_il3 ~loc (IL3stmtCallImmobileOp (Finish_varargs, [base_tid])) ]
    end else
      []
  in
  if rt.ct_ty = Tvoid then begin
    let retp_id = new_tempid ~env in
    let retg_id = new_tempid ~env in
    let tid0 = new_tempid ~env in
    put instr_rev
      [ make_il3 ~loc
	  (IL3stmtIexp(retp_id, type_void, IL3IexInvoke(ILlvVar(spec_id, type_void), List.rev !args_rev))) ];
    put instr_rev va_end_insn;
    put instr_rev
      [ make_il3 ~loc (IL3stmtRexp(tid0, type_dword, IL3RexConstant(constant_zero)));
	make_il3 ~loc (IL3stmtCallReducibleFun (retg_id, IL3pr_convert Dvalue_of_dword, [tid0]));
	make_il3 ~loc (IL3stmtReturn(Some (retg_id)))
      ]
  end else begin
    let rt_p = parse_type ~env rt in
    if rt_p.is_indirect_ret then begin
      let retp_id = new_tempid ~env in
      let retg_id = new_tempid ~env in
      let tid0 = new_tempid ~env in
      put instr_rev
	[ make_il3 ~loc (IL3stmtIexp(retp_id, rt_p.packed_t, IL3IexInvoke(ILlvVar(spec_id, type_void), List.rev !args_rev))) ];
      put instr_rev va_end_insn;
      let tid_tptr = new_tempid ~env in
      put instr_rev [ make_il3 ~loc (IL3stmtRexp(tid_tptr, type_base_t, IL3RexArgument 0)) ];
      require_name (GNrwmeth rt); (* for generic_writer *)
      put instr_rev
	[ 
	  make_il3 ~loc (IL3stmtRexp(tid0, type_dword, IL3RexConstant(constant_zero)));
	  make_il3 ~loc (IL3stmtCallWriterHelper(rt_p.rw_type, tid_tptr, tid0, retp_id, None));
	  make_il3 ~loc (IL3stmtCallReducibleFun (retg_id, IL3pr_convert Dvalue_of_dword, [tid0]));
	  make_il3 ~loc (IL3stmtReturn(Some (retg_id)))
	]
    end else begin
      let retp_id = new_tempid ~env in
      put instr_rev
	[make_il3 ~loc (IL3stmtIexp
			  (retp_id, rt_p.packed_t, 
			   IL3IexInvoke(ILlvVar(spec_id, type_void), List.rev !args_rev)))];
      put instr_rev va_end_insn;
      let retg_id = match rt_p.packed_to_generic with
	None -> retp_id
      | Some lv ->
	  let retg_id = new_tempid ~env in
	  put instr_rev [make_il3 ~loc
			   (IL3stmtCallReducibleFun(retg_id, lv, [retp_id]))];
	  retg_id
      in
      if rt_p.size < Size_word then begin
	let dv_id = new_tempid ~env in
	put instr_rev 
	  [make_il3 ~loc (IL3stmtCallReducibleFun
			    (dv_id,
			     IL3pr_convert Dvalue_of_dword, [retg_id]));
	   make_il3 ~loc (IL3stmtReturn (Some dv_id))]
      end else if rt_p.generic_t == type_dvalue then
	put instr_rev [make_il3 ~loc (IL3stmtReturn (Some retg_id))]
      else if rt_p.generic_t == type_value then begin
	let dv_id = new_tempid ~env in
	put instr_rev 
	  [make_il3 ~loc 
	     (IL3stmtCallReducibleFun
		(dv_id,
		 IL3pr_convert Dvalue_of_value, [retg_id]));
	   make_il3 ~loc (IL3stmtReturn (Some dv_id))]
      end else assert false
    end
  end;
  locput ~loc
    (IL3declFunction(translate_global_storage_class gs, type_genfunc, new_id, ["FAtptr_b"; "FAva_b"],
		     enclose_instr_to_function ~function_loc:loc env.max_var (List.rev !instr_rev)))

let generate_dummy_bridge_function ~genv ~loc gs id ct args = 
  let new_id = translate_global_bridge_function_name ~genv id ct in
  locput ~loc
    (IL3declFunction
       (translate_global_storage_class gs, type_genfunc, new_id, ["FAtptr_b"; "FAva_b"],
	enclose_instr_to_function ~function_loc:loc 3
	  [
	   make_il3 ~loc (IL3stmtRexp (1, type_base_t,
				       IL3RexConstant(constant_zero)));
	   make_il3 ~loc (IL3stmtRexp (2, type_fsc_error,
				       IL3RexConstant(CTTconstAbstract "ERR_UNIMPLEMENTED")));
	   make_il3 ~loc (IL3stmtCallAbort(1, 2));
	 ]
       ))

let generate_bridge_function ~genv ~loc gs id ct args = 
  try
    generate_bridge_function ~genv ~loc gs id ct args 
  with
    FG_not_supported ->
      generate_dummy_bridge_function ~genv ~loc gs id ct args
      
let generate_function_stub ~genv ~loc gs id ct args = 
  require_name (GNtypeinfo ct);
  let typeinfo = 
    make_cttm_expr ~loc
      (CTTMexpRead
	 (CTTMlvVar(encoded_typeinfo_name ~genv ct, type_typeinfo_init),
	  ["val", type_typeinfo_struct])) type_typeinfo_struct in
  let allocsize_e = 
    make_cttm_expr ~loc (CTTMexpConstant (CTTconstInteger unit_big_int)) type_size_t in
  let header =
    make_cttm_expr ~loc (CTTMexpInvoke(lv_cttm_macro_emit_header,
				  [typeinfo; allocsize_e]))
      type_fsc_header
  in
  locput ~loc
    (IL3declVariable
       (translate_global_storage_class gs, type_function_stub_init, 
	translate_global_variable_name ~genv ~known:Known id ct,
	Some 
	  (emit_cttminitlist ~loc
	     [emit_cttminit ~loc header;
	      emit_cttminitlist ~loc
		[emit_cttminit ~loc 
		   (make_cttm_expr ~loc 
		      (CTTMexpCoerce
			 (type_void_ptr,
			  make_cttm_expr ~loc 
			    (CTTMexpRead
			       (CTTMlvVar (translate_global_function_name ~genv id ct, type_void_ptr),
				[]))
			    type_void_ptr))
		      type_void_ptr);
		 emit_cttminit ~loc
		   (make_cttm_expr ~loc
		      (CTTMexpRead 
			 (CTTMlvVar (translate_global_bridge_function_name ~genv id ct, type_genfunc),
		       []))
		      type_genfunc)]])))

let translate_funcdecl_to_prototype f = 
  locmap
    (function
	IL3declFunction(gs, ct, id, args, f) ->
	  IL3declVariable(gs, ct, id, None)
      | _ -> assert false)
    f

let create_denv t = 
  let denv = Hashtbl.create 16 in
  dprintf 6 "Creating denv...";
  List.iter
    (fun decl -> match locval decl with
	ILdeclFunction(_gs, _ct, id, _args, _t) ->
	  dprintf 6 "known: %s" id;
	  Hashtbl.add denv id Known;
      | ILdeclVariable(_gs, ct, id, None) ->
	  if C_typing.is_type_loose_function ct then begin
	    dprintf 6 "poly.: %s" id;
	    Hashtbl.add denv id Unknown_polymorphic
	  end else begin
	    dprintf 6 "mono.: %s" id;
	    Hashtbl.add denv id Unknown_monomorphic
	  end
      | ILdeclVariable(_gs, _ct, id, Some _init) ->
	  dprintf 6 "known: %s" id;
	  Hashtbl.add denv id Known)
    t;
  denv

let f ~genv t =
  let denv = create_denv t in
  let function_predeclarations = Glist.empty () in
  dprintf_start "translating...";
  let new_decls = 
    Util.map_flatten
      (fun decl ->
	let loc = locget decl in
	match locval decl with
	  ILdeclFunction(gs, ct, id, args, f) -> begin
	    dprintf_progress "%s" id;
	    let f = translate_function ~genv ~denv ~loc gs ct id args f in
	    if !compiler_mode <> StdlibImplementation then begin
	      let stubf = 
		generate_bridge_function ~genv ~loc gs id ct args 
	      in
	      let stubrec = generate_function_stub ~genv ~loc gs id ct args in
	      Glist.append function_predeclarations 
		(Util.list_map translate_funcdecl_to_prototype [f; stubf]);
	      Glist.put function_predeclarations
		(locput ~loc
		   (IL3declVariable(gs,
				    type_function_stub_init,
				    translate_global_variable_name ~genv ~known:Known id ct, None)));
	      [f; stubf; stubrec]
	    end
	    else begin
	      Glist.append function_predeclarations 
		(Util.list_map translate_funcdecl_to_prototype [f]);
	      [f]
	    end
	  end
	| ILdeclVariable (gs,ct,id,init) ->
	    match ct.ct_ty with
	      Tfunction _ -> begin
		match gs with
		  Extern l when not (List.mem_assoc "external" l )->
		    let new_id = 
		      translate_global_function_name ~genv id ct in
		    let new_ct = translate_c_type_genv ~genv ct in
		    [ locput ~loc (IL3declVariable (Extern [], new_ct, new_id, None));
		      locput ~loc (IL3declVariable(Extern [],
						   type_function_stub_init,
						   translate_global_variable_name ~genv
						     ~known:(Hashtbl.find denv id) id ct, None)) ]
		      (* TODO: definition and/or declaration of cast stub function *)
		| _ -> []
	      end
	    | _ ->
		let new_gs, emit, init =
		  match gs, init with
		    Extern s, None when List.mem_assoc "emit_typeinfo" s -> 
		      Extern [], false, None
		  | Extern _, None -> Extern [], true, None
		  | Global _ext, None ->
		      Extern [], true, None
		  | Global _ext, _ ->
		      Global [], true, Some (translate_global_initializer ~genv ~denv ~loc ct init) 
		  | ModuleStatic, _ -> 
		      ModuleStatic, true, Some (translate_global_initializer ~genv ~denv ~loc ct init) 
		  | _ -> assert false
		in
		let emit_t = get_storage_type ~genv ~incomplete_ok:true ct in
		let emit_t = { emit_t with ct_volatile_p = ct.ct_volatile_p } in
		if emit then begin
		  Glist.append function_predeclarations
		    [ locput ~loc
			(IL3declVariable(new_gs, emit_t, 
					 translate_global_variable_name ~genv ~known:Known id ct, None)) ];
		  [ locput ~loc
		      (IL3declVariable(new_gs, emit_t,
				       translate_global_variable_name ~genv ~known:Known id ct, init)) ]
		end else begin
		  (* get_storage_type is called above, side-effect (require_name) is already performed *)
		  []
		end
      )
      t
  in
  dprintf_end "done.";
  Glist.to_list function_predeclarations @ new_decls
