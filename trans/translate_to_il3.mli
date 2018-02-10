(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Main translator of Fail-Safe C (new implementation, from ILC to IL3) *)

val f :
  genv:Ctt_abstree.environment ->
  (Ilc.ilc, Ils.ilr_type, Ils.ils_initializer) Il.il_global_declaration_base
  list -> (Il3.temp_id, unit) Il3.il3_global_declaration list
(** The main function. *)

(**{6 support routines used in other modules} *)

type size_info =
    Size_byte
  | Size_hword
  | Size_word
  | Size_dword
  | Size_other of Big_int_infix.big_int option

type type_properties = {
  size : size_info;
  is_struct : bool;
  is_pointer : bool;
  is_fat : bool;
  is_floating : bool;
  is_indirect_ret : bool;
  packed_t : Ctt_abstree.c_type;
  cdr_t : Ctt_abstree.c_type;
  unpacked_types : Ctt_abstree.c_type list;
  initializer_type : Ctt_abstree.c_type;
  generic_t : Ctt_abstree.c_type;
  samesized_word_t : Ctt_abstree.c_type;
  packed_to_generic : Il3.primitive_reducible option;
  generic_to_packed : Il3.primitive_reducible option;
  rw_type : Il3.rw_targets;
  generic_cdr_t : Ctt_abstree.c_type;
}

val type_base_t : Ctt_abstree.c_type
val type_ofs_t : Ctt_abstree.c_type
val type_vaddr_t : Ctt_abstree.c_type

val type_boundary_t : Ctt_abstree.c_type

val type_value : Ctt_abstree.c_type
val type_dvalue : Ctt_abstree.c_type
val type_ptrvalue : Ctt_abstree.c_type
val type_ptrvalue_ptr : Ctt_abstree.c_type

val type_void_ptr : Ctt_abstree.c_type
val type_fsc_error : Ctt_abstree.c_type
val type_stack_frame : Ctt_abstree.c_type

val type_byte : Ctt_abstree.c_type
val type_hword : Ctt_abstree.c_type
val type_word : Ctt_abstree.c_type
val type_dword : Ctt_abstree.c_type

val translate_field_name : string -> string

val parse_type_genv :
  genv:Ctt_abstree.environment -> Ctt_abstree.c_type -> type_properties

val translate_c_type_genv :
  genv:Ctt_abstree.environment -> Ctt_abstree.c_type -> Ctt_abstree.c_type

val is_pointer : Ctt_abstree.c_type -> bool

val encoded_name_of_btype : Ctt_abstree.builtin_type -> string
val encoded_name_of_struct : genv:Ctt_abstree.environment -> int -> string
val encoded_name_of_type_genv :
  genv:Ctt_abstree.environment ->
  ?array_squash:bool -> Ctt_abstree.c_type -> string

val get_global_string_storage_name : genv:Ctt_abstree.environment -> int -> string

val translate_global_function_name :
  genv:Ctt_abstree.environment -> string -> Ctt_abstree.c_type -> string

type decl_type = Known | Unknown_monomorphic | Unknown_polymorphic

val translate_global_variable_name :
  known:decl_type ->
  genv:Ctt_abstree.environment -> string -> Ctt_abstree.c_type -> string

val generate_bridge_function :
  genv:Ctt_abstree.environment ->
  loc:Locterm.location ->
  Ctt_abstree.global_storage_class ->
  string ->
  Ctt_abstree.c_type ->
  string list -> (Il3.temp_id, unit) Il3.il3_global_declaration
val generate_function_stub :
  genv:Ctt_abstree.environment ->
  loc:Locterm.location ->
  Ctt_abstree.global_storage_class ->
  string -> Ctt_abstree.c_type ->
  string list -> (Il3.temp_id, unit) Il3.il3_global_declaration

(*
val get_debug_flags : unit -> int
val dprintf : int -> ('a, Format.formatter, unit, unit) format4 -> 'a
val dprintf_start : ('a, Format.formatter, unit, unit) format4 -> 'a
val dprintf_progress : ('a, Format.formatter, unit, unit) format4 -> 'a
val dprintf_end : ('a, Format.formatter, unit, unit) format4 -> 'a
module VarMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val formatter : ('a -> string) -> Format.formatter -> 'a -> unit
val clear_castflag_at_memop : bool ref
type compiler_mode =
  Transutil.compiler_mode =
    MultiModule
  | MultiLinker
  | StandAlone
  | TrustInterModule
  | StdlibImplementation
val compiler_mode : Transutil.compiler_mode ref
val max_local_var_size : Big_int_infix.big_int ref
val use_signed_compare_for_offset : bool ref
val force_emit_stack_unwind : bool ref
val use_optimized_narrow_cast : bool ref
val use_boundary_t : bool ref
type decl_type = Known | Unknown_monomorphic | Unknown_polymorphic
type decl_environment = (Il0.identifier, decl_type) Hashtbl.t
type e_or_r =
    IEXP of Il3.temp_id Il3.il3_irreducible_exp
  | REXP of Il3.temp_id Il3.il3_reducible_exp
  | MOVE of Il3.temp_id
  | RF of Il3.primitive_reducible * Il3.temp_id list
val emit_e_or_r :
  Il3.temp_id -> Ctt_abstree.c_type -> e_or_r -> Il3.temp_id Il3.il3
type convfuncs = (Ctt_abstree.c_type * Il3.temp_id Il.il_lvalue option) list
type environment = {
  mutable max_var : int;
  f : Ilc.ilc_function;
  is_polymorphic : bool;
  global_env : Ctt_abstree.environment;
  decl_env : decl_environment;
  rettype : Ctt_abstree.c_type;
  arguments_b : int Util.earray;
  arguments_v : int Util.earray;
  arguments_n : int Util.earray;
  prologue0 : Il0.il0 Glist.t;
  prologue1 : Il3.temp_id Il3.il3 Glist.t;
  epilogue : Il3.temp_id Il3.il3 Glist.t;
  mutable exit_code : Il3.temp_id Il3.il3 list;
  struct_is_continuous : bool Util.earray;
  mutable boundary_cache : Il3.temp_id VarMap.t;
  name : Il0.identifier;
}
val constant_zero : Ctt_abstree.c_constants
val constant_one : Ctt_abstree.c_constants
val make_cttm_expr :
  Cttm_abstree.mexpr_desc -> Ctt_abstree.c_type -> Cttm_abstree.mexpr
val rettype_of_functype : Ctt_abstree.c_type -> Ctt_abstree.c_type
val new_tempid : env:environment -> int
val new_temp_label : int -> int -> string
val type_of_tempvar : env:environment -> int -> Ils.ilr_type
val sizeof_type_genv :
  genv:Ctt_abstree.environment -> Ctt_abstree.c_type -> Big_int.big_int
val sizeof_type : env:environment -> Ctt_abstree.c_type -> Big_int.big_int
val is_numeric : Ctt_abstree.c_type -> bool
val size_is_dword : Big_int_infix.big_int -> bool
val type_signed_ofs_t : Ctt_abstree.c_type
val type_ptrinit : Ctt_abstree.c_type
val type_valinit : Ctt_abstree.c_type
val type_dvalinit : Ctt_abstree.c_type
val type_typeinfo_init : Ctt_abstree.c_type
val type_typeinfo_struct : Ctt_abstree.c_type
val type_function_stub_init : Ctt_abstree.c_type
val type_function_stub : Ctt_abstree.c_type
val type_genfunc : Ctt_abstree.c_type
val type_frame_variables_entry : Ctt_abstree.c_type
val type_frame_variables : Ctt_abstree.c_type
val type_frame_variables_n : int -> Ctt_abstree.c_type
val id_local_frame : string
val id_local_frame_variables : string
val lv_local_frame_variables : 'a Il.il_lvalue
val type_fsc_header : Ctt_abstree.c_type
val make_cttm_func_lv :
  Ctt_abstree.identifier ->
  Ctt_abstree.c_type list -> Ctt_abstree.c_type -> Cttm_abstree.mem_object
val lv_cttm_macro_emit_header : Cttm_abstree.mem_object
val encoded_name_of_type : env:environment -> Ctt_abstree.c_type -> string
val encoded_typeinfo_name :
  genv:Ctt_abstree.environment -> Ctt_abstree.c_type -> string
val get_storage_type_array :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.c_type -> Big_int_infix.big_int option -> Ctt_abstree.c_type
val size_and_elemtype_of_onmemory_type :
  ?incomplete_ok:bool ->
  Ctt_abstree.c_type -> Big_int.big_int option * Ctt_abstree.c_type
val storage_for_typeinfo_constant :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.c_type ->
  Ctt_abstree.c_type * Ctt_abstree.c_type * 'a Il.il_lvalue *
  (string * Ctt_abstree.c_type) list
val make_compound :
  env:environment -> Ctt_abstree.c_type -> 'a -> 'a -> int * 'a Il3.il3 list

val parse_type : env:environment -> Ctt_abstree.c_type -> type_properties
val is_type_continous : env:environment -> Ctt_abstree.c_type -> bool
val rf_get_realoffset :
  env:environment ->
  Ctt_abstree.c_type -> Ctt_abstree.c_type * Il3.primitive_reducible
val get_destructors :
  env:environment ->
  Ctt_abstree.c_type ->
  Ctt_abstree.c_type * Il3.pair_types * Ctt_abstree.c_type
val get_pair_type :
  env:environment -> Ctt_abstree.c_type -> Ctt_abstree.c_type
val get_base_type : env:'a -> 'b -> Ctt_abstree.c_type
val get_ofs_or_value_type :
  env:'a -> Ctt_abstree.c_type -> Ctt_abstree.c_type
val get_generic_readers :
  env:environment ->
  Ctt_abstree.c_type ->
  Il3.rw_targets * Ctt_abstree.c_type *
  (Ctt_abstree.c_type * Il3.pair_types) option
val get_generic_writers :
  env:environment ->
  Ctt_abstree.c_type ->
  Il3.rw_targets * Ctt_abstree.c_type *
  (Ctt_abstree.c_type * Il3.primitive_reducible) option
val check_size_of_local_variable :
  genv:Ctt_abstree.environment -> string -> Ctt_abstree.c_type -> unit
val make_coerce_from_vaddr : Ctt_abstree.c_type -> Il3.temp_id -> e_or_r
val make_coerce_to_vaddr :
  Ctt_abstree.c_type -> Ctt_abstree.c_type -> Il3.temp_id -> e_or_r
val make_coerce_to_value2 : Ctt_abstree.c_type -> Il3.primitive_reducible
val make_specific_read1 :
  env:environment ->
  Il3.temp_id ->
  Ctt_abstree.c_type -> int -> Il.field list -> int Il3.il3 list
val make_specific_write1 :
  env:'a -> 'b -> 'c -> 'b -> Il.field list -> 'b Il3.il3 list
val make_specific_read2 :
  env:environment ->
  Il3.temp_id * Il3.temp_id ->
  Ctt_abstree.c_type -> int -> Il.field list -> int Il3.il3 list
val make_specific_write2 :
  env:environment ->
  int * int -> Ctt_abstree.c_type -> int -> Il.field list -> int Il3.il3 list
val make_generic_read1 :
  env:environment ->
  Il3.temp_id ->
  Ctt_abstree.c_type ->
  'a -> Il3.temp_id -> Il3.temp_id -> Il3.temp_id Il3.il3 list
val make_generic_read2 :
  env:environment ->
  Il3.temp_id * Il3.temp_id ->
  Ctt_abstree.c_type ->
  'a -> Il3.temp_id -> Il3.temp_id -> Il3.temp_id Il3.il3 list
val make_generic_write1 :
  env:environment ->
  Il3.temp_id ->
  Ctt_abstree.c_type ->
  Ctt_abstree.c_type option ->
  Il3.temp_id -> Il3.temp_id -> Il3.temp_id Il3.il3 list
val make_generic_write2 :
  env:environment ->
  int * int ->
  Ctt_abstree.c_type ->
  Ctt_abstree.c_type option -> int -> int -> int Il3.il3 list
val rettype : env:environment -> Ctt_abstree.c_type
val translate_type : env:environment -> Ils.ilr_type -> Ctt_abstree.c_type
val translate_ilstype : env:'a -> Ils.ils_type -> Ctt_abstree.c_type
val original_type_from_val_type : Ils.ilr_type -> Ctt_abstree.c_type
val is_type_polymorphic_func : genv:'a -> Ctt_abstree.c_type -> bool
val translate_c_type_funcarg :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.c_type -> Ctt_abstree.c_type list -> Ctt_abstree.c_type list
val translate_c_type :
  env:environment -> Ctt_abstree.c_type -> Ctt_abstree.c_type
val foldr_with_argtypes :
  env:environment ->
  Ctt_abstree.c_type ->
  varargsf:('a list -> 'b -> 'b) ->
  widef:(Ctt_abstree.c_type -> 'a -> 'b -> 'b) ->
  narrowf:(Ctt_abstree.c_type -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val flatten_mapi_with_argtypes :
  env:environment ->
  Ctt_abstree.c_type ->
  varargsf:('a list -> 'b list) ->
  widef:(int -> Ctt_abstree.c_type -> 'a -> 'b list) ->
  narrowf:(int -> Ctt_abstree.c_type -> 'a -> 'b list) -> 'a list -> 'b list
val translate_func_concrete_args :
  env:environment ->
  Ctt_abstree.c_type -> Ils.ils_funcarg list -> 'a list * Ils.temp_id list
val translate_func_varargs :
  env:environment ->
  Ils.ils_funcarg list ->
  Ils.temp_id Il3.il3 list * int list * int Il3.il3 list
val translate_funcargs :
  env:environment ->
  tptr_id:Ils.temp_id ->
  Ctt_abstree.c_type ->
  Ils.ils_funcarg list ->
  Ils.temp_id Il3.il3 list * Ils.temp_id list * int Il3.il3 list
val type_of_union_initializer :
  env:environment -> Ctt_abstree.c_type -> Ctt_abstree.c_type
val get_storage_type :
  genv:Ctt_abstree.environment ->
  ?incomplete_ok:bool -> Ctt_abstree.c_type -> Ctt_abstree.c_type
val translate_global_bridge_function_name : genv:'a -> string -> 'b -> string
val translate_ptr_local_heap_variable_name :
  genv:Ctt_abstree.environment -> string -> Ctt_abstree.c_type -> string
val translate_local_stack_variable_name :
  genv:Ctt_abstree.environment -> string -> Ctt_abstree.c_type -> string
val translate_fields :
  genv:Ctt_abstree.environment ->
  (string * Ctt_abstree.c_type) list -> (string * Ctt_abstree.c_type) list
val get_type_of_field : 'a -> ('b * 'a) list -> 'a
val storage_for_variable_internal :
  genv:Ctt_abstree.environment ->
  (genv:Ctt_abstree.environment -> 'a -> Ctt_abstree.c_type -> 'b) ->
  global:bool ->
  'a ->
  Ctt_abstree.c_type ->
  (string * Ctt_abstree.c_type) list ->
  Ctt_abstree.c_type * Ctt_abstree.c_type * 'b *
  (string * Ctt_abstree.c_type) list
val storage_for_global_string_internal :
  genv:Ctt_abstree.environment ->
  string ->
  Ctt_abstree.c_type * Ctt_abstree.c_type * string *
  (string * Ctt_abstree.c_type) list
val cttm_storage_for_global_string :
  genv:Ctt_abstree.environment ->
  string ->
  Ctt_abstree.c_type * Ctt_abstree.c_type * Cttm_abstree.mem_object *
  (string * Ctt_abstree.c_type) list
val storage_for_global_string :
  genv:Ctt_abstree.environment ->
  string ->
  Ctt_abstree.c_type * Ctt_abstree.c_type * 'a Il.il_lvalue *
  (string * Ctt_abstree.c_type) list
val get_field_offset :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.c_type ->
  (Ctt_abstree.identifier * Ctt_abstree.c_type) list ->
  Ctt_abstree.struct_ofsinfo
val cttm_storage_for_global_variable :
  genv:Ctt_abstree.environment ->
  known:decl_type ->
  string ->
  Ctt_abstree.c_type ->
  (string * Ctt_abstree.c_type) list ->
  Ctt_abstree.c_type * Ctt_abstree.c_type * Cttm_abstree.mem_object *
  (string * Ctt_abstree.c_type) list
val storage_for_variable :
  env:environment ->
  Ils.ils_vartype ->
  Il0.identifier ->
  Ctt_abstree.c_type ->
  (string * Ctt_abstree.c_type) list ->
  'a Il3.il3 list * Ctt_abstree.c_type * Ctt_abstree.c_type *
  int Il.il_lvalue * (string * Ctt_abstree.c_type) list
val cttm_macro_for_blockheader :
  genv:Ctt_abstree.environment ->
  Ctt_abstree.c_type -> Big_int_infix.big_int option -> Cttm_abstree.mexpr
val exp_of_error_constant : Ils.ils_error -> 'a Il3.il3_reducible_exp
val translate_rettype_hint :
  env:environment -> Ils.ilr_base_usetype_info -> int * int Il3.il3 list
val resolve_reference_type :
  Ils.ilr_type -> Ctt_abstree.c_type * Ctt_abstree.c_type
val translate_expr1 :
  bool ->
  env:environment ->
  Ils.ilr_type ->
  Ilc.ilc_expr -> Ils.temp_id Il3.il3 list * e_or_r * int Il3.il3 list
val translate_expr2 :
  env:environment ->
  Ils.ilr_type * Ils.ilr_type ->
  Ilc.ilc_expr ->
  Ils.temp_id Il3.il3 list * e_or_r * e_or_r * int Il3.il3 list
val make_ptr_operations :
  env:environment ->
  idb:VarMap.key ->
  ido:Il3.temp_id ->
  genf:(env:environment ->
        'a ->
        Ctt_abstree.c_type ->
        Ctt_abstree.c_type option ->
        VarMap.key -> Il3.temp_id -> VarMap.key Il3.il3 list) ->
  specf:(env:environment ->
         'a ->
         Ctt_abstree.c_type ->
         int ->
         (Ctt_abstree.identifier * Ctt_abstree.c_type) list ->
         VarMap.key Il3.il3 list) ->
  tyd:Ctt_abstree.c_type ->
  fl:(Ctt_abstree.identifier * Ctt_abstree.c_type) list ->
  'a -> Il3.temp_id list -> VarMap.key Il3.il3
val translate_global_initializer_base :
  genv:Ctt_abstree.environment ->
  denv:(Ils.identifier, decl_type) Hashtbl.t ->
  Ils.ils_initializer_base -> Cttm_abstree.mexpr * Ctt_abstree.c_type option
val constant_of_ilsinitofs :
  Ils.ils_initializer_offset -> Ctt_abstree.c_constants
val is_pointer_cast :
  region_target_type:Ctt_abstree.c_type ->
  value_target_type:Ctt_abstree.c_type ->
  unknown_p:bool -> offset:Big_int_infix.big_int -> bool
val select_macro_emit :
  genv:Ctt_abstree.environment ->
  is_global:bool ->
  Ctt_abstree.c_type ->
  Ctt_abstree.c_type option * Ctt_abstree.c_type *
  Cttm_abstree.mem_object option * Ctt_abstree.c_type
val translate_global_initializer_value :
  genv:Ctt_abstree.environment ->
  denv:(Ils.identifier, decl_type) Hashtbl.t ->
  is_global:bool ->
  Ctt_abstree.c_type ->
  Ils.ils_initializer_base ->
  Ils.ils_initializer_offset -> Cttm_abstree.mexpr
val translate_local_initializer :
  env:environment ->
  safe:bool ->
  Ils.ils_initializer ->
  ((Il0.initfield * Ctt_abstree.c_type) list *
   (denv:(Ils.identifier, decl_type) Hashtbl.t -> Cttm_abstree.mexpr))
  list
val is_empty_initialization : Ils.ils_initializer -> bool
val translate_local_const_initialization_value :
  env:environment ->
  Ctt_abstree.c_type ->
  int ->
  Il.field list ->
  Ils.ils_initializer_base -> Ils.ils_initializer_offset -> int Il3.il3 list
val translate_local_const_initialization :
  env:environment ->
  Ctt_abstree.c_type ->
  Ils.ils_initializer -> int -> Il.field list -> int Il3.il3 list
val translate_stmt : env:environment -> Ilc.ilc -> Ils.temp_id Il3.il3
type prescan_env = {
  heapvars : (Ctt_abstree.c_type * Il0.identifier) Glist.t;
  stackvars : (Ctt_abstree.c_type * Il0.identifier) Glist.t;
}
val prescan_stmt : penv:prescan_env -> Ilc.ilc -> unit
val prescan :
  make_frame:bool ->
  env:environment -> Ilc.ilc Il.il_basic_block_base array -> unit
val translate_basic_block :
  env:environment ->
  ?boundmap:Il3.temp_id VarMap.t ->
  Ilc.ilc Il.il_basic_block_base ->
  Ils.temp_id Il3.il3 Il.il_basic_block_base * Il3.temp_id VarMap.t
val translate_basic_blocks :
  env:environment ->
  'a ->
  Ilc.ilc Il.il_basic_block_base array ->
  Ils.temp_id Il3.il3 Il.il_basic_block_base array
val translate_function_body :
  genv:Ctt_abstree.environment ->
  denv:decl_environment ->
  name:Il0.identifier ->
  Ctt_abstree.c_type ->
  string list ->
  (Ilc.ilc, Ils.ilr_type) Il.il_function_base ->
  Ctt_abstree.c_type * string list * (Il3.temp_id, unit) Il3.il3_function
val translate_function :
  genv:Ctt_abstree.environment ->
  denv:decl_environment ->
  Ctt_abstree.global_storage_class ->
  Ctt_abstree.c_type ->
  Il0.identifier ->
  string list ->
  (Ilc.ilc, Ils.ilr_type) Il.il_function_base ->
  (Il3.temp_id, unit) Il3.il3_global_declaration
val translate_global_initializer_list :
  genv:Ctt_abstree.environment ->
  denv:(Ils.identifier, decl_type) Hashtbl.t ->
  Ils.ils_initializer -> Cttm_abstree.cttm_initializer
val translate_global_initializer :
  genv:Ctt_abstree.environment ->
  denv:(Ils.identifier, decl_type) Hashtbl.t ->
  Ctt_abstree.c_type ->
  Ils.ils_initializer option -> Cttm_abstree.cttm_initializer
val enclose_instr_to_function :
  int -> 'a Il3.il3 list -> ('a, unit) Il3.il3_function
val translate_funcdecl_to_prototype :
  ('a, 'b) Il3.il3_global_declaration -> ('c, 'd) Il3.il3_global_declaration
val create_denv :
  ('a, 'b, 'c) Il.il_global_declaration_base list ->
  (Il.identifier, decl_type) Hashtbl.t
val f :
  genv:Ctt_abstree.environment ->
  (Ilc.ilc, Ils.ilr_type, Ils.ils_initializer) Il.il_global_declaration_base
  list -> (Il3.temp_id, unit) Il3.il3_global_declaration list
*)
