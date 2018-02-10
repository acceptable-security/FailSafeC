(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2008. *)

(** Typing C syntax tree. *)

val parse_global_declarations :
  C_abstree.declaration list ->
  Ctt_abstree.environment * Ctt_abstree.global_declaration list
      (** The main typing function of the module. *)

(**{6 customization hooks}*)

val translate_struct_by_extension_hook : 
    (loc:Locterm.location -> ext:C_abstree.extension_spec list ->
      Ctt_abstree.struct_desc -> Ctt_abstree.struct_desc) ref
(** A hook for extension; given a struct declaration, converts it regading to the extensions given. *)

val translate_external_extension_hook : 
    (loc: Locterm.location ->
      ext:C_abstree.extension_spec list ->
	sclass:Ctt_abstree.global_storage_class ->
	  ty:Ctt_abstree.c_type ->
	    is_initialized:bool ->
	      C_abstree.extension_spec list) ref
(** A hook for extension; given a struct declaration, converts it regading to the extensions given. *)

val merge_value_extensions_hook :
    (old_ext:C_abstree.extension_spec list -> new_ext:C_abstree.extension_spec list
      -> have_definition:bool -> C_abstree.extension_spec list) ref
val merge_struct_extensions_hook :
    (old_ext:C_abstree.extension_spec list -> new_ext:C_abstree.extension_spec list
      -> C_abstree.extension_spec list) ref
(** Hooks for extension; check whether extended extern declaration allows
   later redeclaration without extensions. *)

(**{6 customization flags}*)

val fill_zero_initializer : bool ref
    (** whether to fill the initializations with insufficient elements with zeros. Module default: true, Fail-Safe C use: false. *)
val alpha_convert_use_prefixed_name : bool ref
    (** whether to prefix the identifiers with numbers for alpha-conversion (it generates invalid name when used directly). Module default: false, Fail-Safe C use: true. *)
val allow_calling_undefined_functions : bool ref
    (** whether to allow call undefined functions. Use with care. Fail-Safe C use: true. *)
val enable_gnu_autoconf_workaround : bool ref
    (** whether to enable workaround for GNU autoconf. Use with care. Fail-Safe C use: true. *)

(**{6 Other functions} *)

val remove_qualifier : Ctt_abstree.c_type -> Ctt_abstree.c_type

val fold_constants : Ctt_abstree.expr -> Ctt_abstree.expr
    (** reduces constants. If not a constant, it acts as identity. *)

val qual_eq : Ctt_abstree.c_type -> Ctt_abstree.c_type -> bool
val qual_gt : Ctt_abstree.c_type -> Ctt_abstree.c_type -> bool
val qual_lt : Ctt_abstree.c_type -> Ctt_abstree.c_type -> bool
val qual_dcare : Ctt_abstree.c_type -> Ctt_abstree.c_type -> bool

val equal_type :
  ?check_qual:(Ctt_abstree.c_type -> Ctt_abstree.c_type -> bool) ->
  ?check_iqual:(Ctt_abstree.c_type -> Ctt_abstree.c_type -> bool) ->
  ?ignore_signeness:bool -> ?pointers_ignore_signedness:bool ->
  Ctt_abstree.c_type -> Ctt_abstree.c_type -> bool
      (** compare two types. Qualifiers are compared regarding to optional arguments (one for direct qualifiers and the other for indirect ones (inside the type). *)

val type_of : Ctt_abstree.expr -> Ctt_abstree.c_type

val update_fields_cache : Ctt_abstree.struct_desc -> Ctt_abstree.struct_desc

val is_type_loose_function : Ctt_abstree.c_type -> bool
    (** function determing loose checking of function type matching. Related with [enable_gnu_autoconf_workaround]. *)
