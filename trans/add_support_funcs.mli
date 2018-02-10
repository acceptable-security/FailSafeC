(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Generates additional support functions and declaration for FSC-generated code *)

val abi_revision : int (** the revision number of the abi of current compiler. *)
val allowed_minimum_object_abi_revision : int (** the lowest revision number of object which the current compiler accepts. *)
val required_minimum_compiler_abi_revision : int (** the lowest revision number which the currently-generating object requires. *)

val specially_emit_structs : Ctt_abstree.struct_id list ref
val use_gcc_extension : bool (** use GCC/C99 hexadecimal floats in output *)
val use_alignpad : bool ref  (** use special alignment optimization for structs *)
val indexed_ofsmap_expand_limit : int ref (** The limit of inline expansion for struct-element accesss *)

val f_standalone :
  genv:Ctt_abstree.environment ->
  C_abstree.declaration_desc Locterm.t list ->
  C_abstree.declaration_desc Locterm.t list
      (** main function for standalone modules. Used for stand-alone compilation and also for linkers. *)

val f_multifile :
  genv:Ctt_abstree.environment ->
  ?mod_exts:string list ->
  'a Il0.il0_global_declaration list ->
  C_abstree.declaration_desc Locterm.t list ->
  C_abstree.declaration_desc Locterm.t list
      (** main function for multi-module compilation.  It generates only stubs and inline functions,
       and emits embedded information for the linker. *)

val f :
  genv:Ctt_abstree.environment ->
  orig_t:'a Il0.il0_global_declaration list ->
  ?mod_exts:string list ->
  C_abstree.declaration_desc Locterm.t list ->
  C_abstree.declaration_desc Locterm.t list
      (** main function. It uses either of above depending on the compiler mode in Translate module. *)
