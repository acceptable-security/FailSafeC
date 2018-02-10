(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Checks inter-module type consistency and generates the type unifier *)

type t = {
  new_module_hash : string; (** a new module hash for linked module *)
  rename_list :
    (Linker_types.struct_desc, Linker_types.struct_desc) Hashtbl.t;
    (** list of renamed (unified) struct declarations *)
  renamed_typeinfo : (Linker_types.ltype, Linker_types.ltype) Hashtbl.t;
    (** list of typeinfo blocks which requires rename *)
  required_typeinfo : (Linker_types.ltype, unit) Hashtbl.t;
    (** list of typeinfo blocks which requires generation *)
  provided_renamed_identifiers :
    (string * Linker_types.ltype * Linker_types.ltype) list;
    (** list of identifiers provided and rename required *)
  requested_renamed_identifiers :
    (string * Linker_types.ltype * Linker_types.ltype) list;
    (** list of identifiers referenced and rename required *)
  struct_table :
    (int *
     (string option *
      (string * string * Parse_typeinfo.struct_declaration_info)))
    list;
    (** list of structs in linked modules *)
  provided_values : (string, string * Linker_types.ltype) Hashtbl.t;
    (** list of provided identifiers *)
  instanciating_bss_values : (string, string * Linker_types.ltype) Hashtbl.t;
    (** list of BSS identifiers to be instanciated *)
  renamed_unknown_functions : (string * Linker_types.ltype) list;
    (** list of identifiers for unknown functions resolved to specific implementations *)
  required_native_libraries : string list;
    (** list of native libraries required to be linked. *)
  linked_modules : (string, Parse_typeinfo.module_info) Hashtbl.t;
}
(** the type representing the result of linking. *)

val f : (string * Parse_typeinfo.module_info) list -> t
(** main function. *)
