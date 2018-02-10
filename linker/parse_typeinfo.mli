(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Parses module type information embedded in compiled module objects *)

val system_path_ref : string ref
val system_path : unit -> string
(** Returns the path of Fail-Safe C compiler *)

(** Forms of struct declaration in the module. *)
type struct_declaration_info =
    Sabstract
  | Sspecial
  | Sconcrete of (string * Linker_types.ltype) list

(** Attributes attached with declared values *)
type value_attributes = 
    {
     attr_required_native_libraries : string list;
     (** native libraries required to be linked. *)
   }

(** Attributes attached with modules *)
type module_attributes = {
    is_stdlib : bool;
    abi_revision : int;
    allowed_minimum_abi_revision : int;
    need_initialize : string option;
    forced_linking : bool;
  }

type module_info = {
  mi_structs :
    (Linker_types.struct_desc * string * struct_declaration_info) list;
    (** declared structs *)
  mi_required_typeinfo : Linker_types.ltype list;
    (** typeinfo blocks required in the module code *)
  mi_required_values : (string * Linker_types.ltype) list;
    (** values required by the module code *)
  mi_bss_values : (string * Linker_types.ltype) list;
    (** values provided as BSS data by the module code *)
  mi_provided_values : (string * Linker_types.ltype * value_attributes) list;
    (** values provided by the module code *)
  mi_attributes : module_attributes;
    (** misc attributes of modules *)
}
(** the parsed information of the module object *)

val read_string : file:string -> string -> module_info
(** reads the module information from a string. *)
val parse_compile_unit : string -> module_info
(** reads the module information from a compiled module. *)
val parse_unit_manifest : string -> module_info
(** reads the module information from a manifest text (used for standard library). *)
