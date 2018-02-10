(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Pretty printers for untyped C tree. *)
val pp_print_list :
  Format.formatter ->
  elem_pp:(Format.formatter -> 'a -> unit) ->
  sep_pp:(Format.formatter -> unit) -> 'a list -> unit
(*
val pp_print_option : f:('a -> 'b -> unit) -> 'a -> 'b option -> unit
val pp_print_option_with_sep :
  f:('a -> 'b -> unit) -> sep:('a -> unit) -> 'a -> 'b option -> unit
val pp_print_sep_space : Format.formatter -> unit
val pp_print_sep_newline : Format.formatter -> unit
*)
val pp_print_sep_comma : Format.formatter -> unit
val pp_print_sep_none : Format.formatter -> unit
val string_of_extension_tree_hook :
  (string -> C_abstree.extension_tree -> string) ref
(** The customizable function used to print extension trees. *)
val string_of_extension_tree : string -> C_abstree.extension_tree -> string

val pp_print_extension_tree :
  Format.formatter -> string * C_abstree.extension_tree -> unit
val pp_print_extension_list_with_postspc :
  Format.formatter -> (string * C_abstree.extension_tree) list -> unit
val pp_print_identifier : Format.formatter -> string -> unit
val pp_print_binop : Format.formatter -> C_abstree.binop -> unit
val pp_print_unaryop : Format.formatter -> C_abstree.unaryop -> unit
val pp_print_constant : Format.formatter -> C_abstree.c_constants -> unit
val string_of_storage_class : C_abstree.storage_class -> string
val pp_print_storageclass :
  Format.formatter -> C_abstree.storage_class -> unit
val pp_print_typequal : Format.formatter -> C_abstree.type_qualifier -> unit
val pp_print_struct_or_union :
  Format.formatter -> C_abstree.union_flag -> unit
val pp_print_builtin_typespec :
  Format.formatter -> C_abstree.builtin_type_specifier -> unit
val pp_print_typespec : Format.formatter -> C_abstree.type_specifier -> unit
val pp_print_struct_declarator :
  Format.formatter -> C_abstree.struct_declarator -> unit
val pp_print_struct_declaration :
  Format.formatter -> C_abstree.struct_declaration -> unit
val pp_print_struct_declaration_list :
  Format.formatter -> C_abstree.struct_declaration list -> unit
val pp_print_sq : Format.formatter -> C_abstree.specifier_qualifier -> unit
val pp_print_sqlist :
  Format.formatter -> C_abstree.specifier_qualifier list -> unit
val pp_print_enum_entry : Format.formatter -> C_abstree.enumerator -> unit
val pp_print_enum_list :
  Format.formatter -> C_abstree.enumerator list -> unit
val pp_print_declarator :
  Format.formatter -> C_abstree.abstract_declarator -> unit
val pp_print_typename : Format.formatter -> C_abstree.type_name -> unit
val pp_print_paramdecl : Format.formatter -> C_abstree.parameter_type -> unit
val pp_print_expression_iter :
  int -> Format.formatter -> C_abstree.expr -> unit
val pp_print_argument_list : Format.formatter -> C_abstree.expr list -> unit
val pp_print_expression : Format.formatter -> C_abstree.expr -> unit
val pp_print_constant_expression : Format.formatter -> C_abstree.expr -> unit
val pp_print_initializer :
  Format.formatter -> C_abstree.c_initializer -> unit
val pp_print_initdeclarator :
  Format.formatter -> C_abstree.init_declarator -> unit
val pp_print_statement : Format.formatter -> C_abstree.statement -> unit
val pp_print_declaration : Format.formatter -> C_abstree.declaration -> unit
val pp_print_program : Format.formatter -> C_abstree.program -> unit
