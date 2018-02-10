(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Generators for visitors for typed abstruct syntax tree. *)

(** general usage rule: These functions are implemented as open recursive functions
   which calls itself using the [~self] argument.
   So, make a function of the same type as the generator (except labelled arguments).
   If the argument is not an interested one, call the corresponding visitor generator
   and pass itself as [~self] for recursions. Some of the functions requires visitors for
   sub-elements, which is labelled as [~v_...].
 *)

val visit_type :
  self:(Ctt_abstree.c_type -> Ctt_abstree.c_type) ->
  Ctt_abstree.c_type -> Ctt_abstree.c_type
(** recursive visitor for C types. *)

val visit_expr :
  v_type:(Ctt_abstree.c_type -> Ctt_abstree.c_type) ->
  self:(Ctt_abstree.expr -> Ctt_abstree.expr) ->
  Ctt_abstree.texpr Locterm.t -> Ctt_abstree.texpr Locterm.t
(** recursive visitor for C expressions. *)

val visit_statement :
  v_type:(Ctt_abstree.c_type -> Ctt_abstree.c_type) ->
  v_expr:(Ctt_abstree.expr -> Ctt_abstree.expr) ->
  v_ldecl:(Ctt_abstree.variable_declaration ->
           Ctt_abstree.variable_declaration) ->
  self:(Ctt_abstree.statement -> Ctt_abstree.statement) ->
  Ctt_abstree.statement ->
  Ctt_abstree.statement
(** recursive visitor for C statements. *)

val visit_initializer :
  v_expr:(Ctt_abstree.expr -> Ctt_abstree.expr) ->
  v_strinit:(loc:Locterm.location ->
             Ctt_abstree.struct_id ->
             Ctt_abstree.ctt_initializer list ->
             Ctt_abstree.ctt_initializer list) ->
  self:(Ctt_abstree.c_type ->
        Ctt_abstree.ctt_initializer -> Ctt_abstree.ctt_initializer) ->
  Ctt_abstree.c_type ->
  Ctt_abstree.ctt_initializer -> Ctt_abstree.ctt_initializer
(** recursive visitor for C initializers. *)

val visit_struct_initializer :
  genv:Ctt_abstree.environment ->
  loc:Locterm.location ->
  v_init:(Ctt_abstree.c_type ->
    Ctt_abstree.ctt_initializer -> Ctt_abstree.ctt_initializer) ->
   Ctt_abstree.struct_id -> Ctt_abstree.ctt_initializer list ->
        Ctt_abstree.ctt_initializer list
(** recursive visitor for C initializers inside structs. *)

val visit_ldecl :
    v_init:(Ctt_abstree.c_type ->
            Ctt_abstree.ctt_initializer -> Ctt_abstree.ctt_initializer) ->
    v_type:(Ctt_abstree.c_type -> Ctt_abstree.c_type) -> 
    Ctt_abstree.variable_declaration -> Ctt_abstree.variable_declaration
(** recursive visitor for C local declarations. *)

val visit_gdecl :
  v_stmt:(Ctt_abstree.statement -> Ctt_abstree.statement) ->
  v_init:(Ctt_abstree.c_type ->
          Ctt_abstree.ctt_initializer -> Ctt_abstree.ctt_initializer) ->
  v_type:(Ctt_abstree.c_type -> Ctt_abstree.c_type) ->
  Ctt_abstree.global_declaration -> Ctt_abstree.global_declaration
(** recursive visitor for C global declarations. *)
