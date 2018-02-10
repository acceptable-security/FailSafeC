(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** IL3: intermediate language for generated Fail-Safe C code *)

(**{6 Definition of IL3} *)

(** IL3, encoded as [temp_id il3], is an intermediate language in which
    a single operation is corresponding to a single instruction .*)

type temp_id = int
type branch_target = int

type pair_types = Value | Dvalue | PtrValue
type rw_targets = Byte | Hword | Word | Dword | RWstruct of int | RWnone

(** list of internal conversion functions. *)
type internal_converters =
    Value_of_base_ofs
  | Value_of_ptrvalue
  | Ptrvalue_of_value of Ctt_abstree.c_type
  | Value_of_int
  | Dvalue_of_dword
  | Value_of_dvalue
  | Dvalue_of_value
  | Dvalue_of_double
  | Dword_of_double
  | Double_of_dvalue
  | Double_of_dword
  | Value_of_float
  | Word_of_float
  | Float_of_value
  | Float_of_word
  | Ofs_of_value
  | Vaddr_of_base_ofs
  | Ofs_of_base_vaddr

(** list of internal functions which has no side effects. *)
type internal_reducible_functions =
    Base_remove_castflag
  | Base_put_castflag
  | Set_base_castflag of Ctt_abstree.c_type
  | Add_fat_pointer of Ctt_abstree.c_type
  | Is_cast
  | Get_realoffset_pointer
  | Get_realoffset_funcptr
  | Get_realoffset of Ctt_abstree.c_type

(** list of internal functions which has side effects. *)
type internal_immobile_functions =
    Alloc_heapvar
  | Alloc_varargs
  | Alloc_valtempoline
  | Is_offset_ok
  | Get_boundary
  | Is_boundary_offset_ok
  | Invoke_generic_func

(** list of internal functions which has side effects. *)
type internal_immobile_operations =
    Finish_varargs
  | Dealloc_heapvar
  | Dealloc_varargs_finished
  | Put_varargs
  | Put_varargs_2
  | Enter_stack_unwind_area
  | Exit_stack_unwind_area

(** type for internal functions without side effects. *)
type primitive_reducible =
    IL3pr_cons of pair_types
  | IL3pr_car of pair_types
  | IL3pr_cdr of pair_types
  | IL3pr_convert of internal_converters
  | IL3pr_misc of internal_reducible_functions

(** type for expressions without side effects. *)
type 'a il3_reducible_exp =
    IL3RexCoerce of Ctt_abstree.c_type * 'a
  | IL3RexConstant of Ctt_abstree.c_constants
  | IL3RexUndefined
  | IL3RexBinop of Il.il_binop * 'a * 'a
  | IL3RexArgument of int
  | IL3RexAddress of 'a Il.il_lvalue * Il.field list
  | IL3RexUnaryop of Ctt_abstree.unaryop * 'a

(** type for expressions with side effects. 
    Binary arithmetic operators can be represented both as reducible or as irreducible. *)
type 'a il3_irreducible_exp =
    IL3IexInvoke of 'a Il.il_lvalue * 'a list
  | IL3IexBinop of Il.il_binop * 'a * 'a

(** statements of IL3. *)
type 'a il3_desc =
    IL3stmtSequence of 'a il3 list
  | IL3stmtParallel of 'a il3 list
  | IL3stmtCallReducibleFun of temp_id * primitive_reducible * 'a list
  | IL3stmtCallImmobileFun of temp_id * internal_immobile_functions * 'a list
  | IL3stmtCallImmobileOp of internal_immobile_operations * 'a list
  | IL3stmtCallAbort of 'a * 'a
  | IL3stmtCallReaderHelper of temp_id * rw_targets * 'a * 'a
  | IL3stmtCallWriterHelper of rw_targets * 'a * 'a * 'a * 'a option
  | IL3stmtCallReducibleFunOverwriting of temp_id * primitive_reducible *
      'a list
  | IL3stmtMove of temp_id * Ctt_abstree.c_type * 'a
  | IL3stmtRexp of temp_id * Ctt_abstree.c_type * 'a il3_reducible_exp
  | IL3stmtIexp of temp_id * Ctt_abstree.c_type * 'a il3_irreducible_exp
  | IL3stmtReadRaw of temp_id * Ctt_abstree.c_type * 'a Il.il_lvalue *
      Il.field list
  | IL3stmtWriteRaw of 'a Il.il_lvalue * Il.field list * 'a
  | IL3stmtGoto of branch_target
  | IL3stmtIf of Il.il_if_type * 'a * branch_target
  | IL3stmtSwitch of 'a * (Il.il_switch_label * branch_target) list
  | IL3stmtReturn of 'a option
  | IL3stmtConditional of temp_id list * 'a * 'a il3 * 'a il3
and 'a il3_t = { il3_desc : 'a il3_desc; }
and 'a il3 = 'a il3_t Locterm.t

val make_il3 : loc:Locterm.location -> 'a il3_desc -> 'a il3

(** type for a function definition in IL3. *)
type ('a, 'b) il3_function = {
  body : 'a il3 Il.il_basic_block_base array;
  max_variable_number : int;
  variable_environment :
    Ctt_abstree.c_type Il.il_variable_attribute_base Util.earray;
  prologue : Il0.il0 list;
  more_info : 'b;
}

(** type for program elements in IL3. *)
type ('a, 'b) il3_global_declaration_desc =
    IL3declFunction of Ctt_abstree.global_storage_class *
      Ctt_abstree.c_type * Ctt_abstree.identifier *
      Ctt_abstree.identifier list * ('a, 'b) il3_function
  | IL3declVariable of Ctt_abstree.global_storage_class *
      Ctt_abstree.c_type * Ctt_abstree.identifier *
      Cttm_abstree.cttm_initializer option

type ('s_tempid, 'add_info) il3_global_declaration = 
    ('s_tempid, 'add_info) il3_global_declaration_desc Locterm.t

(**{6 Definition of IL3B} *)

(** IL3B, encoded as [il3b_rexp il3], is a more complex
    intermediate language than IL3.
    In this Il, pure computations can be represented as
    a tree, just like that will be in usual programs. *)
type il3b_rexp =
    IL3BTemp of temp_id
   (** variable *)
  | IL3BRfun of (primitive_reducible * il3b_rexp list) Locterm.t
   (** internal pure functions *)
  | IL3BRexp of (Ctt_abstree.c_type * il3b_rexp il3_reducible_exp) Locterm.t
   (** pure expressions *)

(**{6 Utility function for IL3} *)

val enclose_il3_sequence : loc:Locterm.location -> 'a il3 list -> 'a il3
(** Compose a list of sequencial instructions into one instruction. *)
val enclose_il3_parallel : loc:Locterm.location -> 'a il3 list -> 'a il3
(** Compose a set of semantically concurrent instructions into one instruction. *)

val add_code_to_top :
   function_loc:Locterm.location ->
   'a il3 list ->
   'a il3 Il.il_basic_block_base array ->
   'a il3 Il.il_basic_block_base array
(** Add a list of code into the top of the code.
    If needed, an additional block is inserted on top. *)

