(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Shared utility definitions for translators *)

(**{6 Main function} *)

(* Mode Switches *)
val clear_castflag_at_memop : bool ref

type compiler_mode =
    MultiModule (** multi-module compilation *)
  | MultiLinker (** linker *)
  | StandAlone  (** stand-alone compilation: single module only, not using FSC linker *)
  | TrustInterModule (** unsafe, linkerless mode of multi-module compilation *)
  | StdlibImplementation (** special mode for compiling standard library *)
val compiler_mode : compiler_mode ref
(** Mode of the compiler. Affects generated codes. *)

val max_local_var_size : Big_int_infix.big_int ref (** The limit of local variable size. *)
val use_signed_compare_for_offset : bool ref (** Use signed comparison for pointer offsets. *)
val force_emit_stack_unwind : bool ref (** Generate stack management code in all functions. *)
val use_optimized_narrow_cast : bool ref (** Use optimized semantics for pointer cast with narrow builtin types. *)
val use_boundary_t : bool ref (** Use boundary information cache inside functions, to reduce on-memory boundary checks. *)

val builtintype_capable_of_fatvalue : Ctt_abstree.builtin_type -> bool
