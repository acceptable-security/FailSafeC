(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

open Ctt_abstree
open Fsc_config
open Big_int_infix

(* compilation flags *)

let clear_castflag_at_memop = ref true

type compiler_mode = MultiModule | MultiLinker | StandAlone | TrustInterModule | StdlibImplementation

let compiler_mode = ref StandAlone

let max_local_var_size = ref (big_int_of_int 0x2000000)

let use_signed_compare_for_offset = ref false

let force_emit_stack_unwind = ref false

let use_optimized_narrow_cast = ref true

let use_boundary_t = ref true

(* helper functions *)

let builtintype_capable_of_fatvalue = function
    Tdouble | Tfloat | Tlongdouble -> false
  | bt ->
      Ctt_abstree.size_of_builtin_type bt >= sizeof_pointer
