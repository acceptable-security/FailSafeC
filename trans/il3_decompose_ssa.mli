(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Il3_decompose_ssa: computing phi assignments for IL3 program. *)

(** The module is named to "decompose", but actually it only
    calculates what should be done for decomposition. 
    The actual transformation is done in {! Il3_to_ctt}. *)

type phi_assignment_chains = (int * int) list list
(** list of assignment chains *)

type 'a block_info = {
  mutable trampoline_blocks : (int * 'a) list;
  mutable phi_assignments_at_bottom : (int * 'a) option;
}

type block_phi_info = phi_assignment_chains block_info
(** type representing phi assignments and their placements for each blocks.*)

type phi_info = block_phi_info array
(** phi_info for one function. *)

val translate_function :
  ('a, Il3_optimize.cogen_decision Util.Earray.t) Il3.il3_function ->
  ('a, Il3_optimize.cogen_decision Util.Earray.t * phi_info) Il3.il3_function
(** translate one function. *)

val f :
  ('a, Il3_optimize.cogen_decision Util.Earray.t)
    Il3.il3_global_declaration list ->
      ('a, Il3_optimize.cogen_decision Util.Earray.t * phi_info)
	Il3.il3_global_declaration list
(** translate a whole program. *)
