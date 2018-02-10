(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2003-2009. *)

(** Il3_optimize: optimizing / compacting code of IL3 *)


type cogen_decision =
    Omit (** The variable is defined but not used. *)
  | Inline of Il3.temp_id Il3.il3 (** The variable is used once, and it can be moved. *)
  | Constant of Il3.temp_id Il3.il3 (** The variable is simple, and it can be moved. *)
  | GenTempVar1 (** The variable is used once, but it can't be moved easily. *)
  | GenTempVarM (** The variable is used many times. *)
type cogen_decisions = cogen_decision Util.earray
(** Types for analysis results. Can be used for code generation. *)

(** type-A simple optimization *)

val optimize_function_a :
  fname:string ->
  (Il.temp_id, unit) Il3.il3_function ->
  (Il3.il3b_rexp, cogen_decision Util.Earray.t) Il3.il3_function
val optimize_program_a :
  (Il.temp_id, unit) Il3.il3_global_declaration list ->
  (Il3.il3b_rexp, cogen_decision Util.Earray.t) Il3.il3_global_declaration
  list

(** type-B aggressive optimization *)

val optimize_function_b :
  fname:string ->
  (Il.temp_id, unit) Il3.il3_function ->
  (Il3.il3b_rexp, cogen_decision Util.Earray.t) Il3.il3_function
val optimize_program_b :
  (Il.temp_id, unit) Il3.il3_global_declaration list ->
  (Il3.il3b_rexp, cogen_decision Util.Earray.t) Il3.il3_global_declaration
  list
