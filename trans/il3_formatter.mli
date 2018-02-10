(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Formatters for IL3 *)

(**{6 formatters for simple IL3} *)

val pp_temp_id : Format.formatter -> int -> unit
val pp_rwtype : Format.formatter -> Il3.rw_targets -> unit
val pp_internal_immobile_functions :
  Format.formatter -> Il3.internal_immobile_functions -> unit
val pp_internal_immobile_operations :
  Format.formatter -> Il3.internal_immobile_operations -> unit
val pp_primitive_reducible :
  Format.formatter -> Il3.primitive_reducible -> unit
val pp_il3_rexp :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a Il3.il3_reducible_exp -> unit
val pp_il3_iexp :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a Il3.il3_irreducible_exp -> unit
val pp_il3 :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Il3.il3 -> unit
val pp_il3_basic_block :
  (Format.formatter -> 'a -> unit) ->
  int -> Format.formatter -> 'a Il3.il3 Il.il_basic_block_base -> unit
val pp_il3_function :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> ('a, 'b) Il3.il3_function -> unit
val pp_il3_global_declaration :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> ('a, 'b) Il3.il3_global_declaration -> unit
val pp_il3_program :
  (Format.formatter -> 'a -> unit) ->
  'b -> Format.formatter -> ('a, 'c) Il3.il3_global_declaration list -> unit
(** to format IL3 program use [(pp_il3_program pp_temp_id pp_ignore)]. *)

(**{6 formatters for IL3B} *)

val pp_il3b_rexp : Format.formatter -> Il3.il3b_rexp -> unit
(** to format IL3B program use [(pp_il3_program pp_il3b_rexp pp_ignore)]. *)

val pp_unit : Format.formatter -> unit -> unit
(** Trivial (no-op) formatter for [unit]. *)
val pp_ignore : Format.formatter -> 'a -> unit
(** Formatter version of [Pervasive.ignore]. These are used for skipping "[more_info]" field of IL3 function. *)
