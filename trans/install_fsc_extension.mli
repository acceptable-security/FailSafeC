(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Install various Fail-Safe C specific hooks for parser and trans modules. *)
val trust_input_file_anyway : bool ref (** A flag to allow extension keywords in all input files *)
val allow_require_libraries : bool ref (** A flag to allow extension requiring native libraries: used in fscw *)
val install : unit -> unit (** main function *)

val fsc_default_il2_reduction_parameter :
  Il2_reduce_local_variable.reduction_parameter (** A reduction parameter used in Il2_reduce_local_variable. *)
