(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Generates GNU-ld linker scripts *)

val p : out_channel -> Resolve.t -> unit

val ptail : out_channel -> Resolve.t -> unit

