(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** The main module of the Fail-Safe C compiler. *)

type stopat
val process : stopat:stopat -> outfile:string -> string -> unit

val userusage : (unit -> unit) ref

val f_options : (string * Arg.spec * string) list
val parse_f : string -> unit
