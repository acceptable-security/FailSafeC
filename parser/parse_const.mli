(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** String parser for C constants. *)
val parse_constant :
  C_abstree.c_constants -> Ctt_abstree.c_constants * Ctt_abstree.c_type
(** parses C constants and returns its value and type. *)

val decode_c_string : string -> string
(** parses C string constants and returns its byte contents. *)
