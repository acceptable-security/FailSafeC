(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Lexer for C syntax. *)

exception Eof
val use_failsafec_extension : bool ref
(** Whether to use Fail-Safe C extended keywords. *)

val use_c99_constants : bool ref
(** Whether to allow C99-style hexadecimal floating constants. *)

val extension_keywords : C_abstree.identifier list ref
(** Keywords which will be accepted as extension specifiers. *)

val token : Lexing.lexbuf -> C_parser.token
