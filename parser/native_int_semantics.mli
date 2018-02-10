(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Alternative implementations of quotient and modulo on big_int, in native C compatible semantics *)

val quomod_big_int :
  Big_int_infix.big_int ->
  Big_int_infix.big_int -> Big_int_infix.big_int * Big_int_infix.big_int
val div_big_int :
  Big_int_infix.big_int -> Big_int_infix.big_int -> Big_int_infix.big_int
val mod_big_int :
  Big_int_infix.big_int -> Big_int_infix.big_int -> Big_int_infix.big_int
val ( /! ) :
  Big_int_infix.big_int -> Big_int_infix.big_int -> Big_int_infix.big_int
