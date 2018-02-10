(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Simplify constant expressions. *)

exception NotConstant
(** raised when the expression is not a constant. *)

val reduce_expression :
  Ctt_abstree.texpr Locterm.t -> Ctt_abstree.texpr Locterm.t
(** given an expression, returns a simplified expression. *)
