(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Il3_constants: various constants for generating code from IL3. *)

(** functions returning L-values in IL. *)

val rettype_of_lv : 'a Il.il_lvalue -> Ctt_abstree.c_type
val lv_immobile_functions :
  Il3.internal_immobile_functions -> 'a Il.il_lvalue
val lv_immobile_operations :
  Il3.internal_immobile_operations -> 'a Il.il_lvalue
val lv_reducible_functions :
  genv:Ctt_abstree.environment -> Il3.primitive_reducible -> 'a Il.il_lvalue
val lv_reader_helper :
  genv:Ctt_abstree.environment -> Il3.rw_targets -> 'a Il.il_lvalue
val lv_writer_helper :
  genv:Ctt_abstree.environment -> Il3.rw_targets -> 'a Il.il_lvalue
val lv_abortfunc : 'a Il.il_lvalue

(** functions returning the return type of each helper function. *)

val rettype_immobile_functions :
  genv:Ctt_abstree.environment -> Il3.internal_immobile_functions -> Ctt_abstree.c_type
val rettype_reducible_functions :
  genv:Ctt_abstree.environment ->
  Il3.primitive_reducible -> Ctt_abstree.c_type
val rettype_reader_helper :
  genv:Ctt_abstree.environment -> Il3.rw_targets -> Ctt_abstree.c_type
