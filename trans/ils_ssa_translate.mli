(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Making SSA representation over ILS (ILS to ILS) *)

(**
 Both input and output are in the same ILS type.

   Input is assumed to be: 
   -  all branch instructions (conditional and unconditional) are
     only at the bottom of each basic block.
   -  phi functions lists are empty.

   Output will be:
   -  all temporary variables are assigned only once.
   -  all "merges" of variables are represented as phi functions
      attached to the top of each blocks.
 *)

val translate_program :
  (Ils.ils, Ils.ils_type, 'a) Il.il_global_declaration_base list ->
  (Ils.ils, Ils.ils_type, 'a) Il.il_global_declaration_base list
      (** main function. *)
      
val test :
  (Ils.ils, Ils.ils_type, 'a) Il.il_global_declaration_base list -> unit
      (** test routine. *)
