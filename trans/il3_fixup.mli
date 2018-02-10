(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** fixup IL3 program: fill types for any untyped declarations *)

val translate_program :
    genv:Ctt_abstree.environment ->
      ('a, 'b) Il3.il3_global_declaration list ->
	('a, 'b) Il3.il3_global_declaration list
(** main function. *)
