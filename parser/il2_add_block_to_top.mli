(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** A support module for IL2: add some instructions to function top. *)

val add_code :
    function_loc:Locterm.location -> Il2.il2 list -> Il2.il2_function_body -> Il2.il2_function_body
