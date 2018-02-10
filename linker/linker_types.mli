(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** The type definition for C types in linker *)

type ltype =
    LTconcrete of string    (** concrete, built-in types *)
  | LTpointer of ltype      (** pointers *)
  | LTstruct of struct_desc (** structs *)
  | LTarray of int * ltype  (** arrays *)
  | LTfunction of ltype list * bool * ltype (** functions *)
  | LTunknownfunc           (** functions with unknown argument types *)
and struct_desc = 
    LTSnamed of string (** specially name assigned *)
  | LTShashed of string * int (** normal *)

val parse_enctype : string -> ltype 
(** parses types encoded in linker information *)
val parse_structfields : string -> (string * ltype) list
(** parses list of struct fields and those types encoded in linker information *)

val encode_ltype : ?no_array_size:bool -> ltype -> string
(** returns encoded representation of a linker type *)

val strof_ltype : ltype -> string
(** returns readable representation of a linker type *)

val strof_strdesc : struct_desc -> string
(** returns readable representation of a struct descriptor *)

