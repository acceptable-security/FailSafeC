(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2004. *)

(* set implemented by simple list *)

(* abstract type *)

type 'a set

(* constructors/deconstructors *)

val empty : 'a set
val singleton : 'a -> 'a set
val of_list : 'a list -> 'a set

val to_list : 'a set -> 'a list

(* basic operations *)

val intersection : 'a set -> 'a set -> 'a set
val subtract : 'a set -> 'a set -> 'a set
val has_intersection : 'a set -> 'a set -> bool
val union : 'a set -> 'a set -> 'a set
val subtract : 'a set -> 'a set -> 'a set
val add : 'a set -> 'a -> 'a set

(* higher-order operations *)

val mem : 'a -> 'a set -> bool
val iter : ('a -> unit) -> 'a set -> unit
