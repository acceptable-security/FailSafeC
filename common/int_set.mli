(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Integer set implemented by using Set, compatible to Set_list. *)

(* abstract type *)

type int_set
(** the type of the set. *)
type t = int_set
(** the type of the set (alias). *)

(* constructors/deconstructors *)

val empty : int_set
(** the empty set. *)
val singleton : int -> int_set
(** making a singleton set from an element *)
val of_list : int list -> int_set
(** making a set fron an integer element list. *)

val to_list : int_set -> int list
(** making a sorted list of elements. *)

(* basic operations *)

val intersection : int_set -> int_set -> int_set
(** intersection of two sets. *)
val subtract : int_set -> int_set -> int_set
(** the unidirectional difference of two sets. *)
val has_intersection : int_set -> int_set -> bool
(** check whether there is any common element in two sets. *)
val union : int_set -> int_set -> int_set
(** union of two sets. *)
val add : int_set -> int -> int_set
(** adding an element to a set. *)

val mem : int -> int_set -> bool
(** check whether an element is contained in a set. *)
val iter : (int -> unit) -> int_set -> unit
(** iterates over elements in a set. *)
