(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2004. *)

(** Topological sorting of list elements regarding defines-depends relation *)

(* 'b must be comparable type *)

type set = Int_set.t

val topological_sort :
  depends:('a -> set) -> defines:('a -> set) -> 'a list -> 'a list
(** computes a list sorted in the order where all elements defining a value appears before all elements depends on the value. *)
