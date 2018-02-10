(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   This file is written by Yutaka Oiwa in 2002-2004. *)

(** Common operation for 'a option type *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f a] returns [Some (f x)] when [a = Some x], [None] when [a = None]. *)

val do_option : ('a -> 'b) -> 'a option -> 'b option
(** the same as [map]. *)

val iter : ('a -> unit) -> 'a option -> unit
(** [iter f a] performs [f x] when [a = Some x], nothing when [a = None]. *)

val default : 'a -> 'a option -> 'a
(** [default a dv] returns [x] when [a = Some x], [dv] when [a = None]. *)

val get : 'a option -> 'a
(** [get a] returns [x] when [a = Some x], fails when [a = None]. *)

val to_list : 'a option -> 'a list
(** [to_list a] returns [[x]] when [a = Some x], [[]] when [a = None]. *)

