(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)

(** Appendable List Generator, in O(1) stack consumption. *)

type 'a t
(** Type of the list generator. *)

type 'a backpatch_descriptor
(** a descriptor for backpatch operation *)

val empty : unit -> 'a t
(** Creates new list generator. *)

val put : 'a t -> 'a -> unit
(** Append one element. *)

val put_backpatchable : 'a t -> 'a -> 'a backpatch_descriptor
(** Append one element, remember for future backpatching *)

val backpatch : 'a backpatch_descriptor -> 'a -> unit
(** do backpatch at element put by put_backpatchable *)

val append : 'a t -> 'a list -> unit
(** Append elements. Makes copy of the source list in O(1) stack. *)

val to_list : 'a t -> 'a list
(** Get the result list. It can be called only once. *)

val is_empty : 'a t -> bool
(** Returns [true] if no elements are added. *)

val reset : 'a t -> unit
(** Remove its contents and make it reusable. *)

module Infix :
  sig
    val ( <::= ) : 'a t -> 'a -> unit
    val ( <@= ) : 'a t -> 'a list -> unit
  end
