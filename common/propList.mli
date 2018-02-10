(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2003. *)

(* Implementation of type-safe, polymorphic property list *)

(* The abstract type of property lists. *)
type t

(* The abstract type of property tags. *)
(*   'a is the type of data associated with the tag. *)
type 'a tag

(* Makes an empty property list. *)
val empty : unit -> t

(* Makes a globally-unique new tag. *)
(* The argument is an example value, which is used for proper typing. *)
val make_new_tag : unit -> 'a tag

(* Put a new property (tag-value pair) to the property list. *)
val put : 'a tag -> 'a -> t -> unit

(* Get a property associated with given tag. *)
(* Raises Not_found if there isn't. *)
val get : 'a tag -> t -> 'a

(* Checks whether a property list contains a value with given tag. *)
val mem : 'a tag -> t -> bool

