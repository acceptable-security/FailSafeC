(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2009. *)

(** General utility functions. *)

val identity : 'a -> 'a
(** an identity function. *)

val someof : 'a option -> 'a
(** same as [Option.get]. deprecated. *)

val gcd : int -> int -> int
(** returns greatest common divisor. *)

val frexp_int : float -> int64 * int
(** [frexp f] returns a 64-bit integer [s] and an integer [e], where {i f} = {i s} * {i 2^e} holds. *)

val float_of_c99_string : string -> float
(** [float_of_c99_string] is equivalent to [float_of_string], except that it always accepts a hexadecimal float. *)

val map_flatten : ('a -> 'b list) -> 'a list -> 'b list
(** [map_flatten f l] is equivalent to [(List.flatten (List.map f l))], but more efficient on large lists. *)

val map_flatten_i : (int -> 'a -> 'b list) -> 'a list -> 'b list
(** The same as [map_flatten], except the function receives integer index of each element. *)

val list_iteri : (int -> 'a -> 'b) -> 'a list -> unit
(** The same as [List.iter], except the function receives integer index of each element. *)

val split_at_nth : int -> 'a list -> 'a list * 'a list
(** [split_at_nth n l] splits a list two two parts: first n elements and the rest. *)

val list_map : ('a -> 'b) -> 'a list -> 'b list
(** equivalent to [List.map] but more efficient on large lists. *)

val list_map_ordered : ('a -> 'b) -> 'a list -> 'b list
(** equivalent to [List.map] but in-order processing guaranteed. *)

val list_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** equivalent to [List.map2] but more efficient on large lists. *)

val list_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(** equivalent to [list_map] except that the function receives an integer index of each element. *)

val list_repeat : 'a -> int -> 'a list
(** [list_repeat e i] creates a list containing [i] elements of [e]. *)

val list_append : 'a list -> 'a list -> 'a list
(** same as [List.append] but more efficient on large lists. *)

val string_of_list : ('a -> string) -> string -> 'a list -> string
(** [string_of_list f sep l] creates string representation of list [l]:
    each element is converted by [f], and [sep] is inserted between elements. *)

val pp_list : 
    (Format.formatter -> 'a -> unit) ->
      (Format.formatter -> unit) ->
	Format.formatter -> 'a list -> unit
(** [pp_list f sep_f l] pretty-prints the list [l]:
     each element is converted by [f], and [sep] is called between elements. *)

module Careful_integer :
  sig
    exception Integer_overflow
    val ( + ) : int -> int -> int
    val ( - ) : int -> int -> int
    val ( * ) : int -> int -> int
    val ( / ) : int -> int -> int
    val ( ~-) : int -> int
    val abs : int -> int
    val succ : int -> int
    val pred : int -> int
    val incr : int ref -> unit
    val decr : int ref -> unit
  end
    (** Basic integer arithmetic careful of integer overflow. *)

module Earray :
  sig
    type 'a t
   (** the type of the array. *)
    val empty : unit -> 'a t
   (** creates an empty earray. *)
    val empty_with_default : zero:'a -> 'a t
   (** creates an earray, all elements are intrinsicly filled by [zero] element. *)
    val mem : 'a t -> int -> bool
   (** returns whether the specified element is set.
       Note that it may (will) return "true" for every unset element if an earray has a default value. *)
    val get : 'a t -> int -> 'a
   (** returns an element of an earray: raise [Failure] when it is empty. *)
    val get_option : 'a t -> int -> 'a option
   (** returns an element of an earray as an option value. *)
    val unset : 'a t -> int -> unit
   (** clears the specified element to be unset. *)
    val set : 'a t -> int -> 'a -> unit
   (** sets an element of an earray. *)
    val incr : int t -> int -> unit
   (** increments an element of an integer erray. *)
    val length : 'a t -> int
   (** returns the 'length' of the earray: which is larger than the largest index which was set. *)
    val iteri : (int -> 'a -> unit) -> 'a t -> unit
   (** iterates over the non-empty element of the given earray. *)
    val copy : 'a t -> 'a t
   (** makes a copy of an earray. *)
    val map : ('a -> 'b) -> 'a t -> 'b t
   (** [Earray.map f] creates a mapped copy of an earray: non-empty elements are mapped by [f], and
      empty elements are mapped to the empty. *)
    val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
   (** same as [Earray.map f], except the index given to the function. *)
    val filter_i : (int -> 'a -> bool) -> 'a t -> 'a t
   (** creates a copy of an earray, where only elements selected by the filter function is
      set. Other elements will be empty. *) 
  end
    (** Automatically-extended array. Each element in an earray is either set or empty. *)

type 'a earray = 'a Earray.t
(** copy of [Earray] types and functions. *)
val earray_get : 'a Earray.t -> int -> 'a
val earray_set : 'a Earray.t -> int -> 'a -> unit
val earray_length : 'a Earray.t -> int
val earray_iteri : (int -> 'a -> unit) -> 'a Earray.t -> unit
val earray_mem : 'a Earray.t -> int -> bool

val unwind_protect : (unit -> 'a) -> ('a -> 'b) -> ('a -> unit) -> 'b
(** [unwind_protect prepare main cleanup] first calls
   [prepare], whose return value is passed to both [main] and [cleanup].
   Then it calls [main]. The return value of [main] is returned to the caller,
   after [cleanup] is called.
   If [main] raises any exception, it is propargated after [cleanup] is called. 
   Unless [prepare] fails, [cleanup] is always called.
*)

val sfprintf: ('a, Format.formatter, unit, string) format4 -> 'a
(** [sfprintf fmt args...] is similar to [Format.sprintf], but taking
    [fprintf]-compatible formats as an argument. *)

val failwith_p : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failwith_p fmt args...] is a formatting version of [Pervasive.failure]:
    it formats [args] as in [Format.bprintf], and
    then raises [Failure] with the formatted string. *)
