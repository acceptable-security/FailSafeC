(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

(** Type for terms attributed with a location in source files. *)

type location =
    Located of string * int * int * bool
  | Unlocated
(** the type of locations in the source file: file-name, line, column, and whether it is came from trusted file. *)

type 'a t = private { locterm_v : 'a; locterm_loc : location; }
(** a term attributed with a location *)

val dummy_location : location
(** a dummy location not specifying any source location. *)

val locput : loc:location -> 'a -> 'a t
(** puts a location for a term. *)
val locput_dummy : 'a -> 'a t
(** puts the dummy location for a term. *)

val locval : 'a t -> 'a
(** gets a term inside a located-term. *)
val locget : 'a t -> location
(** gets a location from a located-term. *)

val locpair : 'a t -> location * 'a
(** [locpair v = locget v, locpair v]. Used for pattern matching. *)

val loccopy : orig:'a t -> 'b -> 'b t
(** creates a new term which shares the same location as [orig]. *)

val locmap : ('a -> 'b) -> 'a t -> 'b t
(** given a conversion function, creates a new converted term with the same location. *)

val locmap_l : (loc:location -> 'a -> 'b) -> 'a t -> 'b t
(** [locmap] with location information passed to the conversion function. *)

val locmap_list : ('a -> 'b) -> 'a t list -> 'b t list
val locmapl_list : (loc:location -> 'a -> 'b) -> 'a t list -> 'b t list
(** [locmap] and [locmap_l] combined with [List.map]. *)
val locmap_flatten : (loc:location -> 'a -> 'b list) -> 'a t list -> 'b t list
(** [locmap_l] combined with [Util.map_flatten]. *)

val lociter_list : ('a -> unit) -> 'a t list -> unit
(** given a process function, iterates over the list of located terms. *)

val strof_location : location -> string
(** generate a string representation of the location. *)

(** {6 Special formatter for location handling} *)

(** A formatter which inserts an appropriate #file directives.
    note: it is currently designed with C language's and gcc's semantics.
 *)

val encode_c_string : string -> string
(** create a C notation of given string. *)

val location_formatter_of_channel : ?fname:string -> out_channel -> Format.formatter
(** create a location formatter to any out channel. *)

val location_formatter : Format.formatter
(** a pre-constructed location formatter to stdout. *)

val mark_location_open : Format.formatter -> location -> unit
(** insert a opening marker tag of the location. *)

val mark_location_close : Format.formatter -> location -> unit
(** insert a closing marker tag of the location. *)
