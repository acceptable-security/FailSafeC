(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2005-2007. *)

(** Module for generating debugging messages. *)

(** To use this module, put
[include Debug.Install (struct let category = XXX end)]
on the top of each file.
*)

val set_debug_flags : category:int -> level:int -> unit
(** sets debugging level for the specified category. *)
val parse_debug_flags : string -> unit
(** parses Sendmail-style debug flag specifications. *)
val dprintf : category:int -> int -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [dprintf ~category lv fmt ...] prints the message formatted by [fmt],
   when the current debugging level for the specified category is not smaller than [lv]. *)
val get_debug_flags : category:int -> int
(** returns debugging level for the specified category. *)

val category_for_general_progress : int ref
(** [category_for_general_progress] defines one global category which controls progress display for all categories.
   Default: 2 *)
val level_for_general_progress : int ref
(**  defines the debug level above which detailed progress is reported. Default: 1 *)

module Install (X : sig val category : int end) :
    sig
      val get_debug_flags : unit -> int
      val dprintf : int -> ('a, Format.formatter, unit, unit) format4 -> 'a
   (** [dprintf lv fmt ...] prints the message formatted by [fmt],
      when the current debugging level is not smaller than [lv]. *)
      val dprintf_start : ('a, Format.formatter, unit, unit) format4 -> 'a
   (** used for the progress report message for starting operations. *)
      val dprintf_progress : ('a, Format.formatter, unit, unit) format4 -> 'a
   (** used for the progress report message during operations. If the debugging level is
    the same as [level_for_general_progress], single "." is displayed for each call of this function.
    If the level is more than that, the whole message is shown.
    *)
      val dprintf_end : ('a, Format.formatter, unit, unit) format4 -> 'a
   (** used for the progress report message for finishing operations. *)
    end
    (** introduces debugging functions specialized for the category. *)
