(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Helper module for communication between parser and lexer *)

(** Not to be used directly by users. *)

(**/**)

val name_table : (string * bool) list ref
val scope_stack : (string * bool) list list ref
val push : 'a list ref -> 'a -> unit
val pop : 'a list ref -> 'a
val is_typedef_name : string -> bool
val enter_identifier_scope : unit -> unit
val leave_identifier_scope : unit -> unit
val leave_and_save_identifier_scope : (string * bool) list option ref -> unit
val enter_merge_identifier_scope : (string * bool) list option ref -> unit
val clear_typedef_name : unit -> unit
val add_name : string -> bool -> unit
val lno_table : (int * (string * int * bool)) list ref
val lineno : int ref
val filename : string ref
val file_is_trustful : bool ref
val filecnt : int ref
val reset : string -> unit
val register_linestart : int -> lineno:int -> unit
val lookup_location : int -> Locterm.location
