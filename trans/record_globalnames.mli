(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(** Records global names required for stub generation *)

type globalnames =
    GNtypeinfo of Ctt_abstree.c_type (** type information structures *)
  | GNstorage of Ctt_abstree.c_type * Big_int.big_int option (** types for generating global storages *)
  | GNbasecast of Ctt_abstree.c_type (** function for casting pointer bases *)
  | GNaddptr of Ctt_abstree.c_type  (** function for adding integers to pointer *)
  | GNalloc of Ctt_abstree.c_type (** allocator (obsolete) *)
  | GNgetrealofs of Ctt_abstree.c_type (** function for real address offset from virtual offset *)
  | GNrwmeth of Ctt_abstree.c_type (** reader/writer methods *)

val require_name : globalnames -> unit
    (** requests generation of specific global names *)

val add_struct_dependency : genv:Ctt_abstree.environment -> unit -> unit
    (** adds all structs which is embedded in any requested structs *)

val to_list : unit -> globalnames list
    (** returns all requested names *)

val clear : unit -> unit
    (** clears the registry of requested names. *)

val get_string_constant_id : string -> int
    (** registers string constants to be generated, and returns an identifier No. for it. *)

val get_global_string_list : unit -> (string * int) list
    (** returns the list of all registered strings. *)
