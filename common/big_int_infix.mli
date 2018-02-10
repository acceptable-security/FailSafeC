(* Part of Fail-Safe C. (c) 2002-2004 Yutaka Oiwa. *)

(** Module providing infix operators and additional (missing) functions for Big_int. *)

open Big_int

type big_int = Big_int.big_int

val ( +! ) : big_int -> big_int -> big_int (** addition *)
val ( +%! ) : int -> big_int -> big_int (** adding an integer to big_int *)
val ( -! ) : big_int -> big_int -> big_int (** subtract *)
val ( *! ) : big_int -> big_int -> big_int (** multiply *)
val ( *%! ) : int -> big_int -> big_int (** multiply big_int with an integer *)
val ( /! ) : big_int -> big_int -> big_int (** division *)
val neq_big_int : big_int -> big_int -> bool (** non-equality (alias) *)
val ( =! ) : big_int -> big_int -> bool (** equality *)
val ( ==! ) : big_int -> big_int -> bool (** equality (alias) *)
val ( <>! ) : big_int -> big_int -> bool (** non-equality *)
val ( <! ) : big_int -> big_int -> bool (** less *)
val ( <=! ) : big_int -> big_int -> bool (** not larger *)
val ( >! ) : big_int -> big_int -> bool (** larger *)
val ( >=! ) : big_int -> big_int -> bool (** not smaller *)
val ( **%%! ) : int -> int -> big_int (** power of an integer to an integer, returned as big_int *)
val ( **!% ) : big_int -> int -> big_int (** power of a big_int to an integer *)
val ( **%! ) : int -> big_int -> big_int (** power of an integer to a big_int *)
val ( **! ) : big_int -> big_int -> big_int (** power *)
val ( ~-! ) : big_int -> big_int (** negative *)
val big_int : int -> big_int     (** converts an integer to a big_int *)
val lnot_big_int : big_int -> big_int (** logical bitwise negation in 2's complements *)
val land_big_int : big_int -> big_int -> big_int (** logical bitwise and *)
val lor_big_int : big_int -> big_int -> big_int (** logical bitwise or *)
val lxor_big_int : big_int -> big_int -> big_int (** logical bitwise xor *)
val power2_big_int : int -> big_int (** powers of 2 in big_int *)
val pred_power2_big_int : int -> big_int (** powers of 2 minus 1 *)
val shift_left_big_int_positive_int : big_int -> int -> big_int (** big_int bit-shifted to left by a positive integer *)
val shift_right_big_int_positive_int : big_int -> int -> big_int (** big_int bit-shifted to right by a positive integer *)
val ( <<!% ) : big_int -> int -> big_int (** an alias of shift_left_big_int_positive_int *)
val ( >>!% ) : big_int -> int -> big_int (** an alias of shift_right_big_int_positive_int *)
val big_int_of_int64 : int64 -> big_int (** converts int64 to big_int *)
val big_int_of_float : float -> big_int (** converts float to big_int *)
val round_to_signed_binary_big_int : big_int -> int -> big_int (** [round_to_signed_binary_big_int b i] rounds the value [b] into [i] bit signed representation. *)
val round_to_unsigned_binary_big_int : big_int -> int -> big_int  (** [round_to_unsigned_binary_big_int b i] rounds the value [b] into [i] bit unsigned representation. *)

(** The functions inherited from original big_int *)

	val zero_big_int : big_int
	val unit_big_int : big_int
	val minus_big_int : big_int -> big_int
	val abs_big_int : big_int -> big_int
	val add_big_int : big_int -> big_int -> big_int
	val succ_big_int : big_int -> big_int
	val add_int_big_int : int -> big_int -> big_int
	val sub_big_int : big_int -> big_int -> big_int
	val pred_big_int : big_int -> big_int
	val mult_big_int : big_int -> big_int -> big_int
	val mult_int_big_int : int -> big_int -> big_int
	val square_big_int : big_int -> big_int
	val sqrt_big_int : big_int -> big_int
	val quomod_big_int : big_int -> big_int -> big_int * big_int
	val div_big_int : big_int -> big_int -> big_int
	val mod_big_int : big_int -> big_int -> big_int
	val gcd_big_int : big_int -> big_int -> big_int
	val power_int_positive_int : int -> int -> big_int
	val power_big_int_positive_int : big_int -> int -> big_int
	val power_int_positive_big_int : int -> big_int -> big_int
	val power_big_int_positive_big_int : big_int -> big_int -> big_int
	val sign_big_int : big_int -> int
	val compare_big_int : big_int -> big_int -> int
	val eq_big_int : big_int -> big_int -> bool
	val le_big_int : big_int -> big_int -> bool
	val ge_big_int : big_int -> big_int -> bool
	val lt_big_int : big_int -> big_int -> bool
	val gt_big_int : big_int -> big_int -> bool
	val max_big_int : big_int -> big_int -> big_int
	val min_big_int : big_int -> big_int -> big_int
	val num_digits_big_int : big_int -> int
	val string_of_big_int : big_int -> string
	val big_int_of_string : string -> big_int
	val big_int_of_int : int -> big_int
	val is_int_big_int : big_int -> bool
	val int_of_big_int : big_int -> int
	val float_of_big_int : big_int -> float
	val nat_of_big_int : big_int -> Nat.nat
	val big_int_of_nat : Nat.nat -> big_int
