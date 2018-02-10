(* Part of Fail-Safe C. (c) 2002-2004 Yutaka Oiwa. *)

include Big_int
open Nat

let ( +! ) = add_big_int
let ( +%! ) = add_int_big_int
let ( -! ) = sub_big_int
let ( *! ) = mult_big_int
let ( *%! ) = mult_int_big_int
let ( /! ) = div_big_int

let neq_big_int a b = not (eq_big_int a b)

let ( =! ) = eq_big_int
let ( ==! ) = eq_big_int
let ( <>! ) = neq_big_int
let ( <! ) = lt_big_int
let ( <=! ) = le_big_int
let ( >! ) = gt_big_int
let ( >=! ) = ge_big_int

let ( **%%! ) = power_int_positive_int
let ( **!% ) = power_big_int_positive_int
let ( **%! ) = power_int_positive_big_int
let ( **! ) = power_big_int_positive_big_int

let ( ~-! ) = minus_big_int

let big_int = big_int_of_int

let big_int_of_int64 i = 
  big_int_of_string (Int64.to_string i)

(* notice:
   Many of the following routines differ in handling of negative
   numbers from ones in big_int.cma of OCaml 3.12.0.
   Ours handle negatives as 2's complements, where OCaml's 
   handle them as either 1's complements (marked as NH_D) or
   error (marked as NH_U).
   Do not simply replace them with OCaml's ones.
 *)

let lnot_big_int b = 
  pred_big_int (minus_big_int b)

let land_big_int = (* NH_U *)
  let land_digits an a_size a_comp bn b_size b_comp = 
    if a_comp then complement_nat an 0 a_size;
    if b_comp then complement_nat bn 0 b_size;
    if a_size > b_size then
      if b_comp then begin
	for i = 0 to b_size - 1 do land_digit_nat an i bn i done;
	an, a_size
      end else begin
	for i = 0 to b_size - 1 do land_digit_nat bn i an i done;
	bn, b_size
      end
    else
      if a_comp then begin
	for i = 0 to a_size - 1 do land_digit_nat bn i an i done;
	bn, b_size
      end else begin
	for i = 0 to a_size - 1 do land_digit_nat an i bn i done;
	an, a_size
      end
  in
  fun a b ->
    let a_sign, b_sign = sign_big_int a, sign_big_int b in
    if a_sign = 0 || b_sign = 0 then zero_big_int else
    let ap, af = if a_sign < 0 then lnot_big_int a, true else a, false in
    let bp, bf = if b_sign < 0 then lnot_big_int b, true else b, false in
    let an = nat_of_big_int ap in
    let bn = nat_of_big_int bp in
    let an_size = length_nat an in
    let bn_size = length_nat bn in
    let r, r_size = land_digits an an_size af bn bn_size bf in
    if af && bf then begin
      (* neg & neg = neg *)
      complement_nat r 0 r_size;
      lnot_big_int (big_int_of_nat r)
    end
    else big_int_of_nat r

let lor_big_int = (* NH_U *)
  let lor_digits an a_size a_comp bn b_size b_comp = 
    if a_comp then complement_nat an 0 a_size;
    if b_comp then complement_nat bn 0 b_size;
    if a_size > b_size then
      if b_comp then begin
	for i = 0 to b_size - 1 do lor_digit_nat bn i an i done;
	bn, b_size
      end else begin
	for i = 0 to b_size - 1 do lor_digit_nat an i bn i done;
	an, a_size
      end
    else
      if a_comp then begin
	for i = 0 to a_size - 1 do lor_digit_nat an i bn i done;
	an, a_size
      end else begin
	for i = 0 to a_size - 1 do lor_digit_nat bn i an i done;
	bn, b_size
      end
  in
  fun a b ->
    let a_sign, b_sign = sign_big_int a, sign_big_int b in
    if a_sign = 0 then b else
    if b_sign = 0 then a else
    let ap, af = if a_sign < 0 then lnot_big_int a, true else a, false in
    let bp, bf = if b_sign < 0 then lnot_big_int b, true else b, false in
    let an = nat_of_big_int ap in
    let bn = nat_of_big_int bp in
    let an_size = length_nat an in
    let bn_size = length_nat bn in
    let r, r_size = lor_digits an an_size af bn bn_size bf in
    if af || bf then begin
      complement_nat r 0 r_size;
      lnot_big_int (big_int_of_nat r)
    end
    else big_int_of_nat r

let lxor_big_int = (* NH_U *)
  let lxor_digits an a_size a_comp bn b_size b_comp = 
    if a_comp then complement_nat an 0 a_size;
    if b_comp then complement_nat bn 0 b_size;
    if a_size > b_size then begin
      for i = 0 to b_size - 1 do lxor_digit_nat an i bn i done;
      if b_comp then complement_nat an b_size (a_size - b_size);
      an, a_size
    end else begin
      for i = 0 to a_size - 1 do lxor_digit_nat bn i an i done;
      if a_comp then complement_nat bn a_size (b_size - a_size);
      bn, b_size
    end
  in
  fun a b ->
    let a_sign, b_sign = sign_big_int a, sign_big_int b in
    if a_sign = 0 then b else
    if b_sign = 0 then a else
    let ap, af = if a_sign < 0 then lnot_big_int a, true else a, false in
    let bp, bf = if b_sign < 0 then lnot_big_int b, true else b, false in
    let an = nat_of_big_int ap in
    let bn = nat_of_big_int bp in
    let an_size = length_nat an in
    let bn_size = length_nat bn in
    let r, r_size = lxor_digits an an_size af bn bn_size bf in
    if af && not bf || bf && not af then begin
      (* neg | any = neg *)
      complement_nat r 0 r_size;
      lnot_big_int (big_int_of_nat r)
    end
    else big_int_of_nat r

let maximal_shift_width = ref 255

let check_shift_width x = 
  if x < 0 || x > !maximal_shift_width
  then failwith "big_int_infix: too big shift width"
  else ()

let trash_nat = nat_of_int 1

let power2_big_int_real x = 
  check_shift_width x;
  let sz = x / Nat.length_of_digit + 1 in
  let pos = x / Nat.length_of_digit in
  let n = Nat.make_nat sz in
  set_digit_nat n pos 1;
  shift_left_nat n pos 1 trash_nat 0 (x mod Nat.length_of_digit);
  big_int_of_nat n

let pred_power2_big_int_real x = 
  pred_big_int (power2_big_int_real x)

let power2_big_int_tables = 
  Array.init 80 (fun i -> power2_big_int_real i)
  
let pred_power2_big_int_tables = 
  Array.init 80 (fun i -> pred_big_int (power2_big_int_tables.(i)))
  
let power2_big_int x = 
  if x < 80 then power2_big_int_tables.(x) else power2_big_int_real x

let pred_power2_big_int x = 
  if x < 80 then pred_power2_big_int_tables.(x) else pred_power2_big_int_real x

let shift_left_big_int_positive_int b s = 
  b *! (power2_big_int s)

let shift_right_big_int_positive_int b s = (* NH_D *)
  b /! (power2_big_int s)

let ( <<!% ) = shift_left_big_int_positive_int
let ( >>!% ) = shift_right_big_int_positive_int (* NH_D *)

let big_int_of_float f = 
  let f = floor f in
  let i, v = Util.frexp_int f in
  assert (v >= 0);
  let r = (big_int_of_int64 i) *! (power2_big_int v) in
  assert (float_of_big_int r = f);
  r

let round_to_unsigned_binary_big_int i bits = 
  let mask = pred_power2_big_int bits in
  land_big_int i mask

let round_to_signed_binary_big_int i bits = 
  let i = round_to_unsigned_binary_big_int i bits in
  if i >=! power2_big_int (bits - 1) then
    i -! power2_big_int bits
  else
    i

(*
let test () = 
  let big_int_of_int64 i = big_int_of_string (Int64.to_string i) in
  let print_big_int b = print_endline (string_of_big_int b) in
  let pl3 = big_int_of_int 3 in
  let mi2 = big_int_of_int (-2) in
  let mi1 = big_int_of_int (-1) in
  let pl3F = big_int_of_int64 0x33333333FFFFFFFFL in
  let test f = 
    print_big_int (f mi2 pl3F);
    print_big_int (f mi2 mi1);
    print_big_int (f mi2 unit_big_int);
    print_big_int (f pl3 unit_big_int) in
  test land_big_int;
  test lor_big_int;
  test lxor_big_int

let test_shift () = 
  let test f s = 
    for x = -8 to 8 do
      let xb = big_int_of_int x in
      let r = f xb s in
      print_string (string_of_big_int r);
      print_char ' '
    done;
    print_newline ()
  in
  test shift_left_big_int_positive_int 3;
  test shift_left_big_int_positive_int 2;
  test shift_left_big_int_positive_int 1;
  test shift_left_big_int_positive_int 0;
  print_newline ();
  test shift_right_big_int_positive_int 0;
  test shift_right_big_int_positive_int 1;
  test shift_right_big_int_positive_int 2;
  test shift_right_big_int_positive_int 3
*)
let test () = 
  let test f s = 
    for x = -8 to 8 do
      let xb = big_int_of_int x in
      let r = f xb s in
      print_string (string_of_big_int r);
      print_char ' '
    done;
    print_newline ()
  in
  test round_to_unsigned_binary_big_int 1;
  test round_to_unsigned_binary_big_int 2;
  test round_to_unsigned_binary_big_int 3;
  test round_to_unsigned_binary_big_int 4;
  print_newline ();
  test round_to_signed_binary_big_int 1;
  test round_to_signed_binary_big_int 2;
  test round_to_signed_binary_big_int 3;
  test round_to_signed_binary_big_int 4
