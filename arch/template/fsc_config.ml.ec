<%#
char *caml_bool(int b) {
    return b ? "true" : "false";
}

int is_littleendian(void) {
    union {
	int i;
	char c[sizeof(int)];
    } v;

    v.i = 1;
    return v.c[0];
}

void try_divmod(int i1, int i2) {
    int d, m;

    d = i1 / i2;
    m = i1 % i2;

    printf("(%d, %d)", d, m);
}

struct minimum_struct { char x[1]; };
#%>
(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)
(* <%=s EC_VERSION_MSG %> *)

let bits_of_byte = 8

let sizeof_char = <%=d sizeof(char) %>
let sizeof_short = <%=d sizeof(short) %>
let sizeof_int = <%=d sizeof(int) %>
let sizeof_long = <%=d sizeof(long int) %>
let sizeof_longlong = <%=d sizeof(long long) %>
let sizeof_float = <%=d sizeof(float) %>
let sizeof_double = <%=d sizeof(double) %>
let real_sizeof_longdouble = <%=d sizeof(long double) %>

let emit_downcoerce_longdouble = (* TODO: current limitation *)
  real_sizeof_longdouble > sizeof_longlong
let sizeof_longdouble = 
  if emit_downcoerce_longdouble then
    sizeof_double
  else
    real_sizeof_longdouble

let sizeof_pointer = <%=d sizeof(void *) %>

(* some architecture requires additional alignment (padding) to structs *)
let minimum_align_struct = <%=d sizeof(struct minimum_struct) %>

let max_bitfield_width = sizeof_int * bits_of_byte (* 32 *)

let char_is_signed = <%=s caml_bool(0 > (char)-1) %>
let is_littleendian = <%=s caml_bool(is_littleendian()) %>

type quomod_style = 
    Div_round_to_zero | Mod_match_divisor | Mod_always_positive
let quomod_style = 
  match <% try_divmod(5, 3); %>, <% try_divmod(5, -3); %>,
    <% try_divmod(-5, 3); %>, <% try_divmod(-5, -3); %> with
  (* 5/3     5/-3      -5/3      -5/-3 *)
    (1, 2), (-1, 2),  (-1, -2), (1, -2) -> Div_round_to_zero
  | (1, 2), (-2, -1), (-2, 1),  (1, -2) -> Mod_match_divisor
  | (1, 2), (-1, 2),  (-2, 1),  (2, 1) -> Mod_always_positive
  | _ -> failwith "FSC_CONFIG failed: unknown div/mod semantics"

let print_config () = 
  Printf.printf
    "Configuration values:

sizes:
   %d: char
   %d: short
   %d: int
   %d: long
   %d: long long
   %d: float
   %d: double
   %d: long double (real: %d)
   %d: pointers

char is %s.
endianness is %s-endian.
division semantics is \"%s\".
"
    sizeof_char sizeof_short sizeof_int sizeof_long sizeof_longlong
    sizeof_float sizeof_double sizeof_longdouble
    real_sizeof_longdouble
    sizeof_pointer
    (if char_is_signed then "signed" else "unsigned")
    (if is_littleendian then "little" else "big")
    (match quomod_style with 
      Div_round_to_zero -> "match with absolute value (normal)"
    | Mod_match_divisor -> "modulo sign matches divisor"
    | Mod_always_positive -> "modulo always positive")

let _ = begin (* TODO things *)
  assert (sizeof_longdouble = sizeof_double);
  assert (sizeof_int = sizeof_long);
  assert (sizeof_long = sizeof_pointer);
  assert (sizeof_int = sizeof_float);
  assert (sizeof_double = sizeof_longlong);
end

(* calculate_bitfields_packing *)
(* ����: (������ (bitñ��)) �Υꥹ�� *)
(* ����: (�����Ǥγ��ϰ��� (�Х��Ȱ��֤ȥӥåȰ���) �Υꥹ�� �� ���ΤΥХ�����, ���饤�����׵�) �� triple *)

let rec calculate_bitfields_packing rest_width words =
  function
      [] -> [], words * sizeof_int, sizeof_int
    | (width :: rest) as whole ->
	if width > max_bitfield_width then failwith "too wide bitfield"
	else if width <= rest_width then
	  let (r,final_bytes,final_align) 
	      = calculate_bitfields_packing (rest_width - width) words rest in
	  (((words - 1) * sizeof_int, rest_width - 1) :: r), final_bytes, final_align
	else
	  calculate_bitfields_packing (sizeof_int * bits_of_byte) (words + 1) whole

let calculate_bitfields_packing =
  calculate_bitfields_packing 0 0

let cpp_options = "-undef -D__sparc" (* not used *)

let allow_integer_arith_on_pointer = false

(* specification of runtime library behaviour *)

let use_hardware_null_check = true
let size_check_subsumes_cast_check = true
