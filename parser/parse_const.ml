(* Part of Fail-Safe C. (c) 2002-2004 Yutaka Oiwa. *)

open Util
open Big_int_infix
open C_abstree
open Ctt_abstree

let int_of_char c = 
  match c with
    '0' .. '9' -> Char.code c - Char.code '0'
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | _ -> assert false

let big_int_of_char c = big_int_of_int (int_of_char c)

exception Parse_Failure of int

let c_int_of_string s = 
  let l = String.length s in
  let rec start () = 
    match s.[0] with
      '0' -> zero 1
    | '1' .. '9' as c -> 
	decimal (big_int_of_char c) 1
    | _ -> raise (Parse_Failure 0)
	 
  and decimal v i =
    if i = l then v, type_int else
    match s.[i] with
      '0' .. '9' as c -> decimal (int_of_char c +%! 10 *%! v) (i + 1)
    | 'U' | 'u' -> postfix_u v (i + 1)
    | 'L' | 'l' -> postfix_l v (i + 1) s.[i]
    | _ -> raise (Parse_Failure i)
  
  and zero i = 
    if i = l then zero_big_int, type_int else
    match s.[i] with
      'x' | 'X' -> hexadecimal zero_big_int (i + 1)
    | '0' .. '7' as c -> octal (big_int_of_char c) (i + 1)
    | 'U' | 'u' -> postfix_u zero_big_int (i + 1)
    | 'L' | 'l' -> postfix_l zero_big_int (i + 1) s.[i]
    | _ -> raise (Parse_Failure i)
   
  and octal v i = 
    if i = l then v, type_int else
    match s.[i] with
      '0' .. '7' as c -> octal (int_of_char c +%! 8 *%! v) (i + 1)
    | 'U' | 'u' -> postfix_u v (i + 1)
    | 'L' | 'l' -> postfix_l v (i + 1) s.[i]
    | _ -> raise (Parse_Failure i)

  and hexadecimal v i = 
    if i = l then v, type_int else
    match s.[i] with
      '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' as c ->
	hexadecimal (int_of_char c +%! 16 *%! v) (i + 1)
    | 'U' | 'u' -> postfix_u v (i + 1)
    | 'L' | 'l' -> postfix_l v (i + 1) s.[i]
    | _ -> raise (Parse_Failure i)
	  
  and postfix_u v i = 
    if i = l then v, type_unsigned_int else
    match s.[i] with
      'L' | 'l' -> postfix_ul v (i + 1) s.[i]
    | _ -> raise (Parse_Failure i)

  and postfix_ul v i el = 
    if i = l then v, type_unsigned_int else
    match s.[i] with
      'L' | 'l' when s.[i] = el -> finish v type_unsigned_long_long (i + 1)
    | _ -> raise (Parse_Failure i)

  and postfix_l v i el = 
    if i = l then v, type_long else
    match s.[i] with
      'L' | 'l' when s.[i] = el -> postfix_ll v (i + 1)
    | 'U' | 'u' -> finish v type_unsigned_long (i + 1)
    | _ -> raise (Parse_Failure i)

  and postfix_ll v i = 
    if i = l then v, type_long_long else
    match s.[i] with
      'U' | 'u' -> finish v type_unsigned_long_long (i + 1)
    | _ -> raise (Parse_Failure i)

  and finish v t i = 
    if i = l then v, t else raise (Parse_Failure i)

  in
  try
    start ()
  with
    Parse_Failure i ->
      failwith (Printf.sprintf "c_int_of_string: char #%d in %S is invalid" i s)

let max_int = get_max_value_of_type type_int
let max_uint = get_max_value_of_type type_unsigned_int
let max_long = get_max_value_of_type type_long
let max_ulong = get_max_value_of_type type_unsigned_int
let max_ll = get_max_value_of_type type_long_long
let max_ull = get_max_value_of_type type_unsigned_long_long

let promote_trial_list = 
  [ type_int, 
    [ type_int, max_int;
      type_unsigned_int, max_uint;
      type_long, max_long;
      type_unsigned_long, max_ulong;
      type_long_long, max_ll;
      type_unsigned_long_long, max_ull ];
    type_unsigned_int,
    [ type_unsigned_int, max_uint;
      type_unsigned_long, max_ulong;
      type_unsigned_long_long, max_ull ];
    type_long,
    [ type_long, max_long;
      type_unsigned_long, max_ulong;
      type_long_long, max_ll;
      type_unsigned_long_long, max_ull ];
    type_unsigned_long,
    [ type_unsigned_long, max_ulong;
      type_unsigned_long_long, max_ull ];
    type_long_long, 
    [ type_long_long, max_ll;
      type_unsigned_long_long, max_ull ];
    type_unsigned_long_long, 
    [ type_unsigned_long_long, max_ull ] ]

let maximal_unsigned_long_long = 
  pred_power2_big_int (Fsc_config.sizeof_longlong * Fsc_config.bits_of_byte)

let promote_integer_constant t v = 
  assert (v >=! zero_big_int);
  let trials = List.assq t promote_trial_list in
  let rec iter = function
      [] -> (* failwith "too big integer constant" *)
	land_big_int v maximal_unsigned_long_long, type_unsigned_long_long
    | (tresult, max) :: tl ->
	if v <=! max then v, tresult else iter tl
  in
  iter trials

let c_float_of_string s = 
  let l = String.length s in
  let t, trunc =
    match s.[l - 1] with
      'F' | 'f' -> type_float, true
    | 'L' | 'l' -> type_long_double, true
    | _ -> type_double, false in
  let s = if trunc then String.sub s 0 (l - 1) else s in
  let v = Util.float_of_c99_string s in
  let v = 
    if Option.get t.ct_size <=! big_int_of_int 4 then
      Int32.float_of_bits (Int32.bits_of_float v)
    else
      v
  in
  v, t

let backslash_table = 
  [ 'a', '\007'; 'b', '\008'; 'f', '\012';
    'n', '\010'; 'r', '\013';
    't', '\009'; 'v', '\011';
    '"', '"'   ; '\'', '\'' ; '?', '?'; '\\', '\\' ]

let max_char = 256

let decode_c_string s = 
  let l = String.length s in
  let b = Buffer.create l in

  let rec normal i =
    if i = l then () else
    match s.[i] with
      '\\' -> backslash (i + 1)
    | c -> Buffer.add_char b c; normal (i + 1)

  and backslash i = 
    if i = l then failwith "decode_c_string" else
    match s.[i] with
      '0' .. '7' as c -> octal 1 (int_of_char c) (i + 1)
    | 'x' -> hexadecimal 0 0 (i + 1)
    | c -> begin
	try 
	  let cq = List.assoc c backslash_table in 
	  Buffer.add_char b cq; normal (i + 1)
	with
	  Not_found -> failwith "decode_c_string: unknown char escape"
    end
  and octal ol v i = 
    if i = l then Buffer.add_char b (Char.chr v) else
    match s.[i] with
      '0' .. '7' as c when ol < 4 ->
	let v = v * 8 + int_of_char c in
	if v < max_char then
	  octal (ol + 1) v (i + 1)
	else
	  failwith "decode_c_string: too big octal char escape"
    | _ ->
	Buffer.add_char b (Char.chr v); normal i
	
  and hexadecimal hl v i = 
    if i = l then Buffer.add_char b (Char.chr v) else
    match s.[i] with
      '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' as c ->
	let v = v * 16 + int_of_char c in
	if v < max_char then
	  hexadecimal (hl + 1) v (i + 1)
	else
	  failwith "decode_c_string: too big hexadecimal char escape"
    | _ ->
	if hl <> 0 then (Buffer.add_char b (Char.chr v); normal i)
	else failwith "decode_c_string: empty hexadecimal char escape"

  in
  normal 0;
  Buffer.contents b

let parse_constant = function
    PconstInteger(s) ->
      let v, t = c_int_of_string s in
      let v, t = promote_integer_constant t v in
      CTTconstInteger(v), t
  | PconstFloat(s) ->
      let v, t = c_float_of_string s in
      CTTconstFloat(v), t
  | PconstString(s) ->
      let s = String.concat "" (list_map decode_c_string s) in
      CTTconstString(s ^ "\000"), type_char_array (String.length s + 1)
  | PconstChar(s) ->
      let s' = decode_c_string s in
      if String.length s' <> 1 then
	failwith (Printf.sprintf "invalid character constant: '%s'" s);
      CTTconstInteger(big_int_of_int (Char.code s'.[0])), type_int
