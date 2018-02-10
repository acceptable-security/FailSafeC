(*pp camlp4o *)
(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2005-2006. *)

open Util

type ltype = 
    LTconcrete of string
  | LTpointer of ltype
  | LTstruct of struct_desc
  | LTarray of int * ltype
  | LTfunction of ltype list * bool * ltype
  | LTunknownfunc
and struct_desc = 
    LTSnamed of string
  | LTShashed of string * int

let strof_strdesc = function
    LTSnamed s -> "<named:" ^ s ^ ">"
  | LTShashed (s, i) -> "<hashed:" ^ s ^ "," ^ string_of_int i ^ ">"

let rec strof_ltype = function
    LTconcrete c -> c
  | LTpointer p -> "*" ^ strof_ltype p
  | LTstruct s -> strof_strdesc s
  | LTarray(s,t) -> strof_ltype t ^ "[" ^ string_of_int s ^ "]"
  | LTfunction(at,vp,rt) ->
      "{" ^ String.concat "," (List.map strof_ltype at)
      ^ (if vp then ",... -> " else " -> ") ^ strof_ltype rt ^ "}"
  | LTunknownfunc -> "{ _ -> _ }"

let string_of_char_list l = 
  let b = Buffer.create 16 in
  List.iter (Buffer.add_char b) l;
  Buffer.contents b

let rec encode_ltype ~no_array_size = function
    LTconcrete c -> c
  | LTpointer t -> "P" ^ encode_ltype ~no_array_size t
  | LTarray(i, t) -> 
      "A" ^ encode_ltype ~no_array_size t ^ "_" ^
      string_of_int (if no_array_size then 0 else i)
  | LTfunction(t, op, rt) -> 
      "F" ^ String.concat "" (List.map (encode_ltype ~no_array_size) t)
      ^ (if op then "V" else "") ^ "_" ^ encode_ltype ~no_array_size rt
  | LTstruct (LTSnamed n) -> "Sn" ^ string_of_int (String.length n) ^ n ^ "_"			     
  | LTstruct (LTShashed (n, id)) -> "Sh_" ^ n ^ "_" ^ string_of_int id ^ "_"			     
  | LTunknownfunc -> "Xuf_"

let encode_ltype ?(no_array_size=false) t = encode_ltype ~no_array_size t

let parse_enctype, parse_enctypelist, parse_structfields = 
  let rec get_type = parser
      [< ' ('a' .. 'z' as c) >] -> 
	LTconcrete (String.make 1 c)
    | [< ''P'; t = get_type >] -> LTpointer t
    | [< ''A'; t = get_type ; ''_'; i = get_integer >] -> LTarray(i, t)
    | [< ''F'; t = get_type_list; op = get_varp; ''_'; rt = get_type >] ->
	LTfunction(t, op, rt)
    | [< ''X' ; ''u'; ''f'; ''_' >] -> LTunknownfunc
    | [< ''S'; t = get_struct >] -> t
  and get_type_list = parser
      [< h = get_type; t = get_type_list >] -> h :: t
    | [< >] -> []
  and get_varp = parser
      [< ''V' >] -> true
    | [< >] -> false
  and get_integer = parser
      [< ''0' >] -> 0
    | [< ' ('1' .. '9') as f; l = get_integer_trail >] ->
	int_of_string (string_of_char_list (f :: l))
  and get_integer_trail = parser
      [< ' ('0' .. '9') as c; l = get_integer_trail >] -> c :: l
    | [< >] -> []
  and get_struct = parser
      [< ''n'; name = get_length_string; ''_' >] -> LTstruct (LTSnamed name)
    | [< ''l' >] -> failwith "parse_enctype: line_based desc should not fed to linker."
    | [< ''h'; ''_'; name = get_underscored_string; ''_' ; id = get_integer; ''_' >] ->
	LTstruct (LTShashed (name, id))
    | [< id = get_integer >] -> failwith "parse_enctype; untagged struct is not allowed here."
  and get_length_string st = 
    let l = get_integer st in
    let s = String.make l ' ' in
    for i = 0 to l - 1 do
      s.[i] <- Stream.next st
    done;
    s
  and get_underscored_string st = 
    let b = Buffer.create 16 in
    let rec iter () = match Stream.peek st with
      Some '_' -> ()
    | None -> raise (Stream.Error "get_underscored_string")
    | Some c -> Buffer.add_char b c; Stream.junk st; iter ()
    in
    iter (); Buffer.contents b
  and get_length_strings = parser 
      [< h = get_length_string; t = get_length_strings >] -> h :: t
    | [< >] -> []
  and get_structfields = parser
      [< n = get_length_string; ''_'; t = get_type; ''_'; tl = get_structfields >] -> (n, t)::tl
    | [< >] -> []
  in

  let f n p = 
    fun s ->
      let st = Stream.of_string s in
      try 
	let t = p st in
	Stream.empty st;
	t
      with
	Stream.Failure | Stream.Error _ ->
	  failwith_p "Linker_types.%s: parse error: %S" n s
  in
  f "get_type" get_type, 
  f "get_type_list" get_type_list,
  f "get_structfields" get_structfields
