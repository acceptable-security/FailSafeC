(* Part of Fail-Safe C. (c) 2002-2009 Yutaka Oiwa. *)

(** {2}Additional operations on option types. *)

(** obsolete: use Option.get *)
let someof = function
    Some s -> s
  | None -> invalid_arg "Util.someof"

(** {2}Additional integer operations. *)

let rec gcd x y = if y = 0 then x else gcd y (x mod y)

module Careful_integer = struct
  exception Integer_overflow
  let (+), (-), ( * ), ( / )  = 
    (fun a b ->
      let result = a + b in
      if b < 0 then
	if result > a then raise Integer_overflow else result
      else
	if result < a then raise Integer_overflow else result),
    (fun a b ->
      let result = a - b in
      if b < 0 then
	if result < a then raise Integer_overflow else result
      else
	if result > a then raise Integer_overflow else result),
    (fun a b ->
      if b = 0 then 0 else
      if a = min_int && b = -1 then raise Integer_overflow else
      let m = a * b in
      if m / b = a then m else raise Integer_overflow),
    (fun a b ->
      if b = 0 then raise Division_by_zero else
      if a = min_int && b = -1 then raise Integer_overflow else a / b)
  let abs x = if x >= 0 then x else if x = min_int then raise Integer_overflow else -x
  let ( ~- ) = (-) 0
  let pred x = x - 1
  let succ x = x + 1
  let incr x = x := succ !x
  let decr x = x := pred !x
end
	  
(** {2}Additional float operations. *)

let frexp_int f = 
  (** returns integer significant and exponent of f.
     when (x, n) = frexp_int f, 
     x is an 64bit integer, n is integer, and
     f = x *. 2 ** n holds. *)
  if f = 0.0 then 0L, 0 else
  let rec iter s oexp =
    if (fst (modf s) = 0.0) then s, oexp
    else
      iter (s *. 2.0) (oexp - 1)
  in
  let x, e = frexp f in
  let x, e = iter x e in
  let x64 = Int64.of_float x in
  assert (Int64.to_float x64 = x);
  assert (ldexp x e = f);
  x64, e

(** {2}Additional list operations. *)

(** now using ExtLib *)

module ExtList_addition = struct
  (* copied from ExtList *)
  type 'a mut_list =  {
      hd: 'a; 
      mutable tl: 'a list
    }
  external inj : 'a mut_list -> 'a list = "%identity"
  let dummy_node () = { hd = Obj.magic (); tl = [] }

  let map_flatten f l = 
    let rec inner dst = function
	[] -> dst
      | h :: t -> 
	  let r = { hd = h; tl = [] } in
	  dst.tl <- inj r;
	  inner r t
    in
    let rec outer dst = function
      | [] -> ()
      | h :: t -> outer (inner dst (f h)) t
    in
    let r = dummy_node () in
    outer r l;
    r.tl

  let map_flatten_i f l = 
    let rec inner dst = function
	[] -> dst
      | h :: t -> 
	  let r = { hd = h; tl = [] } in
	  dst.tl <- inj r;
	  inner r t
    in
    let rec outer i dst = function
      | [] -> ()
      | h :: t -> outer (i + 1) (inner dst (f i h)) t
    in
    let r = dummy_node () in
    outer 0 r l;
    r.tl
end

let map_flatten = ExtList_addition.map_flatten
let map_flatten_i = ExtList_addition.map_flatten_i
let list_iteri = ExtList.List.iteri
let split_at_nth = ExtList.List.split_nth
let list_map = ExtList.List.map
let list_map_ordered = list_map
let list_map2 = ExtList.List.map2
let list_mapi = ExtList.List.mapi
let list_append = ExtList.List.append

let list_repeat = 
  let rec iter acc e = function
    0 -> acc
  | n -> iter (e::acc) e (n - 1)
  in fun e -> iter [] e

let rec string_of_list f sep l =
    String.concat sep (List.map f l)
(*  [] -> ""
  | [e] -> f e
  | h::t -> f h ^ sep ^ string_of_list f sep t*)

let rec pp_list pp_elem pp_sep ppf v = 
  match v with
    [] -> ()
  | [v] -> pp_elem ppf v
  | h::t -> pp_elem ppf h; pp_sep ppf; pp_list pp_elem pp_sep ppf t

(** {2}extensive array *)

module Earray = struct
  type 'a t = {
      mutable size : int; (* max. index *)
      mutable data : 'a option array;
      zero : 'a option
    }

  let empty_with_default ~zero = { size = 0; data = [| |]; zero = Some zero }
  let empty () = { size = 0; data = [| |]; zero = None }

  let copy a = 
    { a with data = Array.copy a.data }

  let mem a i = 
    if i < a.size then
      a.data.(i) <> None
    else
      false

  let get_option a i =
    if i < a.size then
      a.data.(i)
    else 
      a.zero

  let get a i = 
    match get_option a i with
      Some n -> n
    | None -> raise Not_found

  let unset a i =
    if mem a i then
      a.data.(i) <- a.zero

  let set a i v =
    let old_size = Array.length a.data in
    if i >= old_size then begin
      let new_size = old_size * 2 in
      let new_size = if i >= new_size then i + 1 else new_size in
      let new_array = 
	Array.create new_size a.zero in
      Array.blit a.data 0 new_array 0 old_size;
      a.data <- new_array
    end;
    if i >= a.size then a.size <- i + 1;
    a.data.(i) <- Some v

  let incr a i = 
    set a i (get a i + 1)

  let length a = a.size

  let iteri f a =
    for i = 0 to a.size - 1 do
      match a.data.(i) with
	None -> ()
      |	Some x -> f i x
    done

  let map f a = 
    let b = 
      match a.zero with
	Some x -> empty_with_default (f x)
      |	None -> empty ()
    in
    iteri (fun i x -> set b i (f x)) a;
    b

  let mapi f a = 
    let b = empty () in
    iteri (fun i x -> set b i (f i x)) a;
    b

  let filter_i f a = 
    let b = empty ()
    in
    iteri (fun i x -> if f i x then set b i x) a;
    b
end

type 'a earray = 'a Earray.t
let earray_get = Earray.get
let earray_set = Earray.set
let earray_length = Earray.length
let earray_iteri = Earray.iteri
let earray_mem = Earray.mem

(** {2} Other Functions. *)

let identity = fun x -> x
(* leave it as a normal closure, 
   as its uses are almost as a closure, not a direct application. *)

let unwind_protect p b f = 
  let p = p () in
  try
    let r = b p in
    f p; r
  with e ->
    f p; raise e

let sfprintf fmt = 
  let b = Buffer.create 80 in
  Format.kfprintf 
    (fun f ->
      Format.pp_print_flush f ();
      let s = Buffer.contents b in
      Buffer.clear b;
      s)
    (Format.formatter_of_buffer b)
    fmt

let failwith_p fmt = 
  let b = Buffer.create 80 in
  Format.kfprintf 
    (fun f ->
      Format.pp_print_flush f ();
      let s = Buffer.contents b in
      Buffer.clear b;
      failwith s)
    (Format.formatter_of_buffer b)
    fmt

(****** here opening Careful_integer *******)

open Careful_integer

(** {2} More additional float functions *)

let float_of_c99_string s = 
  (** translate C99-style floating representation into float.
      Note: float_of_string supports C99-style hexadecimal representation
      when underlying C library supports it. *)
  let l = String.length s in
  let fail () = failwith "Util.float_of_c99_string" in
  
  if l < 3 then float_of_string s else

  let sign, p = 
    match s.[0], s.[1], s.[2] with
      '0', ('x' | 'X'), _ -> 1.0, 2
    | '+', '0', ('x' | 'X') -> 1.0, 3
    | '-', '0', ('x' | 'X') -> -1.0, 3
    | _ -> 0.0, 0
  in
  if p = 0 then float_of_string s else
  let digits_to_be_shift = ref 0 in
  let digits_read = ref 0 in
  let exp_sign = ref 1 in
  let exp = ref 0 in
  let acc = ref 0.0 in
  let max_digits = 60 in (* not cause floating overflow to Inf *)
  let put_digit c = 
    if !digits_read >= max_digits then
      incr digits_to_be_shift (* no more mantissa to avoid overflow *)
    else
      let s = "0x0" in
      s.[2] <- c;
      acc := !acc *. 16.0 +. float (int_of_string s);
      if !acc <> 0.0 || !digits_read <> 0 then incr digits_read
  in
  let put_exp c p = 
    try
      exp := !exp * 10 + Char.code c - Char.code '0'
    with Integer_overflow ->
      for i = p to l - 1 do
	match s.[i] with
	  '0' .. '9' -> ()
	| _ -> fail ()
      done;
      raise Exit
  in
  let get p = if p >= l then None else Some s.[p] in
  let rec s0 p = (* state just after 0x *)
    match get p with
      None -> fail ()
    | Some ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c) -> 
	put_digit c;
	s1 (p + 1)
    | Some '.' -> s2 (p + 1)
    | Some _ -> fail ()
  and s1 p = (* no dot appeared: 0x1 *)
    match get p with
      None -> ()
    | Some ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c) -> 
	put_digit c;
	s1 (p + 1)
    | Some '.' -> s3 (p + 1)
    | Some ('P' | 'p') -> s4 (p + 1)
    | Some _ -> fail ()
  and s2 p = (* dot appeared first: 0x. *)
    match get p with
      None -> fail ()
    | Some ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c) -> 
	put_digit c;
	decr digits_to_be_shift;
	s3 (p + 1)
    | Some _ -> fail ()
  and s3 p = (* digits and dot appeared: 0x1.0 *)
    match get p with
      None -> fail ()
    | Some ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c) -> 
	put_digit c;
	decr digits_to_be_shift;
	s3 (p + 1)
    | Some ('P' | 'p') ->
	s4 (p + 1)
    | Some _ -> fail ()
  and s4 p = (* P appeared: 0x1.0P *)
    match get p with
      None -> fail ()
    | Some '+' -> s5 (p + 1)
    | Some '-' -> exp_sign := -1; s5 (p + 1)
    | Some ('0' .. '9' as c) -> put_exp c p; s5 (p + 1)
    | Some _ -> fail ()
  and s5 p = (* exp scanning: 0x1.0P1 *)
    match get p with
      None -> ()
    | Some ('0' .. '9' as c) -> put_exp c p; s5 (p + 1)
    | Some _ -> fail ()
  in
  try 
    s0 p (* skip "0x" *);
    let e = (try !digits_to_be_shift * 4 + (!exp * !exp_sign) with Integer_overflow -> raise Exit) in
    sign *. ldexp !acc e
  with
    Exit ->
      (* exponent integer overflow *)
      if !acc = 0.0 || !exp_sign = -1 then 0.0 *. sign
      else sign *. infinity
