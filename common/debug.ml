(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2005-2007. *)

open Util

let max_category = 1000

let debug_flags = Earray.empty_with_default ~zero:0

let set_debug_flags ~category ~level = Earray.set debug_flags category level

let get_debug_flags ~category = Earray.get debug_flags category

let parse_debug_flags s = 
  let assign f t m v = 
    if f < 0 || t > max_category || f > t then
      failwith "parse_debug_flags: bad spec(3)"
    else
      let v = m * v in
      for i = f to t do
	Earray.set debug_flags i v
      done
  in
  let l = String.length s in
  let rec from p v = 
    if p >= l then failwith "parse_debug_flags: bad spec(1)";
    match s.[p] with
      '0' .. '9' as c -> from (p + 1) (v * 10 + (Char.code c - Char.code '0'))
    | '-' -> too (p + 1) v 0
    | '.' | '=' -> value (p + 1) v v 1 0
    | _ -> failwith "parse_debug_flags: bad spec(2-1)"
  and too p f v =
    if p >= l then failwith "parse_debug_flags: bad spec(1)";
    match s.[p] with
      '0' .. '9' as c -> too (p + 1) f (v * 10 + (Char.code c - Char.code '0'))
    | '.' | '=' -> value (p + 1) f v 1 0
    | _ -> failwith "parse_debug_flags: bad spec(2-2)"
  and value p f t m v = 
    if p >= l then assign f t m v else
    match s.[p] with
      '-' when m > 0 && v == 0 -> value (p + 1) f t (-1) v
    | '0' .. '9' as c -> value (p + 1) f t m (v * 10 + (Char.code c - Char.code '0'))
    | ';' | ',' | 'x' | '/' -> assign f t m v; start (p + 1)
    | _ -> failwith "parse_debug_flags: bad spec(2-3)"
  and start p = 
    if p >= l then () else from p 0
  in
  start 0

type state = Normal | Started | Processing

module F =
  (struct 
    let dummy_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
    let ifprintf fmt format = 
      Format.fprintf dummy_formatter format

    (* if there is a standard ifprintf function in Format, the one overrides
       the above less-efficient one. *)
    include Format
   end : sig
     val ifprintf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
     val dummy_formatter : Format.formatter
   end)

let dprintf_gen bool f g format = 
  if bool then begin
    let b = Buffer.create 80 in
    Format.kfprintf
      (fun fmt ->
	Format.pp_print_flush fmt ();
	let c = Buffer.contents b in
	Buffer.reset b;
	f (); prerr_string c; g (); flush stderr)
      (Format.formatter_of_buffer b)
      format
  end else
    F.ifprintf F.dummy_formatter format

let state = ref Normal
let had_message = ref false

(* for general diagnostic messages *)
let dprintf ~category level format = 
  dprintf_gen
    (Earray.get debug_flags category >= level)
    (fun () ->
      if !state <> Normal then
	if not !had_message then begin
	  had_message := true;
	  prerr_string "...\n"
	end)
    prerr_newline
    format

(* for progress messages *)

let category_for_general_progress = ref 2
let level_for_general_progress = ref 1

let dprintf_start ~category format =
  dprintf_gen
    (Earray.get debug_flags category >= !level_for_general_progress ||
     Earray.get debug_flags !category_for_general_progress >= !level_for_general_progress)
    (fun () -> state := Started)
    (fun () -> ())
    format
  
let dprintf_progress ~category format =
  let mlevel = 
    max
      (Earray.get debug_flags category)
      (Earray.get debug_flags !category_for_general_progress)
  in
  if mlevel = !level_for_general_progress then begin
    if !had_message then begin
      prerr_string "...";
      had_message := false
    end;
    state := Processing;
    prerr_string "*";
    flush stderr;
  end;
  dprintf_gen
    (mlevel > !level_for_general_progress)
    (fun () ->
      if !had_message then begin
	prerr_string "...";
	had_message := false
      end;
      if !state == Processing then begin
	prerr_string ", ";
      end else
	state := Processing)
    (fun () -> ())
    format

let dprintf_end ~category format =
  dprintf_gen
    (Earray.get debug_flags category >= !level_for_general_progress ||
     Earray.get debug_flags !category_for_general_progress >= !level_for_general_progress)
    (fun () ->
      if !had_message || !state == Processing then
	prerr_string " ...";
      state := Normal;
      had_message := false)
    prerr_newline
    format

let test () = 
  for i = 0 to Earray.length debug_flags - 1 do
    let x = Earray.get debug_flags i in
    if x <> 0 then
      Printf.eprintf "%8d: %d\n" i x
  done

module Install = 
  functor
  (X : sig val category : int end) ->
    struct
      let category = X.category
      let get_debug_flags () = get_debug_flags ~category
      let dprintf l f = dprintf ~category l f
      let dprintf_start f = dprintf_start ~category f
      let dprintf_progress f = dprintf_progress ~category f
      let dprintf_end f = dprintf_end ~category f
    end
	  
(* usage: instead of open Debug, put

include Debug.Install (struct let category = XXX end)

on the top of each file.
*)
