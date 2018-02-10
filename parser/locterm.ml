(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

(* generic type for location-attributed terms *)

type location = 
    Located of string * int * int * bool
  | Unlocated

let dummy_location = Unlocated

type 'a t = 
    { locterm_v : 'a;
      locterm_loc : location }

let locput ~loc v = { locterm_v = v; locterm_loc = loc }
let locput_dummy v = locput ~loc:dummy_location v

let locval v = v.locterm_v
let locget v = v.locterm_loc

let locpair v = locget v, locval v

let loccopy ~orig v = locput ~loc:(locget orig) v

let locmap f v = 
  loccopy ~orig:v (f v.locterm_v)

let locmap_l f v = 
  loccopy ~orig:v (f ~loc:(locget v) v.locterm_v)

let locmap_list f l = 
  Util.list_map (locmap f) l

let locmapl_list f l = 
  Util.list_map (locmap_l f) l

let lociter_list f l = 
  List.iter (fun x -> f x.locterm_v) l

(*let locmap_list_glist f l = 
  List.iter
    (fun v -> f ~add:(fun l a -> Glist.put l (loccopy ~orig:v a)))
    l*)

let locmap_flatten f l = 
  Util.map_flatten
    (fun lv ->
      Util.list_map 
	(fun v' -> loccopy ~orig:lv v')
	(f ~loc:lv.locterm_loc lv.locterm_v))
    l
let strof_location = function
    Unlocated -> "(unknown location)"
  | Located(f, l, c, t) -> Printf.sprintf "%s(%d)" f l

(** Pretty formatter *)
open Format
include Debug.Install (struct let category = 13 end)

external is_printable : char -> bool = "caml_is_printable"

let encode_c_string s = 
  let b = Buffer.create (String.length s * 5 / 4 + 1) in (* rough guess *)
  for i = 0 to String.length s - 1 do
    match s.[i] with
      ('"' | '\\') as c ->
        Buffer.add_char b '\\'; Buffer.add_char b c;
    | '\010' ->
        Buffer.add_char b '\\'; Buffer.add_char b 'n';
    | '\t' ->
        Buffer.add_char b '\\'; Buffer.add_char b 't';
    | c ->
        if is_printable c then
          Buffer.add_char b c
        else begin
          let a = Char.code c in
          Buffer.add_char b '\\';
          Buffer.add_char b (Char.chr (48 + a / 64));
          Buffer.add_char b (Char.chr (48 + (a / 8) mod 8));
          Buffer.add_char b (Char.chr (48 + a mod 8))
        end
  done;
  let s = Buffer.contents b in
  Buffer.clear b;
  s

let directive_regexp = Str.regexp "\n#line \\([0-9]+\\) \\(\".*\"\\)\n"

let repeats_gen c = 
  let s = String.make 80 c in
  let rec iter outf n = 
    if n = 0 then ()
    else if n <= 80 then begin
      outf s 0 n
    end else begin
      outf s 0 80; iter outf (n - 80)
    end
  in
  iter

let spaces = repeats_gen ' '
let newlines = repeats_gen '\n'

type state = 
    {
     (* state for input *)
     mutable logical_file : string;
     mutable logical_line : int; (* 1 origin *)
     mutable column : int;       (* 0 origin *)
     mutable continuation_lines : int;  (* normally 0 *)
     mutable clean_line : bool;
     mutable located_region : bool;

     mutable physical_line : int; (* 1 origin *)
   }

let state_new () = { 
  logical_file = ""; logical_line = 1; column = 0;
  clean_line = true; continuation_lines = 0;
  located_region = false;
  physical_line = 1;
}

let newline_state state n = 
  state.column <- 0;
  state.logical_line <- state.logical_line + n + state.continuation_lines;
  state.continuation_lines <- 0;
  state.physical_line <- state.physical_line + n;
  state.clean_line <- true;
  ()

let location_formatter_of_channel ?(fname = "") ch = begin
  let qfname = if fname = "" then 
    "\"<generated code>\""
  else
    "\"" ^ String.escaped fname ^ " <generated code>\""
  in
  let s_in = state_new () in
  let s_out = state_new () in
  let outf = output ch in 
  let fmt = make_formatter outf (fun () -> flush ch) in

  let outf_str s = outf s 0 (String.length s) in

  let out_orig, flush_orig, newline_orig, spaces_orig = 
    pp_get_all_formatter_output_functions fmt () in

  let d s = 
    let dl = get_debug_flags () in
    if dl = 51 then
      outf_str (Printf.sprintf "[[[P%d]]]"
		  s_out.physical_line)
    else if dl > 6 && dl < 50 then
      outf_str (Printf.sprintf "[[[%s%s I%d:%d O%d:%d]]]"
		  s
		  (if s_in.logical_file <> s_in.logical_file then "**" else "")
		  s_in.logical_line s_in.column
		  s_out.logical_line s_out.column);
  in

  let output_str s start l = 
    outf s start l;
    s_out.column <- s_out.column + l;
    s_out.clean_line <- false
  in

  let output_pending_spaces () = 
    spaces outf (s_in.column - s_out.column);
    s_out.column <- s_in.column
  in

  let output_newline () = 
    outf_str "\n";
    newline_state s_out 1;
    if get_debug_flags () = 54 then
      outf_str (Printf.sprintf "[[[AFT_NL %d]]]" s_out.physical_line);
  in

  let output_continuation () = 
    outf_str " \\\n";
    s_out.column <- 0;
    s_out.physical_line <- s_out.physical_line + 1;
    s_out.continuation_lines <- s_out.continuation_lines + 1;
    if get_debug_flags () = 54 then
      outf_str (Printf.sprintf "[[[AFT_CL %d]]]" s_out.physical_line);
  in

  let output_adjust_newlines () = 
    let n = s_in.logical_line - s_out.logical_line - s_out.continuation_lines in
    if n <> 0 then begin
      assert (n > 0);
      newlines outf n;
      newline_state s_out n;
      if get_debug_flags () = 54 then
	outf_str (Printf.sprintf "[[[AFT_NL%d %d]]]" n s_out.physical_line);
    end
  in

  let output_directive () = 
    if not s_out.clean_line then
      output_newline ();

    if s_out.located_region && not s_in.located_region then begin
      s_in.logical_line <- s_out.physical_line + 1;
      s_in.logical_file <- qfname;
    end;

    if get_debug_flags () = 53 then
      outf_str (Printf.sprintf "[[[DO O%s:%d P%d I%s:%d]]]"
		  s_out.logical_file s_out.logical_line s_out.physical_line s_in.logical_file s_in.logical_line);

    outf_str (Printf.sprintf "#line %d %s\n" s_in.logical_line s_in.logical_file);
    newline_state s_out 1;

    s_out.logical_line <- s_in.logical_line;
    s_out.logical_file <- s_in.logical_file;
    s_out.column <- 0;
    s_out.clean_line <- true;
    s_out.located_region <- s_in.located_region;
  in

  let process_directive str start l = 
    let dir = String.sub str start l in
    let b = Str.string_match directive_regexp dir 0 in
    if not b || Str.match_end () <> String.length dir then begin
      Util.failwith_p "panic 179: bad directive found: %S" dir
    end;
    let line, file = int_of_string (Str.matched_group 1 dir), Str.matched_group 2 dir in
    s_in.physical_line <- s_in.physical_line + 2; (* directive contains 2 \n's *)

    if get_debug_flags () = 52 then
      outf_str (Printf.sprintf "[[[DI %s:%d]]]"
		  file line);

    if line = 0 && file = "\"\"" then begin
      s_in.located_region <- false;
    end else begin
      s_in.located_region <- true;
      (* do not reset s_in.column *)
      s_in.logical_line <- int_of_string (Str.matched_group 1 dir);
      s_in.logical_file <- Str.matched_group 2 dir;
      d "D";
    end;
    () (* no output at this time *)
  in

  let process_string str start l = 
    if l = 0 then () else
    d "O>";
    if s_out.logical_file <> s_in.logical_file || s_in.located_region <> s_out.located_region then
      output_directive ()
    else if s_out.logical_line = s_in.logical_line then begin
      if s_in.column >= s_out.column then
	()
      else if s_out.clean_line then
	(* continuation trick only works with non-empty line *)
	output_directive ()
      else begin
	d "C>";
	output_continuation ();
	d "C<";
      end
    end else begin
      if not s_out.clean_line then 
	output_newline ();
      d "O|";
      if s_out.logical_line > s_in.logical_line then
	output_directive ()
      else if s_out.logical_line < s_in.logical_line + 20 then
	output_adjust_newlines ()
      else
	output_directive ()
    end;
    output_pending_spaces ();
    output_str str start l;
    s_in.column <- s_in.column + l;
    d "O<";
  in

  let f_spaces n =
    d "S>";
    s_in.column <- s_in.column + n;
    d "S<";
  in

  let f_newline () = 
    d "N>";
    newline_state s_in (if s_in.located_region then 0 else 1);
    d "N<";
  in

  let f_out str start l = 
    if l >= 7 && String.sub str start 7 = "\n#line " then
      process_directive str start l
    else if l = 1 && str.[start] = ' ' then
      f_spaces 1 (* work around for a fixed space in fprintf formats *)
    else 
      let endpos = start + l in
      let rec iter s p = 
	if p = endpos then 
	  process_string str s (p - s)
	else if str.[p] = '\n' then begin
	  process_string str s (p - s);
	  f_newline ();
	  iter (p + 1) (p + 1)
	end else
	  iter s (p + 1)
      in
      iter start start
  in
  
  pp_set_all_formatter_output_functions fmt f_out flush_orig f_newline f_spaces;
  pp_set_tags fmt true;
  pp_set_mark_tags fmt true;
  pp_set_formatter_tag_functions fmt
    { mark_open_tag = (fun s -> "\n#line " ^ s ^ "\n");
      mark_close_tag = (fun s -> "");
      print_open_tag = (fun s -> ());
      print_close_tag = (fun s -> ());
    };
  fmt
end

let location_formatter = location_formatter_of_channel Pervasives.stdout

let mark_location_open ppf loc = 
  match loc with
    Located(file, line, _, _) ->
      let file_e = encode_c_string file in
      if get_debug_flags () = 61 then
	Format.eprintf "marking: %d \"%s\"" line file_e;
      pp_open_tag ppf (sprintf "%d \"%s\"" line file_e)
  | Unlocated -> 
      pp_open_tag ppf "0 \"\""

let mark_location_close ppf loc = 
  match loc with
    Located(file, line, _, _) ->
      pp_close_tag ppf ()
  | Unlocated -> 
      pp_close_tag ppf ()

