(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   This file is written by Yutaka Oiwa in 2003-2006. *)

open Ctt_abstree
open Big_int
let print_bigint x = Format.printf "B%s" (string_of_big_int x)
let print_c_type x = Format.printf "%a" Ctt_formatter.pp_c_type x
let print_expr x = Format.printf "%a" Ctt_formatter.pp_expr x
let print_mexpr x = Format.printf "%a" Ctt_formatter.pp_mexpr x
let print_global_declaration x = Format.printf "%a" Ctt_formatter.pp_global_declaration x
let print_global_declarations x = List.iter (fun x -> Format.printf "%a@." Ctt_formatter.pp_global_declaration x) x
let print_il0 x = Format.printf "%a" Il0.pp_il0 x
(*let print_il1 x = Format.printf "%s" (Flatten_statement.strof_il1stmt x)*)
let print_int_set x = Format.printf "[%s]" (String.concat ";" (Util.list_map string_of_int (Set_list.to_list x)))
let print_c_program x = Format.printf "%a" C_pp.pp_print_program x

let print_untyped_expr x = C_pp.pp_print_expression x
let print_untyped_statement x = C_pp.pp_print_statement x

let open_preprocess_chan fname = 
  let cmdline = Printf.sprintf ("cpp %s %s") Fsc_config.cpp_options fname in
  let chan = Unix.open_process_in cmdline
  in
  chan

let read fname =
  C_parserhelper.reset fname;
  Util.unwind_protect
    (fun () -> open_preprocess_chan fname)
    (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      C_parser.translation_unit C_lexer.token lexbuf)
    (fun chan -> close_in chan)

let step0 fname = read fname

let step1 fname = 
  C_typing.parse_global_declarations (step0 fname)

let step2 fname = 
  let genv, t = step1 fname in
  let t = Expunge_localstatic.reduce_program ~genv t in
  let genv, t = Ctt_pad_structures.translate_program genv t in
  let genv, t = Ctt_reduce_mdarray.reduce_program ~genv t in
  let genv, t = Ctt_reduce_unions.translate_program ~genv t in
  Ctt_add_tentative_initializer.reduce_program ~genv t

let step2_1 fname = 
  let genv, t = step2 fname in
  genv, Cttm_expunge_assignment.translate_program t

let step3 fname = 
  let genv, t = step2_1 fname in
  genv, Separate_side_effect.translate_program_to_il0 ~genv t

let step3_1 fname = 
  let genv, t = step3 fname in
  Il0_translate_bitfield.translate_program ~genv t

let step4 fname = 
  let genv, t = step3_1 fname in
  genv, Flatten_statement.translate_program ~genv t

let () = 
  C_lexer.extension_keywords := ["__fsc_attribute__"];
  C_typing.translate_struct_by_extension_hook := (fun ~loc ~ext def -> def);
  C_typing.translate_external_extension_hook := (fun ~loc ~ext ~sclass ~ty ~is_initialized -> []);
  C_typing.fill_zero_initializer := false
