(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   This file is written by Yutaka Oiwa in 2003-2006. *)

include Parser_loadup

let print_ils x = Format.printf "%a" Ils_formatter.pp_ils x
let print_ilsinit x = Format.printf "%a" Ils_formatter.pp_ils_initializer x
let print_ilc x = Format.printf "%a" Ilc_formatter.pp_ilc x

let step5_1 fname = 
  let genv, t = step4 fname in
  genv, Classify_local.translate_program ~genv t

let step5_2 fname = 
  let genv, t = step5_1 fname in
  genv, Il2_reduce_local_variable.translate_program ~genv t

let step5 fname = 
  let genv, t = step5_2 fname in
  genv, Separate_fatpointer.translate_program ~genv t

let step6 fname = 
  let genv, t = step5 fname in
  genv, Ils_ssa_translate.translate_program t

let step7 fname = 
  let genv, t = step6 fname in
  genv, Dummy_analyzer.f ~genv t

let step8 fname = 
  let genv, t = step7 fname in
  genv, Insert_check.f ~genv t

let step9 fname = 
  let genv, t = step8 fname in
  let t = Translate_to_il3.f ~genv t in
  genv, Il3_fixup.translate_program ~genv t

let step10 fname = 
  let genv, t = step9 fname in
  genv, Il3_optimize.optimize_program_b t

let step11 fname = 
  let genv, t = step10 fname in
  let t = Il3_decompose_ssa.f t in
  genv, Il3_to_ctt.translate_program ~genv t

let step12 fname =
  let genv, t = step11 fname in
  genv, Ctt_to_ptree.convert_program ~genv t

let step13 fname = 
  let genv, t = step12 fname in
  Add_support_funcs.f ~genv t
