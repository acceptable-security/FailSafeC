(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Generic visitors for IL3 program. *)

(** {6 rewriting visitors} *)

val visit_lv : v_src:('a -> 'b) -> 'a Il.il_lvalue -> 'b Il.il_lvalue

val visit_rexp :
  v_src:('a -> 'b) -> 'a Il3.il3_reducible_exp -> 'b Il3.il3_reducible_exp

val visit_iexp :
  v_src:('a -> 'b) ->
  'a Il3.il3_irreducible_exp -> 'b Il3.il3_irreducible_exp

val visit_instr :
  self:('a Il3.il3 -> 'b Il3.il3) ->
  v_src:('a -> 'b) ->
  v_dest:(Il3.temp_id -> Il3.temp_id) -> 'a Il3.il3 -> 'b Il3.il3

val visit_block :
  v_instr:('a Il3.il3 -> 'b Il3.il3) ->
  v_phi:(Il.temp_id * Il.temp_id array -> Il.temp_id * Il.temp_id array) ->
  'a Il3.il3 Il.il_basic_block_base -> 'b Il3.il3 Il.il_basic_block_base

val visit_function :
  v_block:('a Il3.il3 Il.il_basic_block_base ->
           'b Il3.il3 Il.il_basic_block_base) ->
  v_prologue:(Il0.il0 list -> Il0.il0 list) ->
  ('a, 'c) Il3.il3_function -> ('b, 'c) Il3.il3_function

val visit_gdecl_for_function :
  v_func:(fname:Ctt_abstree.identifier ->
          ('a, 'b) Il3.il3_function -> ('c, 'd) Il3.il3_function) ->
  ('a, 'b) Il3.il3_global_declaration list ->
  ('c, 'd) Il3.il3_global_declaration list

(** {6 scanning-only visitors} *)

val scan_lv : v_src:('a -> unit) -> 'a Il.il_lvalue -> unit

val scan_rexp : v_src:('a -> unit) -> 'a Il3.il3_reducible_exp -> unit

val scan_iexp : v_src:('a -> unit) -> 'a Il3.il3_irreducible_exp -> unit

val scan_instr :
  self:('a Il3.il3 -> unit) ->
  v_src:('a -> unit) -> v_dest:(Il3.temp_id -> 'b) -> 'a Il3.il3 -> unit

val scan_block :
  v_instr:('a Il3.il3 -> unit) ->
  v_phi:(Il.temp_id * Il.temp_id array -> unit) ->
  'a Il3.il3 Il.il_basic_block_base -> unit

val scan_function :
  v_block:('a Il3.il3 Il.il_basic_block_base -> unit) ->
  v_prologue:(('a, 'b) Il3.il3_function -> unit) ->
  ('a, 'b) Il3.il3_function -> unit
