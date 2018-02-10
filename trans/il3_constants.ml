(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. *)

(** Il3_consants: various constants for generating code from IL3. *)

open Util
open Big_int_infix

open Il0
open Il3
open Ctt_abstree

open Transutil
open Translate_to_il3
open Record_globalnames

module Constants = struct
  let make_func_lv name argtype rettype =
    Il.ILlvVar(name, make_c_type (Tfunction(argtype, false, rettype)))
      
  (* immobile functions returning values *)
  let lv_alloc_varargs =
    make_func_lv "fsc_alloc_varargs" [type_size_t] type_base_t
  let lv_check_ofs_ok =
    make_func_lv "is_offset_ok" [type_base_t; type_ofs_t] type_boolean
  let lv_get_boundary =
    make_func_lv "get_boundary" [type_base_t] type_boundary_t
  let lv_check_boundary_ofs_ok =
    make_func_lv "is_boundary_offset_ok" [type_boundary_t; type_ofs_t] type_boolean
  let lv_invoke_genfunc = 
    make_func_lv "fsc_invoke_func_with_varargs" [type_base_t; type_base_t; type_ofs_t; type_base_t; type_ofs_t] type_dvalue
  let lv_alloc_heapvar =
    make_func_lv "fsc_alloc_heapvar" [type_typeinfo_ptr; type_size_t] type_void_ptr

  (* immobile operations *)
  let lv_fsc_abort =
    make_func_lv "fsc_abort" [type_base_t; type_fsc_error] type_void
  let lv_dealloc_varargs_finished =
    make_func_lv "fsc_dealloc_varargs_finished" [type_base_t] type_void
  let lv_dealloc_heapvar =
    make_func_lv "fsc_dealloc_heapvar" [type_base_t] type_void
  let lv_put_varargs =
    make_func_lv "fsc_put_varargs" [type_base_t; type_size_t; type_value] type_void
  let lv_put_varargs_2 =
    make_func_lv "fsc_put_varargs_2" [type_base_t; type_size_t; type_dvalue] type_void
  let lv_finish_varargs =
    make_func_lv "fsc_finish_varargs" [type_base_t] type_void
  let lv_exit_stack_unwind_area =
    make_func_lv "exit_stack_unwind_area" [make_c_type (Tpointer type_char)] type_void
  let lv_alloc_valtrampoline =
    make_func_lv "fsc_alloc_valtrampoline" [type_typeinfo_ptr] type_void_ptr
  let lv_enter_stack_unwind_area = 
    make_func_lv "enter_stack_unwind_area" 
      [make_c_type (Tpointer type_stack_frame);
       make_c_type (Tpointer type_char);
       make_c_type (Tpointer type_void_ptr);
     ]
      type_void

  (* readers/writers *)
  let lv_read_byte =
    make_func_lv "read_byte" [type_base_t; type_ofs_t] type_byte
  let lv_read_hword =
    make_func_lv "read_hword" [type_base_t; type_ofs_t] type_hword
  let lv_read_word =
    make_func_lv "read_word" [type_base_t; type_ofs_t] type_value
  let lv_read_dword =
    make_func_lv "read_dword" [type_base_t; type_ofs_t] type_dvalue
  let lv_write_byte =
    make_func_lv "write_byte" [type_base_t; type_ofs_t; type_byte; type_typeinfo_ptr] type_void
  let lv_write_hword =
    make_func_lv "write_hword" [type_base_t; type_ofs_t; type_hword; type_typeinfo_ptr] type_void
  let lv_write_word =
    make_func_lv "write_word" [type_base_t; type_ofs_t; type_value; type_typeinfo_ptr] type_void
  let lv_write_dword =
    make_func_lv "write_dword" [type_base_t; type_ofs_t; type_dvalue; type_typeinfo_ptr] type_void

  let create_lv_struct_rw ~genv id = 
    let sid = encoded_name_of_struct ~genv id in
    let et = make_c_type (Tabstract ("struct struct_" ^ sid)) (* TODO: code dup *) in
    make_func_lv ("read_struct_" ^ sid) [type_base_t; type_ofs_t] et,
    make_func_lv ("write_struct_" ^ sid) [type_base_t; type_ofs_t; et] type_void
      (* TODO: be careful; args number different! should be fixed. *)

  (* reducible functions: can be moved to any location, can be removed if not used. *)

  (* cons / car / cdr *)
  let lv_cons_val =
    make_func_lv "value_of_base_vaddr" [type_base_t; type_vaddr_t] type_value
      
  let lv_cons_dval =
    make_func_lv "dvalue_of_base_vaddr"
      [type_base_t; type_unsigned_long_long] type_dvalue
      
  let lv_cons_ptrval =
    make_func_lv "ptrvalue_of_base_ofs" [type_base_t; type_ofs_t] type_ptrvalue

  let lv_base_of_value =
    make_func_lv "base_of_value" [type_value] type_base_t
      
  let lv_base_of_dvalue =
    make_func_lv "base_of_dvalue" [type_dvalue] type_base_t
      
  let lv_base_of_ptrvalue =
    make_func_lv "base_of_ptrvalue" [type_ptrvalue] type_base_t

  let lv_vaddr_of_value =
    make_func_lv "vaddr_of_value" [type_value] type_vaddr_t

  let lv_vaddr_of_dvalue =
    make_func_lv "vaddr_of_dvalue" [type_dvalue] type_dword
    
  let lv_ofs_of_ptrvalue =
    make_func_lv "ofs_of_ptrvalue" [type_ptrvalue] type_ofs_t

  (* converters *)
  let lv_value_of_ptr = 
    make_func_lv "value_of_base_ofs" [type_base_t; type_ofs_t] type_value

  let lv_value_of_ptrvalue =
    make_func_lv "value_of_ptrvalue" [type_ptrvalue] type_value

  let lv_ptrvalue_of_value ~genv ct = 
      let id = "ptrvalue_of_value_" ^ encoded_name_of_type_genv ~genv ct in
      make_func_lv id [type_value] type_ptrvalue

  let lv_nullexpand_value =
    make_func_lv "value_of_int" [type_word] type_value
    
  let lv_nullexpand_dvalue =
    make_func_lv "dvalue_of_dword" [type_dword] type_dvalue

  let lv_ofs_of_value =
    make_func_lv "ofs_of_value" [type_value] type_ofs_t

  let lv_vaddr_of_base_ofs =
    make_func_lv "vaddr_of_base_ofs" [type_base_t; type_ofs_t] type_vaddr_t

  let lv_ofs_of_base_vaddr =
    make_func_lv "ofs_of_base_vaddr" [type_base_t; type_vaddr_t] type_ofs_t

  let lv_double_of_dword =
    assert (Fsc_config.sizeof_double = Fsc_config.sizeof_longlong);
    make_func_lv "double_of_dword" [type_dword] type_double
      
  let lv_float_of_word =
    assert (Fsc_config.sizeof_float = Fsc_config.sizeof_long);
    make_func_lv "float_of_word" [type_word] type_float
      
  let lv_dword_of_double =
    make_func_lv "dword_of_double" [type_double] type_dword
      
  let lv_word_of_float =
    make_func_lv "word_of_float" [type_float] type_word
      
  let lv_double_of_dvalue =
    assert (Fsc_config.sizeof_double = Fsc_config.sizeof_longlong);
    make_func_lv "double_of_dvalue" [type_dvalue] type_double
      
  let lv_float_of_value =
    assert (Fsc_config.sizeof_float = Fsc_config.sizeof_long);
    make_func_lv "float_of_value" [type_value] type_float
      
  let lv_dvalue_of_double =
    make_func_lv "dvalue_of_double" [type_double] type_dvalue
      
  let lv_value_of_float =
    make_func_lv "value_of_float" [type_float] type_value
      
  let lv_get_realoffset_ptr = 
    make_func_lv "get_realoffset_P" [type_base_t; type_ofs_t] type_ptrvalue_ptr
      
  let lv_get_realoffset ~genv ct = 
    let p = parse_type_genv ~genv ct in
    let resulttype = make_c_type (Tpointer p.packed_t) in
    make_func_lv 
      ("get_realoffset_" ^ encoded_name_of_type_genv ~genv ct)
      [ type_base_t; type_ofs_t ]
      resulttype

  let lv_get_funcptr = 
    make_func_lv "get_realoffset_PF" [type_base_t] type_void_ptr
      
  let lv_value_of_dvalue = 
    make_func_lv "value_of_dvalue" [type_dvalue] type_value
      
  let lv_dvalue_of_value = 
    make_func_lv "dvalue_of_value" [type_value] type_dvalue

  (* other functions *)
      
  let lv_base_remove_castflag =
    make_func_lv "base_remove_castflag" [type_base_t] type_base_t

  let lv_base_put_castflag =
    make_func_lv "base_put_castflag" [type_base_t] type_base_t

  let lv_check_cast =
    make_func_lv "is_cast" [type_base_t] type_boolean

  let lv_set_base_castflag ~genv t =
    Record_globalnames.require_name (GNbasecast t);
    assert (is_pointer t);
    make_func_lv ("set_base_castflag_" ^ encoded_name_of_type_genv ~genv t)
      [type_base_t] type_base_t

  let lv_add_fat_pointer ~genv t =
    require_name (GNaddptr t);
    assert (is_pointer t);
    make_func_lv ("add_fat_pointer_" ^ encoded_name_of_type_genv ~genv t)
      [type_base_t] type_ptrvalue

end

let rettype_of_lv lv = 
  let t = match lv with
    Il.ILlvVar(id, t) -> t | _ -> assert false in
  let rt = match t.ct_ty with
    Tfunction(at,vt,rt) -> rt | _ -> assert false in
  rt

open Constants

let lv_immobile_functions = function
    Alloc_heapvar -> lv_alloc_heapvar
  | Alloc_varargs -> lv_alloc_varargs
  | Alloc_valtempoline -> lv_alloc_valtrampoline
  | Is_offset_ok -> lv_check_ofs_ok
  | Get_boundary -> lv_get_boundary
  | Is_boundary_offset_ok -> lv_check_boundary_ofs_ok
  | Invoke_generic_func -> lv_invoke_genfunc

let lv_immobile_operations = function
    Dealloc_heapvar -> lv_dealloc_heapvar
  | Dealloc_varargs_finished -> lv_dealloc_varargs_finished
  | Put_varargs -> lv_put_varargs
  | Put_varargs_2 -> lv_put_varargs_2
  | Finish_varargs -> lv_finish_varargs
  | Enter_stack_unwind_area -> lv_enter_stack_unwind_area
  | Exit_stack_unwind_area -> lv_exit_stack_unwind_area

let lv_reducible_functions ~genv = function
    IL3pr_cons Value -> lv_cons_val
  | IL3pr_cons Dvalue -> lv_cons_dval
  | IL3pr_cons PtrValue -> lv_cons_ptrval

  | IL3pr_car Value -> lv_base_of_value
  | IL3pr_car Dvalue -> lv_base_of_dvalue
  | IL3pr_car PtrValue -> lv_base_of_ptrvalue

  | IL3pr_cdr Value -> lv_vaddr_of_value
  | IL3pr_cdr Dvalue -> lv_vaddr_of_dvalue
  | IL3pr_cdr PtrValue -> lv_ofs_of_ptrvalue

  | IL3pr_convert Value_of_base_ofs -> lv_value_of_ptr
  | IL3pr_convert Value_of_ptrvalue -> lv_value_of_ptrvalue
  | IL3pr_convert Ptrvalue_of_value(ct) -> lv_ptrvalue_of_value ~genv ct
  | IL3pr_convert Value_of_int -> lv_nullexpand_value
  | IL3pr_convert Dvalue_of_dword -> lv_nullexpand_dvalue

  | IL3pr_convert Value_of_dvalue -> lv_value_of_dvalue
  | IL3pr_convert Dvalue_of_value -> lv_dvalue_of_value

  | IL3pr_convert Dvalue_of_double -> lv_dvalue_of_double
  | IL3pr_convert Dword_of_double -> lv_dword_of_double
  | IL3pr_convert Double_of_dvalue -> lv_double_of_dvalue
  | IL3pr_convert Double_of_dword -> lv_double_of_dword
  | IL3pr_convert Value_of_float -> lv_value_of_float
  | IL3pr_convert Word_of_float -> lv_word_of_float
  | IL3pr_convert Float_of_value -> lv_float_of_value
  | IL3pr_convert Float_of_word -> lv_float_of_word

  | IL3pr_convert Ofs_of_value -> lv_ofs_of_value
  | IL3pr_convert Vaddr_of_base_ofs -> lv_vaddr_of_base_ofs
  | IL3pr_convert Ofs_of_base_vaddr -> lv_ofs_of_base_vaddr

  | IL3pr_misc Base_remove_castflag -> lv_base_remove_castflag
  | IL3pr_misc Base_put_castflag -> lv_base_put_castflag
  | IL3pr_misc Set_base_castflag ct -> lv_set_base_castflag ~genv ct
  | IL3pr_misc Add_fat_pointer ct -> lv_add_fat_pointer ~genv ct
  | IL3pr_misc Is_cast -> lv_check_cast
  | IL3pr_misc Get_realoffset_pointer -> lv_get_realoffset_ptr
  | IL3pr_misc Get_realoffset_funcptr -> lv_get_funcptr
  | IL3pr_misc Get_realoffset(ct) -> lv_get_realoffset ~genv ct

let lv_reader_helper ~genv = function
    Byte -> lv_read_byte
  | Hword -> lv_read_hword
  | Word -> lv_read_word
  | Dword -> lv_read_dword 
  | RWstruct id -> fst (create_lv_struct_rw ~genv id)
  | RWnone -> assert false

let lv_writer_helper ~genv = function
    Byte -> lv_write_byte
  | Hword -> lv_write_hword
  | Word -> lv_write_word
  | Dword -> lv_write_dword 
  | RWstruct id -> snd (create_lv_struct_rw ~genv id)
  | RWnone -> assert false

let lv_abortfunc = 
  lv_fsc_abort

let rettype_immobile_functions ~genv f = 
  rettype_of_lv (lv_immobile_functions f)

let rettype_reducible_functions ~genv f = 
  rettype_of_lv (lv_reducible_functions ~genv f)

let rettype_reader_helper ~genv f = 
  rettype_of_lv (lv_reader_helper ~genv f)
