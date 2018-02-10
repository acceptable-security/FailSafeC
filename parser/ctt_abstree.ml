(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2001 -- 2006. *)

open Locterm
open Big_int
open Fsc_config
open Util

type identifier = string

type extension_list = (string * C_abstree.extension_tree) list

type global_storage_class =
    Extern of extension_list
  | Global of extension_list
  | ModuleStatic

and local_storage_class = Auto | Register | LocalStatic | FuncArgs

type union_flag = C_abstree.union_flag = Struct | Union

type builtin_type = 
    Tchar | Tschar | Tuchar 
  | Tshort | Tushort 
  | Tint | Tuint
  | Tlong | Tulong
  | Tlonglong | Tulonglong
  | Tfloat | Tdouble | Tlongdouble

type program = global_declaration list

and global_declaration = global_declaration_desc Locterm.t

and ctt_initializer_desc = 
    CTTinitExp of expr
  | CTTinitList of ctt_initializer list

and ctt_initializer = 
    ctt_initializer_desc Locterm.t

and global_declaration_desc = 
    CTTdeclFunction of global_storage_class * c_type * identifier * identifier list * statement
  | CTTdeclVariable of global_storage_class * c_type * identifier * ctt_initializer option

and variable_declaration = 
    local_storage_class * c_type * identifier * ctt_initializer option

and c_type_desc = 
    Tvoid
  | Tbuiltin of builtin_type
  | Tpointer of c_type
  | Tfunction of c_type list * bool (* varargs flag *) * c_type
  | Tarray of c_type * big_int option
  | Tstruct of struct_id
  | Tabstract of string

and c_type = 
    { ct_const_p : bool;
      ct_volatile_p : bool;
      ct_size : big_int option;
      ct_align : big_int option;
      ct_ty : c_type_desc; }

and struct_id = int

and statement_desc = 
    CTTstmtNull
  | CTTstmtExpr of expr (* None -> CTTstmtNull *)
  | CTTstmtLabeled of identifier * statement
  | CTTstmtCase_Labeled of big_int * statement
  | CTTstmtDefault_Labeled of statement
  | CTTstmtCompound of variable_declaration list * statement list
  | CTTstmtIf of expr * statement * statement option
  | CTTstmtSwitch of expr * statement
  | CTTstmtWhile of expr * statement
  | CTTstmtDoWhile of statement * expr
  | CTTstmtFor of expr option * expr option * expr option * statement
  | CTTstmtGoto of identifier
  | CTTstmtContinue
  | CTTstmtBreak
  | CTTstmtReturn of expr option

and statement = statement_desc Locterm.t

and texpr = { expr_type : c_type; expr_t : expr_desc }
and expr = texpr Locterm.t

and binop = 
    CTTbinTimes | CTTbinDiv
  | CTTbinPlusVV | CTTbinMinusVV 
  | CTTbinPostPlusVV | CTTbinPostMinusVV 
  | CTTbinPlusPV | CTTbinMinusPP | CTTbinMinusPV (* added *)
  | CTTbinPostPlusPV | CTTbinPostMinusPV 
  | CTTbinModulo | CTTbinLshift
  | CTTbinRshift | CTTbinLogAnd | CTTbinLogOr | CTTbinIntAnd | CTTbinIntOr | CTTbinIntXor
  | CTTbinLessThan | CTTbinLessEqual | CTTbinGtrThan | CTTbinGtrEqual
  | CTTbinEqual | CTTbinNotEqual

and unaryop = C_abstree.unaryop = UnaryPlus | UnaryMinus | LogNot | IntNot

and expr_desc = 
  | CTTexpComma of expr * expr
  | CTTexpAssign of expr * expr
  | CTTexpBinAssign of binop * expr * c_type option * expr
	(*
	   fst: kind of the operataion
	   snd: left hand side experssion
	   3rd: promoted type of LHS expression (before operation)
	   4th: right hand side expression

	   * Special treatment for LHS cast is needed.

	   ex1: "(int i; long l) i *= l;" becomes
	     CTTeBA("*", CTTeVar i, Some tLong, CTTeVar l) as tInt
                                    ~~~~~~~~~~                ^^^^
	     1) cast i to long, 2) added with l, 3) cast back to int.
	                  ~~~~                                   ^^^
	   ex2: "(int i; long l) l *= i;" becomes
	     CTTeBA("*", CTTeVar l, None, CTTCoerce(tInt, CTTeVar i)) as tLong
                                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~ RHS cast is simple
	 *)
  | CTTexpConditional of expr * expr * expr
  | CTTexpBinExpr of binop * expr * expr
  | CTTexpCoerce of c_type * expr
  | CTTexpUnaryExpr of unaryop * expr
(*  | CTTexpPreInc of expr
  | CTTexpPreDec of expr
  | CTTexpPostInc of expr
  | CTTexpPostDec of expr *)
  | CTTexpAddress of expr
  | CTTexpPtrDeref of expr
  | CTTexpInvoke of expr * expr list
  | CTTexpField of expr * identifier
  | CTTexpConstant of c_constants
  | CTTexpVar of identifier * c_type

(* removed *)
(*  | PexpSizeOfExpr of expr --> SizeOfType --> integer
  | CTTexpSizeOfType of c_type --> integer
  | PexpArrayRef of expr * expr --> CTTtrDeref + CTTtrAdd
  | PexpPtrField of expr * identifier --> CTTtrDeref + Field
*)

and c_constants = 
  | CTTconstNull
  | CTTconstInteger of big_int
  | CTTconstFloat of float
  | CTTconstString of string
  (* Fail-Safe C extensions *)
  | CTTconstTypeInfo of c_type
  | CTTconstAbstract of string

type struct_field_normal = 
    {
     sf_id : identifier;
     sf_type : c_type;
     sf_size : big_int;
   } 

type struct_field_bitfields = 
    {
     s_bf_size : big_int;
     s_bf_fields : (identifier option * c_type * int * (int * int)) list
       (* name, type, bitwidth, (start_byte, start_offset) *)
   } 

type struct_field = 
    NormalField of struct_field_normal
  | BitField of struct_field_bitfields

type struct_ofsinfo =
    StrOfsNormal of big_int
  | StrOfsBitfield of big_int * int * (int * int)

type struct_desc = 
    { 
      str_union_p : union_flag;
      str_size : big_int option;
      str_align : big_int option;
      str_fields : (big_int * struct_field) list;
      str_fields_byname : (identifier * (c_type * struct_ofsinfo)) list;
      str_assignable : bool;
      str_extension : (identifier * C_abstree.extension_tree) list;
      str_loc: Locterm.location;
    } 

type global_binding = {
    gbind_storage_class : global_storage_class;
    gbind_is_initialized : bool;
    gbind_type : c_type;
  }

type environment = {
    module_hash : string;
    struct_table : struct_desc earray;
    struct_name_table : (identifier * struct_id) list;
    global_declarations : (identifier * global_binding) list
  }

let size_of_builtin_type = 
  function
      Tchar | Tuchar | Tschar ->  sizeof_char
    | Tshort | Tushort ->  sizeof_short
    | Tint | Tuint ->  sizeof_int
    | Tlong | Tulong ->  sizeof_long
    | Tlonglong | Tulonglong ->  sizeof_longlong
    | Tfloat ->  sizeof_float
    | Tdouble ->  sizeof_double
    | Tlongdouble -> sizeof_longdouble

let align_of_builtin_type = size_of_builtin_type

let rec size_align_of_type_desc ?s_table = function
  | Tvoid -> None, None
  | Tbuiltin bt -> let s = Some (big_int_of_int (size_of_builtin_type bt)) in s, s
  | Tpointer t -> let s = Some (big_int_of_int sizeof_pointer) in s, s
  | Tabstract _ -> None, None
  | Tfunction _ -> None, None
  | Tstruct id -> begin
      match s_table with
	Some struct_table -> begin
	  try
	    let sd = Earray.get struct_table id in
	    sd.str_size, sd.str_align
	  with
	    Not_found ->
	      failwith ("panic: struct id " ^ string_of_int id ^ 
			" not found in struct table")
	end
      | None ->
	  failwith "size_of_type_desc: no environment given"
  end
  | Tarray(t, numopt) -> begin
      (*let sz1, al = size_align_of_type_desc ?env t.ct_ty in*)
      let sz1, al = t.ct_size, t.ct_align in
      let sz = match sz1 with
      | None -> None
      | Some s ->
	  Option.map (mult_big_int s) numopt
      in
      sz, al
  end

let make_c_type ?(const = false) ?(volatile = false) ?s_table ty = 
  let sz, al = size_align_of_type_desc ?s_table ty in
  { ct_const_p = const; ct_volatile_p = volatile; 
    ct_size = sz; ct_align = al;
    ct_ty = ty }

let update_c_type ?s_table ty = 
  if ty.ct_size = None || ty.ct_align = None then
    let sz, al = size_align_of_type_desc ?s_table ty.ct_ty in
    { ct_const_p = ty.ct_const_p; ct_volatile_p = ty.ct_volatile_p; 
      ct_size = sz; ct_align = al;
      ct_ty = ty.ct_ty }
  else
    ty

let type_char = make_c_type (Tbuiltin Tchar)
let type_signed_char = make_c_type (Tbuiltin Tschar)
let type_unsigned_char = make_c_type (Tbuiltin Tuchar)
let type_short = make_c_type (Tbuiltin Tshort)
let type_unsigned_short = make_c_type (Tbuiltin Tushort)
let type_int = make_c_type (Tbuiltin Tint)
let type_unsigned_int = make_c_type (Tbuiltin Tuint)
let type_long = make_c_type (Tbuiltin Tlong)
let type_unsigned_long = make_c_type (Tbuiltin Tulong)
let type_long_long = make_c_type (Tbuiltin Tlonglong)
let type_unsigned_long_long = make_c_type (Tbuiltin Tulonglong)
let type_float = make_c_type (Tbuiltin Tfloat)
let type_double = make_c_type (Tbuiltin Tdouble)
let type_long_double = make_c_type (Tbuiltin Tlongdouble)

let type_void = make_c_type Tvoid

(* Fail-Safe C extension *)
let type_typeinfo_ptr = make_c_type (Tpointer type_void)

let type_boolean = type_int
let builtintype_ptrdiff_t = Tlong
let type_ptrdiff_t = type_long
let type_size_t = type_unsigned_long

let type_char_array s = 
  make_c_type(Tarray(type_char, Some (big_int_of_int s)))

let make_texpr_desc e ty = 
  { expr_t = e; expr_type = ty }
let make_expr e ty ~loc = 
  Locterm.locput ~loc (make_texpr_desc e ty)

let is_signed_builtin_type = function
    Tschar | Tshort | Tint | Tlong | Tlonglong -> true
  | Tuchar | Tushort | Tuint | Tulong | Tulonglong -> false
  | Tchar -> Fsc_config.char_is_signed
  | Tfloat | Tdouble | Tlongdouble -> invalid_arg "is_signed_builtin_type"

let get_max_value_of_type t = 
  match t.ct_ty with
  | Tbuiltin(Tfloat | Tdouble | Tlongdouble) ->
      failwith "get_max_value_of_type: floating not supported"
  | Tbuiltin bt ->
      let s = size_of_builtin_type bt in
      let is_signed = is_signed_builtin_type bt in
      let bits = s * bits_of_byte - (if is_signed then 1 else 0) in
      sub_big_int (Big_int.power_int_positive_int 2 bits) unit_big_int
  | _ ->
      failwith "get_max_value_of_type: not builtin numeric type"

let get_struct_desc ~genv id = 
  try 
    Earray.get genv.struct_table id
  with
    Not_found -> assert false

let reset_struct_size ~genv t = 
  match t.ct_ty with
    Tstruct id ->
      let sd = get_struct_desc ~genv id in
      {t with ct_size = sd.str_size; ct_align = sd.str_align }
  | _ -> t

let size_of_type ~genv t = t.ct_size

let align_of_type ~genv t = t.ct_align

let reconstruct_gdecl_table ~genv t = 
  let gdecls = 
    list_map
      (fun gd ->
	match locval gd with
	  CTTdeclFunction(gs,ct,id,_args,t) -> 
	    (id, { gbind_storage_class = gs;
		   gbind_is_initialized = true;
		   gbind_type = ct })
	| CTTdeclVariable(gs,ct,id,initopt) ->
	    (id, { gbind_storage_class = gs;
		   gbind_is_initialized = (initopt <> None);
		   gbind_type = ct }))
      t
  in
  { genv with global_declarations = gdecls }
