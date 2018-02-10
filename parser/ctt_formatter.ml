(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2006. *)

open Util
open Big_int_infix
open Locterm
open Ctt_abstree
open Cttm_abstree
open Format

(** helper functions. *)

let encode_c_string = Locterm.encode_c_string
(* moved because the same functionality is required in Locterm. *)

let make_pp f = fun ppf v -> pp_print_string ppf (f v)
let pp_list = C_pp.pp_print_list

(** to_string functions for common data types *)

let strof_builtin_type = function
    Tchar -> "char"
  | Tschar -> "signed char"
  | Tuchar -> "unsigned char"
  | Tshort -> "short"
  | Tushort -> "unsigned short"
  | Tint -> "int"
  | Tuint -> "unsigned int"
  | Tlong -> "long"
  | Tulong -> "unsigned long"
  | Tlonglong -> "long long"
  | Tulonglong -> "unsigned long long"
  | Tfloat -> "float"
  | Tdouble -> "double"
  | Tlongdouble -> "long double"

let pp_builtin_type = make_pp strof_builtin_type

let strof_c_type_qualifiers t = 
  match t.ct_volatile_p, t.ct_const_p with
    true, true -> "volatile const "
  | true, false -> "volatile "
  | false, true -> "const "
  | false, false -> ""

let pp_type_qualifiers = make_pp strof_c_type_qualifiers

let rec pp_c_type_long ?(name="") ?(argnames=[]) ?genv () ppf t = 
  let out str sp s tl = fprintf ppf "%s%s%t%t" str sp s tl in
  let rec iter t sp lv s tl = 
    match t.ct_ty with
    | Tstruct(id) ->
	let name = 
	  match genv with
	    None -> ""
	  | Some genv ->
	      let rec iter = function
		  [] -> ""
		| (nam, v) :: t ->
		    if v = id then nam else iter t
	      in
	      iter genv.struct_name_table
	in
	let name = 
	  match name with
	    "" -> "struct <S" ^ string_of_int id ^ ">"
	  | s ->  "struct " ^ s ^ "<S" ^ string_of_int id ^ ">"
	in
	out (strof_c_type_qualifiers t ^ name) sp s tl
    | Tabstract str ->
	out ("<abstract>" ^ str) sp s tl
    | Tbuiltin(bt) ->
	out (strof_c_type_qualifiers t ^ strof_builtin_type bt) sp s tl
    | Tvoid -> out (strof_c_type_qualifiers t ^ "void") sp s tl
    | Tpointer t1 ->
	iter t1 " " 1 (fun ppf -> fprintf ppf "*%s" (strof_c_type_qualifiers t); s ppf) tl
    | Tfunction(at, vt, rt) when Debug.get_debug_flags ~category:12 = 10 ->
	(* alternative form *)
	fprintf ppf "[@[";
	if at = [] then
	  fprintf ppf "%s%s"
	    (if vt then "..." else "void")
	    (if argnames <> [] then "<some args... why?>" else "")
	else begin
	  if argnames = [] then
	    Util.pp_list (pp_c_type_long ?genv ()) C_pp.pp_print_sep_comma ppf at
	  else
	    let rec inner ss ts = match ss, ts with
		[], [] -> ()
	      | [s], [t] -> 
		  fprintf ppf "%s : %a" s (pp_c_type_long ?genv ()) t
	      | (s::ss), (t::ts) ->
		  fprintf ppf "%s : %a,@ " s (pp_c_type_long ?genv ()) t;
		  inner ss ts
	      | _ -> assert false
	    in
	    inner argnames at
	end;
	fprintf ppf " ->@;<1 2>";
	pp_c_type_long ?genv () ppf rt;
	fprintf ppf "@]]%s%t%t" sp s tl
    | _ when lv >= 1 ->
	iter t " " 0 (fun ppf -> fprintf ppf "(%t%t)" s tl) ignore
    | Tarray(t,None) ->
	iter t sp lv s (fun ppf -> tl ppf; pp_print_string ppf "[]")
    | Tarray(t,Some sz) ->
	iter t sp lv s (fun ppf -> fprintf ppf "%t[%s]" tl (Big_int.string_of_big_int sz))
    | Tfunction([],false,rt) ->
	iter rt sp lv s (fun ppf -> fprintf ppf "%t(void)" tl)
    | Tfunction([],true,rt) ->
	iter rt sp lv s (fun ppf -> fprintf ppf "%t(...)" tl)
    | Tfunction(at,vt,rt) ->
	iter rt sp lv s 
	  (fun ppf ->
	    tl ppf;
	    fprintf ppf "(@[";
	    if List.length argnames = List.length at then
	      let rec inner ss ts = match ss, ts with
		[], [] -> ()
	      | [n], [t] -> 
		  pp_c_type_long ~name:n ?genv () ppf t
	      | (n::ns), (t::ts) ->
		  pp_c_type_long ~name:n ?genv () ppf t;
		  fprintf ppf ",@ ";
		  inner ns ts
	      | _ -> assert false
	      in
	      inner argnames at
	    else
	      Util.pp_list (pp_c_type_long ?name:None ?genv ()) C_pp.pp_print_sep_comma ppf at;
	    fprintf ppf "%s@])" (if vt then ", ..." else ""))
  in
  iter t (if name = "" then "" else " ") 0 (fun ppf -> pp_print_string ppf name) ignore

let pp_c_type = pp_c_type_long ()

let strof_union_flag = function
    Struct -> "struct"
  | Union -> "union"

let pp_union_flag = make_pp strof_union_flag

let strof_binop = function
    CTTbinTimes -> "*"
  | CTTbinDiv -> "/"
  | CTTbinPlusVV -> "+"
  | CTTbinMinusVV -> "-"
  | CTTbinPostPlusVV -> "(post+)"
  | CTTbinPostMinusVV -> "(post-)"
  | CTTbinPlusPV -> ".+"
  | CTTbinMinusPP -> ".-."
  | CTTbinMinusPV -> ".-"
  | CTTbinPostPlusPV -> "(post.+)"
  | CTTbinPostMinusPV -> "(post.-)"
  | CTTbinModulo -> "%"
  | CTTbinLshift -> "<<"
  | CTTbinRshift -> ">>"
  | CTTbinLogAnd -> "&&"
  | CTTbinLogOr -> "||"
  | CTTbinIntAnd -> "&"
  | CTTbinIntOr -> "|"
  | CTTbinIntXor -> "^"
  | CTTbinLessThan -> "<"
  | CTTbinLessEqual -> "<="
  | CTTbinGtrThan -> ">"
  | CTTbinGtrEqual -> ">="
  | CTTbinEqual -> "=="
  | CTTbinNotEqual -> "!="

let pp_binop = make_pp strof_binop

let strof_unaryop = function
    UnaryPlus -> "+"
  | UnaryMinus -> "-"
  | LogNot -> "!"
  | IntNot -> "~"

let pp_unaryop = make_pp strof_unaryop

let pp_ctt_constant ppf = function
    CTTconstNull -> pp_print_string ppf "NULL"
  | CTTconstInteger bi -> pp_print_string ppf (string_of_big_int bi)
  | CTTconstFloat f -> pp_print_string ppf (string_of_float f)
  | CTTconstString s -> fprintf ppf "\"%s\"" (encode_c_string s)
  | CTTconstTypeInfo t -> fprintf ppf "__typeinfo(%a)" pp_c_type t
  | CTTconstAbstract s -> fprintf ppf "<abstract>%s" s

let strof_local_storage_class = function
    Auto -> "auto"
  | Register -> "register"
  | LocalStatic -> "static"
  | FuncArgs -> ""

let pp_local_storage_class = make_pp strof_local_storage_class

let strof_global_storage_class = function
    Extern [] -> "extern"
  | Extern _l -> "extern *extended*"
  | Global [] -> "global"
  | Global _l -> "global *extended*"
  | ModuleStatic -> "static"

let pp_global_storage_class = make_pp strof_global_storage_class (* TODO: extension tree *)

(** CTT datatypes. *)

let pp_expr_desc, pp_expr, pp_expr_with_type = 
  let rec ed_p ~st ppf e = 
    match e.expr_t with
      CTTexpComma (e1,e2) ->
	fprintf ppf "%a,@ %a" (s ~st) e1 (s ~st) e2
    | CTTexpAssign (e1,e2) ->
	fprintf ppf "%a =@ %a" (s ~st) e1 (s ~st) e2
    | CTTexpBinAssign (bop,e1,_,e2) ->
	fprintf ppf "%a %a=@ %a" (s ~st) e1 pp_binop bop (s ~st) e2
    | CTTexpConditional (e1,e2,e3) ->
	fprintf ppf "%a ?@ %a :@ %a" (s ~st) e1 (s ~st) e2 (s ~st) e2
    | CTTexpBinExpr (bop,e1,e2) ->
	fprintf ppf "%a %a@ %a" (s ~st) e1 pp_binop bop (s ~st) e2
    | CTTexpCoerce (ct,e1) ->
	fprintf ppf "(%a)@;<0 2>%a" pp_c_type ct (s ~st) e1
    | CTTexpUnaryExpr (uop,e1) ->
	fprintf ppf "%a%a" pp_unaryop uop (s ~st) e1
    | CTTexpAddress e1 ->
	fprintf ppf "&%a" (s ~st) e1
    | CTTexpPtrDeref e1 ->
	fprintf ppf "*%a" (s ~st) e1
    | CTTexpInvoke (e1,es) ->
	fprintf ppf "%a@;<0 2>(@[<hov>%a@])" (s ~st) e1 (pp_list ~elem_pp:(s ~st) ~sep_pp:C_pp.pp_print_sep_comma) es
    | CTTexpField (e1,id) ->
	fprintf ppf "%a.@,%s" (s ~st) e1 id
    | CTTexpConstant ct ->
	pp_ctt_constant ppf ct
    | CTTexpVar (id, ct) ->
	pp_print_string ppf id
  and ed ~st ppf e = 
    if st then
      match e.expr_t with
	CTTexpConditional _
      | CTTexpAddress _
      | CTTexpInvoke _
      | CTTexpField _
      | CTTexpConstant _
      | CTTexpVar _
	  -> fprintf ppf "(@[%a@ [%a]@])" (ed_p ~st) e pp_c_type e.expr_type
      | _ -> fprintf ppf "(@[%a@])" (ed_p ~st) e
    else
      fprintf ppf "(%a)" (ed_p ~st) e
  and s ~st ppf e = 
    ed ~st ppf (locval e)
  in
  ed,
  s ~st:false,
  s ~st:true

let rec pp_ctt_initalizer ppf i =
  match locval i with
    CTTinitExp e -> pp_expr_with_type ppf e
  | CTTinitList l -> fprintf ppf "{@[<hov 2>%a@]}" (pp_list ~elem_pp:pp_ctt_initalizer ~sep_pp:C_pp.pp_print_sep_comma) l

let pp_local_variable_declaration ppf (sc,ct,id,initopt) = 
  match initopt with
    None ->
      fprintf ppf "@[<hov 2>%a@ %a;@]" pp_local_storage_class sc (pp_c_type_long ~name:id ()) ct
  | Some i ->
      fprintf ppf "@[<hov 2>@[<hov 2>%a@ %a@] =@ %a;@]" pp_local_storage_class sc (pp_c_type_long ~name:id ()) ct pp_ctt_initalizer i

let pp_statement ppf st = 
  let rec sd ppf = function
      CTTstmtNull -> pp_print_string ppf ";"
    | CTTstmtExpr e -> fprintf ppf "%a;" pp_expr_with_type e
    | CTTstmtLabeled (id,st) -> fprintf ppf "%s:@ %a" id s st
    | CTTstmtCase_Labeled (n,st) -> fprintf ppf "case %s:@ %a" (string_of_big_int n) s st
    | CTTstmtDefault_Labeled st -> fprintf ppf "default:@ %a"s st
    | CTTstmtCompound(decls,stmts) ->
	fprintf ppf "{@\n  @[<v>%a@,%a@]@\n}"
	  (pp_list ~elem_pp:pp_local_variable_declaration ~sep_pp:C_pp.pp_print_sep_none) decls
	  (pp_list ~elem_pp:s ~sep_pp:C_pp.pp_print_sep_none) stmts
    | CTTstmtIf(e,st,None) ->
	fprintf ppf "@[<2>if (%a) then@ %a@]" pp_expr_with_type e s st
    | CTTstmtIf(e,st1,Some st2) ->
	fprintf ppf "@[<2>if (%a) then@ %a@ else %a@]" pp_expr_with_type e s st1 s st2
    | CTTstmtSwitch (e,st) ->
	fprintf ppf "@[<2>switch (%a)@ %a@]" pp_expr_with_type e s st
    | CTTstmtWhile (e,st) ->
	fprintf ppf "@[<2>while (%a)@ %a@]" pp_expr_with_type e s st
    | CTTstmtDoWhile (st,e) ->
	fprintf ppf "@[<2>do %a@ while (%a);]" s st pp_expr_with_type e
    | CTTstmtFor(e1o,e2o,e3o,st) ->
	let pp_eopt ppf = function None -> () | Some e -> pp_expr_with_type ppf e in
	fprintf ppf "@[<2>for (@[%a;@ %a;@ %a@])@ %a"
	  pp_eopt e1o pp_eopt e2o pp_eopt e3o s st
    | CTTstmtGoto id -> fprintf ppf "goto %s;" id
    | CTTstmtContinue -> pp_print_string ppf "continue;"
    | CTTstmtBreak -> pp_print_string ppf "break;"
    | CTTstmtReturn(None) -> pp_print_string ppf "return;"
    | CTTstmtReturn(Some e) -> fprintf ppf "return %a;" pp_expr_with_type e
  and s ppf st = sd ppf (locval st)
  in fprintf ppf "@[<v>%a@]" s st

let pp_global_declaration_desc ppf = function
  CTTdeclVariable(gs,ct,id,initopt) -> begin
    match initopt with
      None ->
	fprintf ppf
	  "@[<v>@[<2>%a %a;@]@\n@]" pp_global_storage_class gs (pp_c_type_long ~name:id ()) ct
    | Some i ->
	fprintf ppf
	  "@[<v>@[@[<2>%a %a@]@ = %a@\n@]" pp_global_storage_class gs (pp_c_type_long ~name:id ()) ct pp_ctt_initalizer i
  end
  | CTTdeclFunction(gs,ct,id,ids,st) ->
      fprintf ppf
	"@[<v>@[<2>%a %a@]@\n%a@\n@]@." pp_global_storage_class gs (pp_c_type_long ~name:id ~argnames:ids ()) ct pp_statement st

let pp_global_declaration ppf d = pp_global_declaration_desc ppf (locval d)

let pp_global_declarations ppf d = 
  List.iter (pp_global_declaration ppf) d

(** CTTM datatypes. *)

let pp_mexpr_desc, pp_mexpr, pp_cttm_lv,
  pp_fields_suffix = 
  let rec ed_p ~st ppf e = 
    match e.mexpr_t with
      CTTMexpComma(e1,e2) -> (*true*)
	fprintf ppf "%a,@ %a" (s ~st) e1 (s ~st) e2
    | CTTMexpAddress(lv,fs) -> (*true*)
	fprintf ppf "&%a%a" (mo ~st) lv (f ~st) fs
    | CTTMexpRead(lv,fs) ->
	fprintf ppf "%a%a" (mo ~st) lv (f ~st) fs
    | CTTMexpWrite(lv,fs,None,e1) ->
	fprintf ppf "%a%a :=@ %a" (mo ~st) lv (f ~st) fs (s ~st) e1
    | CTTMexpWrite(lv,fs,Some (binop,_),e1) ->
	fprintf ppf "%a%a :%a=@ %a" (mo ~st) lv (f ~st) fs pp_binop binop (s ~st) e1
    | CTTMexpConditional(e1,e2,e3) ->
	fprintf ppf "%a ?@ %a :@ %a" (s ~st) e1 (s ~st) e2 (s ~st) e2
    | CTTMexpBinExpr(bop,e1,e2) ->
	fprintf ppf "%a %a@ %a" (s ~st) e1 pp_binop bop (s ~st) e2
    | CTTMexpCoerce(ct,e1) ->
	fprintf ppf "(%a)@;<0 2>%a" pp_c_type ct (s ~st) e1
    | CTTMexpUnaryExpr(uop,e1) ->
	fprintf ppf "%a%a" pp_unaryop uop (s ~st) e1
    | CTTMexpInvoke(lv,es) ->
	fprintf ppf "%a@;<0 2>(@[<hov>%a@])" (mo ~st) lv (pp_list ~elem_pp:(s ~st) ~sep_pp:C_pp.pp_print_sep_comma) es
    | CTTMexpConstant(ct) ->
	pp_ctt_constant ppf ct
  and ed ~st ppf e = 
    if st then
      match e.mexpr_t with
	CTTMexpConditional _
      | CTTMexpAddress _
      | CTTMexpRead _
      | CTTMexpWrite _
      | CTTMexpInvoke _
      | CTTMexpConstant _
	-> fprintf ppf "(@[%a@ [%a]@])" (ed_p ~st) e pp_c_type e.mexpr_type
      | _ -> fprintf ppf "(@[%a@])" (ed_p ~st) e
    else
      fprintf ppf "(@[%a@])" (ed_p ~st) e
  and s ~st ppf e = 
    ed ~st ppf (locval e)
  and mo ~st ppf = function
      CTTMlvPtr e ->
	fprintf ppf "*%a" (s ~st) e
    | CTTMlvVar(id,ct) ->
	if st then 
	  fprintf ppf "%s [%a]" id pp_c_type ct
	else pp_print_string ppf id
    | CTTMlvRvalue e ->
	fprintf ppf "(@[%a@])" (s ~st) e
  and f ~st ppf l = 
    List.iter
	 (fun (f,ct) -> 
	   if st then
	     fprintf ppf ".@;<2>@[%s [%a]@]"  f  pp_c_type ct
	   else 
	     fprintf ppf ".@;<2>%s" f)
      l
  in
  (fun ppf e -> ed ~st:true ppf e),
  (fun ppf e -> s ~st:true ppf e),
  (fun ppf e -> mo ~st:true ppf e),
  (fun ppf e -> f ~st:true ppf e)

let rec pp_cttm_initalizer ppf init =
  match locval init with
    CTTMinitExp e -> pp_mexpr ppf e
  | CTTMinitList l -> fprintf ppf "{@[<hov 2>%a@]}" (pp_list ~elem_pp:pp_cttm_initalizer ~sep_pp:C_pp.pp_print_sep_comma) l

