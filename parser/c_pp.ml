(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2002 -- 2006. *)

(* pretty printer for C abstree *)

open Format
open Locterm
open C_abstree

(* generic data structure: list, option *)

let pp_print_list ppf ~elem_pp ~sep_pp = 
  let rec loop ppf = 
    function
	[] -> ()
      | [e] -> elem_pp ppf e;
      | h::tl ->
	  fprintf ppf "@[<hov>%a@]%t@,%a"
	    elem_pp h
	    sep_pp
	    loop tl
  in
  loop ppf

let pp_print_option ~f ppf = 
  function
      None -> ()
    | Some e -> 
	(f ppf e : unit)

let pp_print_option_with_sep ~f ~sep ppf = 
  function
      None -> ()
    | Some e -> 
	(f ppf e : unit);
	sep ppf

let pp_print_sep_space ppf = 
  fprintf ppf "@ "

let pp_print_sep_none ppf = ()

let pp_print_sep_newline ppf = fprintf ppf "@\n"

let pp_print_sep_comma ppf = 
  fprintf ppf ",@ "

(* general extension *)
let default_string_of_extension_tree id t = 
  let rec iter = function
    | Eplist l -> "(" ^ String.concat ", " (Util.list_map iter l) ^ ")"
    | Elist l -> String.concat " " (Util.list_map iter l)
    | Eident i -> i
    | Eint i -> i
    | Estring s -> "\"" ^ s ^ "\""
  in
  id ^ " " ^ iter t

let string_of_extension_tree_hook = ref default_string_of_extension_tree

let string_of_extension_tree id t =
  (!string_of_extension_tree_hook) id t

let pp_print_extension_tree ppf (id, t) = fprintf ppf "%s" (string_of_extension_tree id t)

let pp_print_extension_list_with_postspc ppf extensions = 
  if extensions = [] then () else begin
    pp_print_sep_space ppf;
    (pp_print_list ~elem_pp:pp_print_extension_tree ~sep_pp:pp_print_sep_space) ppf extensions
  end

(* C atoms: identifier, binop, etc. *)

let pp_print_identifier = pp_print_string

let pp_print_binop ppf b =
  let t = function
      PbinTimes -> "*"
    | PbinDiv -> "/"
    | PbinPlus -> "+"
    | PbinMinus -> "-"
    | PbinModulo -> "%"
    | PbinLshift -> "<<"
    | PbinRshift -> ">>"
    | PbinLogAnd -> "&&"
    | PbinLogOr -> "||"
    | PbinIntAnd -> "&"
    | PbinIntOr -> "|"
    | PbinIntXor -> "^"
    | PbinLessThan -> "<"
    | PbinLessEqual -> "<="
    | PbinGtrThan -> ">"
    | PbinGtrEqual -> ">="
    | PbinEqual -> "=="
    | PbinNotEqual -> "!="
  in
  pp_print_string ppf (t b)

let pp_print_unaryop ppf b =
  let t = function
    | UnaryPlus -> "+"
    | UnaryMinus -> "-"
    | LogNot -> "!"
    | IntNot -> "~"
  in
  pp_print_string ppf (t b)

let pp_print_constant ppf = function 
    PconstInteger x -> pp_print_string ppf x
  | PconstFloat x -> pp_print_string ppf x
  | PconstChar x -> pp_print_string ppf x
  | PconstString [] -> pp_print_string ppf "\"\""
  | PconstString l -> List.iter 
	(fun x -> pp_print_string ppf ("\"" ^ x ^ "\" ")) l
         (* TODO : formatting *)

let string_of_storage_class = function
    Extern -> "extern"
  | Auto -> "auto"
  | Static -> "static"
  | Register -> "register"
  | Typedef -> "typedef"
  | Inline -> "inline"

let pp_print_storageclass ppf s =
  pp_print_string ppf (string_of_storage_class s)

let pp_print_typequal ppf = function
    Const -> pp_print_string ppf "const"
  | Volatile -> pp_print_string ppf "volatile"

let pp_print_struct_or_union ppf = function
    Struct -> pp_print_string ppf "struct"
  | Union -> pp_print_string ppf "union"

let pp_print_builtin_typespec ppf = function
    Void -> pp_print_string ppf "void"
  | Char -> pp_print_string ppf "char"
  | Short -> pp_print_string ppf "short"
  | Int -> pp_print_string ppf "int"
  | Long -> pp_print_string ppf "long"
  | Float -> pp_print_string ppf "float"
  | Double -> pp_print_string ppf "double"
  | Signed -> pp_print_string ppf "signed"
  | Unsigned -> pp_print_string ppf "unsigned"
  
(* huge letrec starts here ... :-( *)
(* typespec -> enum -> expression -> sizeof(type) -> typespec *)

let rec pp_print_typespec ppf = function
    PtypespecBuiltin p -> pp_print_builtin_typespec ppf p
  | PtypespecStruct(union_p, id_opt, sbody_opt, extensions, _loc) ->
      fprintf ppf "@[<hov>%a %a%a%a@]"
	pp_print_struct_or_union union_p
	pp_print_extension_list_with_postspc extensions
	(pp_print_option_with_sep ~f:pp_print_identifier ~sep:pp_print_sep_space) id_opt
	(pp_print_option ~f:pp_print_struct_declaration_list) sbody_opt
  | PtypespecEnumByName id ->
      fprintf ppf "enum %a" pp_print_identifier id
  | PtypespecEnumByDef(Some id, e, _loc) ->
      fprintf ppf "enum %a@ %a"
	pp_print_identifier id
	pp_print_enum_list e
  | PtypespecEnumByDef(None, e, _loc) ->
      fprintf ppf "enum@ %a"
	pp_print_enum_list e
  | PtypespecAlias id ->
      pp_print_identifier ppf id

and pp_print_struct_declarator ppf = function
    PstructDeclNormal dc ->
      fprintf ppf "@[%a@]"
	pp_print_declarator dc
  | PstructDeclBitfield(None,ex) ->
      fprintf ppf "@[int : @[%a@]@]"
	pp_print_constant_expression ex
  | PstructDeclBitfield(Some dc, ex) ->
      fprintf ppf "@[%a : @[%a@]@]"
	pp_print_declarator dc
	pp_print_constant_expression ex

and pp_print_struct_declaration ppf (PstructDecl(sq,dc)) =
  fprintf ppf "@[<hov>%a@ @[<hov>%a@]@];"
    pp_print_sqlist sq
    (pp_print_list ~elem_pp:pp_print_struct_declarator ~sep_pp:pp_print_sep_comma)
    dc

and pp_print_struct_declaration_list ppf s = 
  fprintf ppf "{@\n  @[<v>%a@]}"
    (pp_print_list ~elem_pp:pp_print_struct_declaration ~sep_pp:pp_print_sep_none) s

and pp_print_sq ppf = function
    TypeSpec s -> pp_print_typespec ppf s
  | TypeQualifier q -> pp_print_typequal ppf q
  | StorageClass sc -> pp_print_storageclass ppf sc
  | ExtendedDeclSpec(id, t) -> pp_print_string ppf (string_of_extension_tree id t)

and pp_print_sqlist ppf l =
  fprintf ppf "@[<hv 2>%a@]" 
    (pp_print_list ~elem_pp:pp_print_sq ~sep_pp:pp_print_sep_space) l

and pp_print_enum_entry ppf = function
    (id, None) -> pp_print_identifier ppf id
  | (id, Some expr) ->
      fprintf ppf "@[%a =@ @,%a@]"
	pp_print_identifier id
	pp_print_expression expr

and pp_print_enum_list ppf s = 
  fprintf ppf "{@\n @[<hv 2>%a@] }"
    (pp_print_list ~elem_pp:pp_print_enum_entry ~sep_pp:pp_print_sep_comma)
    s

and pp_print_declarator ppf dc =
  let rec loop lv ppf dc = 
    match dc with
      (* level 0: *)
      PdeclPointer(q,d) ->
	if lv > 0 then
	  fprintf ppf "(@[%a@])"
	    (loop 0) dc
	else 
	  if d = PdeclAnonymous then
	    fprintf ppf "*%a"
	      (pp_print_list ~elem_pp:pp_print_typequal ~sep_pp:pp_print_sep_space)
	      q
	  else
	    fprintf ppf "*%a@ %a"
	      (pp_print_list ~elem_pp:pp_print_typequal ~sep_pp:pp_print_sep_space)
	      q
	      (loop 0) d
    | (* level 10: direct *)
      PdeclIdent x ->
	pp_print_identifier ppf x
    | PdeclAnonymous ->
	()
    | PdeclArray(x,e) ->
	fprintf ppf "%a[@[%a@]]"
	  (loop 10) x
	  (pp_print_option ~f:pp_print_expression) e
    | PdeclFuncType(x,argtypes) ->
	fprintf ppf "%a(@[%a@])"
	  (loop 10) x
	  (pp_print_list ~elem_pp:pp_print_paramdecl ~sep_pp:pp_print_sep_comma) argtypes
    | PdeclFuncIdent(x,arglist) ->
	fprintf ppf "%a(@[%a@])"
	  (loop 10) x
	  (pp_print_list ~elem_pp:pp_print_identifier ~sep_pp:pp_print_sep_comma) 
	  arglist
  in
  fprintf ppf "@[<hov 2>%a@]" (loop 0) dc

and pp_print_typename ppf = function
    (Ptypename(sq,PdeclAnonymous)) ->
      fprintf ppf "%a" pp_print_sqlist sq
  | (Ptypename(sq,dc)) ->
      fprintf ppf "%a@ %a" pp_print_sqlist sq pp_print_declarator dc

and pp_print_paramdecl ppf = function
    PpdeclAbstract(sq, PdeclAnonymous) -> 
      fprintf ppf "@[%a@]" pp_print_sqlist sq
  | PpdeclConcrete(sq, dc)
  | PpdeclAbstract(sq, dc) ->
      fprintf ppf "@[%a@ %a@]" pp_print_sqlist sq pp_print_declarator dc
  | PpdeclVariant ->
      fprintf ppf "..."

and pp_print_expression_iter lv ppf e : unit = 
  let print_with_paren e = 
    fprintf ppf "@[<hov 1>(%a)@]" (pp_print_expression_iter 0 : formatter -> expr -> unit) e
  in
  fprintf ppf "@[";
  let loc = locget e in
  let pp_string_with_loc ppf s =
    Locterm.mark_location_open ppf loc;
    pp_print_string ppf s;
    Locterm.mark_location_open ppf loc
  in
  begin match locval e with
    (* level 0: comma *)
    PexpComma(l,r) -> (* LASSOC *)
      if lv > 0 then print_with_paren e
      else
	fprintf ppf "%a%a@ %a"
	  (pp_print_expression_iter 0) l
	  pp_string_with_loc ","
	  (pp_print_expression_iter 10) r
  | (* level 10: assignment *)
    PexpAssign(l,r) ->
      if lv > 10 then print_with_paren e
      else
	fprintf ppf "%a %a@;<1 2>@[<hov>%a@]"
	  (pp_print_expression_iter 20) l
	  pp_string_with_loc "="
	  (pp_print_expression_iter 10) r
  | PexpBinAssign(binop,l,r) ->
      if lv > 10 then print_with_paren e
      else
	fprintf ppf "%a %a%a=%a@;<1 2>@[<hov>%a@]"
	  (pp_print_expression_iter 20) l 
	  mark_location_open loc
	  pp_print_binop binop
	  mark_location_close loc
	  (pp_print_expression_iter 10) r
  | (* level 20: conditional *)
    PexpConditional (e1, e2, e3) ->
      if lv > 20 then print_with_paren e
      else
	fprintf ppf "%a %a@;<1 2>%a %a@;<1 2>%a"
	  (pp_print_expression_iter 30) e1
	  pp_string_with_loc "?"
	  (pp_print_expression_iter 30) e2
	  pp_string_with_loc ":"
	  (pp_print_expression_iter 30) e3
  | (* level 30: || *)
    PexpBinExpr(PbinLogOr,l,r) ->
      if lv > 30 then print_with_paren e
      else
	fprintf ppf "%a@ %a %a"
	  (pp_print_expression_iter 30) l
	  pp_string_with_loc "||"
	  (pp_print_expression_iter 40) r
  | (* level 40: && *)
    PexpBinExpr(PbinLogAnd,l,r) ->
      if lv > 30 then print_with_paren e (* a || b && c parened *)
      else
	fprintf ppf "%a@ %a %a"
	  (pp_print_expression_iter 40) l
	  pp_string_with_loc "&&"
	  (pp_print_expression_iter 50) r
  | (* level 50: | *)
    PexpBinExpr(PbinIntOr,l,r) ->
      if lv > 50 then print_with_paren e
      else
	fprintf ppf "%a@ %a %a"
	  (pp_print_expression_iter 140(*50*)) l
	  pp_string_with_loc "|"
	  (pp_print_expression_iter 140(*60*)) r
  | (* level 60: ^ *)
    PexpBinExpr(PbinIntXor,l,r) ->
      if lv > 60 then print_with_paren e
      else
	fprintf ppf "%a@ %a %a"
	  (pp_print_expression_iter 140(*60*)) l
	  pp_string_with_loc "^"
	  (pp_print_expression_iter 140(*70*)) r
  | (* level 70: ^ *)
    PexpBinExpr(PbinIntAnd,l,r) ->
      if lv > 70 then print_with_paren e
      else
	fprintf ppf "%a@ %a %a"
	  (pp_print_expression_iter 140(*70*)) l
	  pp_string_with_loc "&"
	  (pp_print_expression_iter 140(*80*)) r
  | (* level 80: ==, !- *)
    PexpBinExpr((PbinEqual | PbinNotEqual) as b,l,r) ->
      if lv > 80 then print_with_paren e
      else
	fprintf ppf "%a@ %a%a%a %a"
	  (pp_print_expression_iter 90) l
	  mark_location_open loc
	  pp_print_binop b
	  mark_location_close loc
	  (pp_print_expression_iter 90) r
  | (* level 90: compare *)
    PexpBinExpr
      ((PbinLessEqual | PbinLessThan | PbinGtrEqual | PbinGtrThan) as b,
       l,r
      ) ->
	if lv > 90 then print_with_paren e
	else
	  fprintf ppf "%a@ %a%a%a %a"
	    (pp_print_expression_iter 100) l
	    mark_location_open loc
	    pp_print_binop b
	    mark_location_close loc
	    (pp_print_expression_iter 100) r
  | (* level 100: shift *)
    PexpBinExpr((PbinLshift | PbinRshift) as b,l,r) ->
      if lv > 100 then print_with_paren e
      else
	fprintf ppf "%a@ %a%a%a %a"
	  (pp_print_expression_iter 140(*110*)) l
	  mark_location_open loc
	  pp_print_binop b
	  mark_location_close loc
	  (pp_print_expression_iter 140(*110*)) r
  | (* level 110: + -  *)
    PexpBinExpr((PbinPlus | PbinMinus) as b,l,r) ->
      if lv > 110 then print_with_paren e
      else
	fprintf ppf "@[%a@]@ %a%a%a @[%a@]"
	  (pp_print_expression_iter 110) l
	  mark_location_open loc
	  pp_print_binop b
	  mark_location_close loc
	  (pp_print_expression_iter 120) r
  | (* level 120: * / % *)
    PexpBinExpr((PbinTimes | PbinDiv | PbinModulo) as b,l,r) ->
      if lv > 120 then print_with_paren e
      else
	fprintf ppf "%a@ %a%a%a %a"
	  (pp_print_expression_iter 120) l
	  mark_location_open loc
	  pp_print_binop b
	  mark_location_close loc
	  (pp_print_expression_iter 130) r
  | (* level 130: cast *)
    PexpCast(t,c) ->
      if lv > 130 then print_with_paren e
      else
	fprintf ppf "%a(%a)@;<0 2>@[%a@]%a"
	  mark_location_open loc
	  pp_print_typename t 
	  mark_location_close loc
	  (pp_print_expression_iter 130) c
  | (* level 140: unary prefix *)
    PexpPreInc e1 ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%a++%a%a" 
	  mark_location_open loc
	  (pp_print_expression_iter 140) e1
	  mark_location_close loc
  | PexpPreDec e1 ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%a--%a%a"
	  mark_location_open loc
	  (pp_print_expression_iter 140) e1
	  mark_location_close loc
  | PexpAddress e1 ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%a&%a%a"
	  mark_location_open loc
	  (pp_print_expression_iter 140) e1
	  mark_location_close loc
  | PexpPtrDeref e1 ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%a*%a%a"
	  mark_location_open loc
	  (pp_print_expression_iter 140) e1
	  mark_location_close loc
  | PexpUnaryExpr(uop,e1) ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%a%a%a%a"
	  mark_location_open loc
	  pp_print_unaryop uop
	  (pp_print_expression_iter 140) e1
	  mark_location_close loc
  | PexpSizeOfExpr e1 ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%asizeof %a%a"
	  mark_location_open loc
	  (pp_print_expression_iter 140) e1
	  mark_location_close loc
  | PexpSizeOfType t ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%asizeof(%a)%a"
	  mark_location_open loc
	  pp_print_typename t
	  mark_location_close loc
  (* Fail-Safe C extensions *)
  | PexpTypeOfType t ->
      if lv > 140 then print_with_paren e
      else
	fprintf ppf "%a__typeof(%a)%a"
	  mark_location_open loc
	  pp_print_typename t
	  mark_location_close loc
  | (* level 150: postfix *)
    PexpArrayRef(l,i) ->
      if lv > 150 then print_with_paren e
      else
	fprintf ppf "%a@;<0 2>%a%a%a"
	  (pp_print_expression_iter 150) l
	  pp_string_with_loc "["
	  (pp_print_expression_iter 0) i
	  pp_string_with_loc "]"
  | PexpInvoke(l,args) ->
      if lv > 150 then print_with_paren e
      else
	fprintf ppf "%a(@;<0 2>@[%a@]%a" 
           (* not to break before open parenthesis, because CPP confuses when #line is inserted there *)
	  (pp_print_expression_iter 150) l
	  (pp_print_argument_list) args
	  pp_string_with_loc ")"
  | PexpField(l,id) ->
      if lv > 150 then print_with_paren e
      else
	fprintf ppf "%a%a.@;<0 2>%a%a"
	  (pp_print_expression_iter 150) l
	  mark_location_open loc
	  pp_print_identifier id
	  mark_location_close loc
  | PexpPtrField(l,id) ->
      if lv > 150 then print_with_paren e
      else
	fprintf ppf "%a%a@;<0 2>%a"
	  (pp_print_expression_iter 150) l
	  pp_string_with_loc "->"
	  pp_print_identifier id
  | PexpPostInc(l) ->
      if lv > 150 then print_with_paren e
      else
	fprintf ppf "%a%a"
	  (pp_print_expression_iter 150) l
	  pp_string_with_loc "++"
  | PexpPostDec(l) ->
      if lv > 150 then print_with_paren e
      else
	fprintf ppf "%a%a"
	  (pp_print_expression_iter 150) l
	  pp_string_with_loc "--"
  | (* level 160: primary *)
    PexpVar id ->
      (* mark_location_open ppf loc; *) (* Var and Constaant will not be located, as its error is better to be reported in outer context *)
      pp_print_identifier ppf id;
      (* mark_location_close ppf loc *)
  | PexpConstant con ->
      (* mark_location_open ppf loc; *)
      pp_print_constant ppf con;
      (* mark_location_close ppf loc *)
  end;
  fprintf ppf "@]"

and pp_print_argument_list ppf args =
  pp_print_list ~elem_pp:(pp_print_expression_iter 10) ~sep_pp:(pp_print_sep_comma) ppf args

and pp_print_expression ppf e = 
  fprintf ppf "@[<hv>%a@]" (pp_print_expression_iter 0) e

and pp_print_constant_expression ppf e = 
  fprintf ppf "@[<hv>%a@]" (pp_print_expression_iter 10) e

let rec pp_print_initializer ppf i = 
  let loc = locget i in
  Locterm.mark_location_open ppf loc;
  begin
    match locval i with
      PinitExp e -> pp_print_expression ppf e
    | PinitList l ->
	fprintf ppf "{@[<2>%a@,@]}"
	  (pp_print_list ~elem_pp:pp_print_initializer ~sep_pp:pp_print_sep_comma)
	l;
  end;
  Locterm.mark_location_close ppf loc

let pp_print_initdeclarator ppf = function
    PinitDecl(dc,None) ->
      pp_print_declarator ppf dc
  | PinitDecl(dc,Some i) ->
      fprintf ppf "@[<hov 2>%a =@ %a@]"
	pp_print_declarator dc
	pp_print_initializer i

let rec pp_print_statement ppf st = 
  Locterm.mark_location_open ppf (locget st);
  begin match locval st with
      PstmtLabeled(l,st) ->
	fprintf ppf "@\n%a:@\n%a"
	  pp_print_identifier l
	  pp_print_statement st
    | PstmtCase_Labeled(e,st) ->
	fprintf ppf "@\ncase %a:@\n%a"
	  pp_print_expression e
	  pp_print_statement st
    | PstmtDefault_Labeled st ->
	fprintf ppf "@\ndefault:@\n%a"
	  pp_print_statement st
    | PstmtExpr(None) ->
	fprintf ppf ";"
    | PstmtExpr(Some e) ->
	fprintf ppf "%a;"
	  pp_print_expression e
    | PstmtCompound([],stmts) ->
	fprintf ppf "{@\n  @[<v>%a@]@\n}"
	  (pp_print_list
	     ~elem_pp:pp_print_statement
	     ~sep_pp:pp_print_sep_none)
	  stmts
    | PstmtCompound(decls,stmts) ->
	fprintf ppf "{@\n  @[<v>%a@\n@\n%a@]@\n}"
	  (pp_print_list
	     ~elem_pp:pp_print_declaration
	     ~sep_pp:pp_print_sep_none)
	  decls
	  (pp_print_list
	     ~elem_pp:pp_print_statement
	     ~sep_pp:pp_print_sep_none)
	  stmts
    | PstmtIf(e1,s2,None) ->
	fprintf ppf "@[if (%a)@;<1 2>%a@]"
	  pp_print_expression e1
	  pp_print_statement s2
    | PstmtIf(e1,s2,Some s3) ->
	fprintf ppf "@[if (%a)@;<1 2>%a@]@ @[else@;<1 2>%a@]"
	pp_print_expression e1
	pp_print_statement s2
	pp_print_statement s3
    | PstmtSwitch(e1,s2) ->
	fprintf ppf "@[switch (%a)@;<1 2>%a@]"
	  pp_print_expression e1
	  pp_print_statement s2
    | PstmtWhile(e1,s2) ->	
	fprintf ppf "@[while (%a)@;<1 2>%a@]"
	  pp_print_expression e1
	  pp_print_statement s2
    | PstmtFor(e1,e2,e3,s4) ->	
	fprintf ppf "@[for (%a;@ %a;@ %a)@;<1 2>%a@]"
	  (pp_print_option ~f:pp_print_expression) e1
	  (pp_print_option ~f:pp_print_expression) e2
	  (pp_print_option ~f:pp_print_expression) e3
	  pp_print_statement s4
    | PstmtDoWhile(s1,e2) ->
	fprintf ppf "@[@[do@;<1 4>%a@]@;<1 2>@[while (%a);@]@]"
	  pp_print_statement s1
	  pp_print_expression e2
    | PstmtGoto(l) ->
	fprintf ppf "goto %a;"
	  pp_print_identifier l
    | PstmtContinue ->
	fprintf ppf "continue;"
    | PstmtBreak ->
	fprintf ppf "break;"
    | PstmtReturn(None) ->
	fprintf ppf "return;"
    | PstmtReturn(Some e) ->
	fprintf ppf "return %a;"
	  pp_print_expression e
  end;
  Locterm.mark_location_close ppf (locget st)

and pp_print_declaration ppf decl = 
  Locterm.mark_location_open ppf (locget decl);
  begin match locval decl with
    PdeclFunction(sq,dc,[],fbody) ->
      fprintf ppf "@[<v>@[<hv>%a@;<1 2>%a@]@\n%a@]"
	pp_print_sqlist sq
	pp_print_declarator dc
	pp_print_statement fbody
  | PdeclFunction(sq,dc,argdecls,fbody) ->
      fprintf ppf "@[<v>@[<hv>%a@;<1 2>%a@]@\n  @[<v 2>%a@]@\n%a@]"
	pp_print_sqlist sq
	pp_print_declarator dc
	(pp_print_list ~elem_pp:pp_print_declaration ~sep_pp:pp_print_sep_none)
	argdecls
	pp_print_statement fbody
  | PdeclVariable(sq,initdecls) ->
      fprintf ppf
	"@[<hov>%a@ %a@];"
	pp_print_sqlist sq
	(pp_print_list ~elem_pp:pp_print_initdeclarator ~sep_pp:pp_print_sep_comma)
	initdecls
  | PdeclPragmatic s ->
      fprintf ppf "%s@\n" s
  end;
  Locterm.mark_location_close ppf (locget decl)

let pp_print_program ppf (p : program) = 
  fprintf ppf "@[<v>%a@]@."
    (pp_print_list ~elem_pp:pp_print_declaration ~sep_pp:pp_print_sep_none(*pp_print_sep_newline*))
    p
