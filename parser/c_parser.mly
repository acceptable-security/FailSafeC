%{(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2001 -- 2006. *)

(** The parser for C programs. *)

open Locterm
open C_abstree
open C_parserhelper

let current_token_string = ref None

let parameter_scope = ref None
let location () = 
  let bpos = Parsing.symbol_start () in
  lookup_location bpos

let make_exp t = locput ~loc:(location ()) t
let make_stmt t = locput ~loc:(location ()) t
let make_decl t = locput ~loc:(location ()) t
let make_init t = locput ~loc:(location ()) t
let typedef_flag = ref false

let debug_with_location s = 
  let l = strof_location (location ()) in
  Printf.eprintf "%s: %s" l s;
  prerr_newline ()

let failwith_location s = 
  let l = strof_location (location ()) in
  match !current_token_string with
    None -> failwith (Printf.sprintf "%s: %s" l s)
  | Some t -> failwith (Printf.sprintf "%s: %s before '%s'" l s t)

let enter_scope () =
(*debug_with_location "enter";*)
  enter_identifier_scope ()

let leave_scope () = 
(*debug_with_location "leave";*)
  leave_identifier_scope ()

let enter_merge_parameter_scope p =
(*debug_with_location "enter_merge";*)
  enter_merge_identifier_scope p

let leave_and_save_scope p = 
(*debug_with_location "leave_save";*)
  leave_and_save_identifier_scope p

let clear_parameter_binding () = 
(*debug_with_location "clear";*)
  parameter_scope := None

let rec get_name = function
    PdeclAnonymous -> assert false
  | PdeclIdent x -> x
  | PdeclPointer(_,x) -> get_name x
  | PdeclArray(x,_) -> get_name x
  | PdeclFuncType(x,_) -> get_name x
  | PdeclFuncIdent(x,_) -> get_name x

let rec spec_is_typedef = function
  [] -> false
| StorageClass Typedef::_ -> true
| _::tl -> spec_is_typedef tl

let check_typedef declspec init_declarator =
  let PinitDecl(decl, init) = init_declarator in
  if spec_is_typedef declspec then
    if init != None then
      failwith_location "typedef with initializer"
    else
      add_name (get_name decl) true
  else
    add_name (get_name decl) false

let parse_error_count = ref 0
let parse_error _ =
  (* failwith_location "parse error" *)
  parse_error_count := !parse_error_count + 1;
  let l = strof_location (location ()) in
  match !current_token_string with
    None -> Printf.eprintf "%s: parse error\n" l
  | Some t -> Printf.eprintf "%s: parse error before '%s'\n" l t

%}

%token COMMA SEMICOLON COLON DOT
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED
%token CONST VOLATILE INLINE
%token STRUCT UNION ENUM
%token DOTDOTDOT
%token CASE DEFAULT
%token IF ELSE SWITCH
%token WHILE DO FOR
%token GOTO CONTINUE BREAK RETURN
%token SIZEOF
%token EQUAL STAREQUAL SLASHEQUAL PERCENTEQUAL PLUSEQUAL MINUSEQUAL
%token LSHIFTEQUAL RSHIFTEQUAL AMPERSANDEQUAL HATEQUAL VBAREQUAL
%token QUESTION
%token VBARVBAR AMPERSANDAMPERSAND VBAR HAT AMPERSAND
%token EQUALEQUAL EXCLEQUAL
%token LANGLE RANGLE LANGLEEQUAL RANGLEEQUAL
%token LSHIFT RSHIFT PLUS MINUS STAR SLASH PERCENT
%token TILDE EXCL
%token PLUSPLUS MINUSMINUS RARROW
%token <string> INTEGER_CONSTANT
%token <string> CHARACTER_CONSTANT
%token <string> FLOATING_CONSTANT
%token <string> STRING
%token <C_abstree.identifier> IDENTIFIER
/* %token <C_abstree.identifier> ENUMERATION_CONSTANT */
%token <C_abstree.identifier> TYPEDEF_NAME
%token <string> PRAGMA

%token EOF

%type <C_abstree.program> translation_unit
%start translation_unit

/* Fail-Safe C extensions */
%token TYPEOF

/* Hook to general extension */
%token <C_abstree.identifier> EXTENSION_INTRO

/*
 * shift 'ELSE'
 *   IF LPAREN e RPAREN s . ELSE
 */
%nonassoc IF
%nonassoc ELSE

/*
 * reduce to type_specifier if possible
 *   identifier_forced: TYPEDEF_NAME .
 *   type_specifier: TYPEDEF_NAME .
 */
%nonassoc shift_typedef_name
%nonassoc TYPEDEF_NAME
%nonassoc reduce_if_typedef_name

%%
translation_unit:
  initialize translation_unit_impl EOF { List.rev $2 }

initialize: 
  { clear_parameter_binding (); clear_typedef_name () }

enter_scope:
  %prec reduce_if_typedef_name { enter_scope () }

clear_parameter_binding: { clear_parameter_binding () }

enter_merge_parameter_scope:
  { 
    if !parameter_scope = None then
      failwith_location "parameter declaration missing"
    else
      enter_merge_parameter_scope parameter_scope
  }

translation_unit_impl:
| translation_unit_impl external_declaration	{ $2 :: $1 }
| translation_unit_impl error { $1 }
| /* empty */ { [] }

external_declaration:
| declaration clear_parameter_binding semis_opt { $1 }
| function_definition semis_opt { $1 }
| PRAGMA { make_decl(PdeclPragmatic $1) }

semis_opt:
  { () }
| SEMICOLON semis_opt { () }

/*
 * Three kinds of declarator in C.
 *
 * 1. declarators used in variable declaration (declarator).
 * 2. declarators used in parameter declaration (parameter_declarator).
 * 3. declarators used in type name (abstract_declarator).
 *
 * 'declarator' must contain one identifier.
 * If the identifier is 'typedef-name', re-declare the identifier.
 *
 * 'parameter_declarator' may contain an identifier.
 * 'typedef-name' must be reduced to 'type-specifier' if possible.
 *
 * 'abstract-declarator' must contain no identifier.
 * 'typedef-name' is always reduced to 'type-specifier'.
 *
 * example: {type t: int}
 *
 * Declarator: t
 *   1. re-declare t
 *   2. re-declare t
 *   3. error
 * Declarator: (t)
 *   1. redelcare t
 *   2. anonymous.  fun int -> _
 *   3. anonymous.  fun int -> _
 * Declarator: ( * t)
 *   1. re-delcare t
 *   2. re-delcare t
 *   3. error
 * Declarator: ()
 *   1. error
 *   2. fun void -> _
 *   3. fun void -> _
 *
 */

declaration_specifier:
  storage_class_specifier { StorageClass $1 }
| type_qualifier          { TypeQualifier $1 }
| extended_declaration_specifier { $1 }

declaration_specifiers:
  declspec_with_type_spec { $1 }
| declspec_without_type_spec %prec shift_typedef_name { $1 }

declspec_without_type_spec:
  declaration_specifier { [$1] }
| declspec_without_type_spec declaration_specifier { $2::$1 }

declspec_with_type_spec:
  type_specifier { [TypeSpec($1)] }
| declspec_without_type_spec type_specifier { TypeSpec($2) :: $1 }
| declspec_with_type_spec declaration_specifier	{ $2::$1 }
| declspec_with_type_spec type_specifier_forced	{ TypeSpec($2) :: $1 }

function_definition:
  declaration_specifiers_opt declarator declaration_list_opt function_body
    {
      if spec_is_typedef $1 then
	failwith_location "typedef with function definition"
      else
        make_decl(PdeclFunction($1, $2, $3, $4))
    }

declaration_specifiers_opt:
  /* opt */ %prec shift_typedef_name { [] }
| declaration_specifiers { $1 }

declaration:
  declaration_specifiers SEMICOLON	{ make_decl(PdeclVariable($1, [])) }
| declaration_start SEMICOLON		{ make_decl(PdeclVariable(fst $1, List.rev (snd $1))) }

declaration_start:
  declaration_specifiers_opt init_declarator {
    check_typedef $1 $2;
    $1, [$2]
  }
| declaration_start COMMA init_declarator {
    check_typedef (fst $1) $3;
    fst $1, $3::snd $1
  }

init_declarator:
  declarator { PinitDecl($1, None) }
| declarator EQUAL c_initializer { PinitDecl($1, Some $3) }

declaration_list_opt:
  /* opt */	{ [] }
| declaration_list	{ $1 }

declaration_list:
  declaration_with_declspec	{ [$1] }
| declaration_list declaration_with_declspec	{ $1 @ [$2] }

declaration_with_declspec:
  declaration_specifiers SEMICOLON { make_decl(PdeclVariable($1, [])) }
| declaration_with_declspec_start SEMICOLON { make_decl(PdeclVariable(fst $1, List.rev (snd $1))) }

declaration_with_declspec_start:
  declaration_specifiers init_declarator {
    check_typedef $1 $2;
    $1, [$2]
  }
| declaration_with_declspec_start COMMA init_declarator {
    check_typedef (fst $1) $3;
    fst $1, $3::snd $1
  }

storage_class_specifier:
  AUTO	{ Auto }
| REGISTER	{ Register }
| STATIC	{ Static }
| EXTERN	{ Extern }
| TYPEDEF	{ Typedef }

parameter_storage_class_specifier:
  REGISTER	{ Register }

type_specifier_forced:
  VOID	{ PtypespecBuiltin Void }
| CHAR	{ PtypespecBuiltin Char }
| SHORT	{ PtypespecBuiltin Short }
| INT	{ PtypespecBuiltin Int }
| LONG	{ PtypespecBuiltin Long }
| FLOAT	{ PtypespecBuiltin Float }
| DOUBLE	{ PtypespecBuiltin Double }
| SIGNED	{ PtypespecBuiltin Signed }
| UNSIGNED	{ PtypespecBuiltin Unsigned }
| struct_or_union_specifier	{ $1 }
| enum_specifier	{ $1 }

type_specifier:
  type_specifier_forced { $1 }
| TYPEDEF_NAME { PtypespecAlias $1 }

type_qualifier:
  CONST		{ Const }
| VOLATILE	{ Volatile }

identifier_forced_opt:
  /* opt */ { None }
| identifier_forced { Some $1 }

struct_or_union_specifier:
  struct_or_union extended_declaration_specifiers_opt
    identifier_forced_opt LBRACE enter_scope struct_declaration_list RBRACE
    {
     leave_scope ();
     PtypespecStruct(fst $1,$3,Some $6,$2,snd $1)
   }
| struct_or_union extended_declaration_specifiers_opt identifier_forced
    { 
      PtypespecStruct(fst $1,Some $3,None,$2, snd $1)
    }

struct_or_union:
  STRUCT { Struct, location () }
| UNION	{ Union, location () }

struct_declaration_list:
  struct_declaration	{ [$1] }
| struct_declaration_list struct_declaration	{ $1 @ [$2] }

specifier_qualifier_list:
  sq_list_without_type_spec %prec shift_typedef_name { $1 }
| sq_list_with_type_spec { $1 }

sq_list_without_type_spec:
  type_qualifier { [ TypeQualifier $1 ] }
| sq_list_without_type_spec type_qualifier { TypeQualifier $2 :: $1 }

sq_list_with_type_spec:
  type_specifier { [ TypeSpec $1 ] }
| sq_list_without_type_spec type_specifier { TypeSpec $2 :: $1 }
| sq_list_with_type_spec type_qualifier { TypeQualifier $2 :: $1 }
| sq_list_with_type_spec type_specifier_forced { TypeSpec $2 :: $1 }

struct_declaration:
  specifier_qualifier_list struct_declarator_list_opt SEMICOLON
   { PstructDecl($1, $2) }

struct_declarator_list_opt:
  /* opt */ { [] }
| struct_declarator_list { $1 }

struct_declarator_list:
  struct_declarator	{ [$1] }
| struct_declarator COMMA struct_declarator_list
    { $1 :: $3 }

struct_declarator:
  declarator  { PstructDeclNormal $1 }
| declarator COLON constant_expression { PstructDeclBitfield(Some $1, $3) }
| COLON constant_expression            { PstructDeclBitfield(None, $2) }

enum_specifier:
  ENUM identifier_forced LBRACE enumerator_list RBRACE
    { PtypespecEnumByDef(Some $2, $4, location ()) }
| ENUM LBRACE enumerator_list RBRACE
    { PtypespecEnumByDef(None,$3, location ()) }
| ENUM identifier_forced { PtypespecEnumByName $2 }

enumerator_list:
  enumerator	{ [$1] }
| enumerator_list COMMA enumerator	{ $1 @ [$3] }

enumerator:
  identifier_forced {
    add_name $1 false;
    $1, None
  }
| identifier_forced EQUAL constant_expression {
    add_name $1 false;
    $1, Some $3
  }

declarator:
  direct_declarator { $1 }
| STAR type_qualifier_list_opt declarator { PdeclPointer($2,$3) }

direct_declarator:
  identifier_forced { PdeclIdent($1) }
| LPAREN declarator RPAREN	{ $2 }
| direct_declarator LBRACKET constant_expression_opt RBRACKET
    { PdeclArray($1,$3) }
| direct_declarator LPAREN parameter_type_list RPAREN
    { PdeclFuncType($1,$3) }
| direct_declarator LPAREN parameter_identifier_list_opt RPAREN
    { PdeclFuncIdent($1,$3) }

type_qualifier_list_opt:
  /* opt */	{ [] }
| type_qualifier_list	{ $1 }

type_qualifier_list:
  type_qualifier	{ [$1] }
| type_qualifier_list type_qualifier	{ $1 @ [$2] }

parameter_type_list_opt:
  /* opt */	{ [] }
| parameter_type_list	{ $1 }

parameter_type_list:
  enter_scope parameter_list
    { 
      leave_and_save_scope parameter_scope; 
      $2
    }
| enter_scope parameter_list COMMA DOTDOTDOT
    {
     leave_and_save_scope parameter_scope;
     $2 @ [PpdeclVariant]
   }

parameter_list:
  parameter_declaration	{ [$1] }
| parameter_list COMMA parameter_declaration	{ $1 @ [$3] }

parameter_declaration_specifier:
  parameter_storage_class_specifier { StorageClass $1 }
| type_qualifier          { TypeQualifier $1 }
| extended_declaration_specifier { $1 }

parameter_declaration_specifiers:
  pdeclspec_without_type_spec %prec shift_typedef_name { $1 }
| pdeclspec_with_type_spec { $1 }

pdeclspec_without_type_spec:
  parameter_declaration_specifier { [$1] }
| pdeclspec_without_type_spec parameter_declaration_specifier { $2::$1}

pdeclspec_with_type_spec:
  type_specifier { [TypeSpec($1)] }
| pdeclspec_without_type_spec type_specifier { TypeSpec($2) :: $1 }
| pdeclspec_with_type_spec parameter_declaration_specifier { $2::$1 }
| pdeclspec_with_type_spec type_specifier_forced { TypeSpec($2) :: $1 }

parameter_declaration:
  parameter_declaration_specifiers parameter_declarator {
    if fst $2 then begin
      add_name (get_name (snd $2)) false;
      PpdeclConcrete($1, snd $2)
    end else
      PpdeclAbstract($1, snd $2)
  }
| parameter_declaration_specifiers {
    PpdeclAbstract($1, PdeclAnonymous)
  }

parameter_declarator:
  direct_parameter_declarator { $1 }
| STAR type_qualifier_list_opt parameter_declarator
    { fst $3, PdeclPointer($2, snd $3) }
| STAR type_qualifier_list_opt
    { false, PdeclPointer($2, PdeclAnonymous) }

direct_parameter_declarator:
  identifier_forced { true, PdeclIdent $1 }
| LPAREN parameter_declarator RPAREN { $2 }
| direct_parameter_declarator LBRACKET constant_expression_opt RBRACKET
    { fst $1, PdeclArray(snd $1, $3) }
| direct_parameter_declarator LPAREN parameter_type_list RPAREN
    { fst $1, PdeclFuncType(snd $1, $3) }
| direct_parameter_declarator LPAREN parameter_identifier_list_opt RPAREN
    { fst $1, PdeclFuncIdent(snd $1, $3) }
| LBRACKET constant_expression_opt RBRACKET
    { false, PdeclArray(PdeclAnonymous, $2) }
| LPAREN parameter_type_list RPAREN
    { false, PdeclFuncType(PdeclAnonymous, $2) }

/* not used
identifier_list_opt:
  / * opt * /	{ [] }
| identifier_list	{ $1 }
*/

identifier_list:
  identifier	{ [$1] }
| identifier_list COMMA identifier
    { $1 @ [$3] }

parameter_identifier_list_opt:
  /* opt */ { enter_scope (); leave_and_save_scope parameter_scope; [] }
| enter_scope parameter_identifier_list	{ leave_and_save_scope parameter_scope; $2 }

parameter_identifier_list:
  identifier	{ add_name $1 false; [$1] }
| identifier COMMA identifier_list
    { add_name $1 false; $1 :: $3 }

/* "initializer" conflicts with OCaml keyword, renamed to c_initializer */
c_initializer:
  assignment_expression	{ make_init (PinitExp($1)) }
| LBRACE initializer_list_rev RBRACE	{ make_init (PinitList(List.rev $2)) }
| LBRACE initializer_list_rev COMMA RBRACE	{ make_init (PinitList(List.rev $2)) }

initializer_list_rev:
  c_initializer	{ [$1] }
| initializer_list_rev COMMA c_initializer	{ $3 :: $1 }

type_name:
  specifier_qualifier_list abstract_declarator_opt
    { Ptypename($1,$2) }

abstract_declarator:
  direct_abstract_declarator { $1 }
| STAR type_qualifier_list_opt abstract_declarator_opt { PdeclPointer($2,$3) }

abstract_declarator_opt:
| /* opt */ { PdeclAnonymous }
| direct_abstract_declarator { $1 }
| STAR type_qualifier_list_opt abstract_declarator_opt { PdeclPointer($2,$3) }

direct_abstract_declarator:
  LPAREN abstract_declarator RPAREN	{ $2 }
| LBRACKET constant_expression_opt RBRACKET
    { PdeclArray(PdeclAnonymous,$2) }
| direct_abstract_declarator LBRACKET constant_expression_opt RBRACKET
    { PdeclArray($1,$3) }
| direct_abstract_declarator LPAREN parameter_type_list_opt RPAREN
    { PdeclFuncType($1,$3) }
| LPAREN parameter_type_list_opt RPAREN
    { PdeclFuncType(PdeclAnonymous,$2) }

statement:
  labelled_statement	{ make_stmt($1) }
| expression_statement	{ make_stmt(PstmtExpr $1) }
| compound_statement	{ $1 }
| selection_statement	{ make_stmt($1) }
| iteration_statement	{ make_stmt($1) }
| jump_statement	{ make_stmt($1) }
| error SEMICOLON       { make_stmt(PstmtExpr None) }

labelled_statement:
  identifier COLON statement	{ PstmtLabeled($1,$3) }
| CASE constant_expression COLON statement	{ PstmtCase_Labeled($2,$4) }
| DEFAULT COLON statement	{ PstmtDefault_Labeled($3) }

expression_statement:
  expression_opt SEMICOLON	{ ($1) }

compound_statement:
  LBRACE enter_scope declaration_list_opt clear_parameter_binding statement_list_opt RBRACE
    { leave_scope (); make_stmt(PstmtCompound($3,$5)) }

function_body:
  LBRACE enter_merge_parameter_scope declaration_list_opt statement_list_opt RBRACE
    { leave_scope (); make_stmt(PstmtCompound($3,$4)) }

statement_list_opt:
  /* opt */	{ [] }
| statement_list	{ $1 }

statement_list:
  statement	{ [$1] }
| statement statement_list	{ $1 :: $2 }

expression_with_paren:
  LPAREN expression RPAREN      { $2 }
| error                         { make_exp(PexpConstant(PconstInteger "0")) }

selection_statement:
  IF expression_with_paren statement	{ PstmtIf($2,$3,None) }
| IF expression_with_paren statement ELSE statement	{ PstmtIf($2,$3,Some $5) }
| SWITCH expression_with_paren statement	{ PstmtSwitch($2,$3) }

iteration_statement:
  WHILE expression_with_paren statement	{ PstmtWhile($2,$3) }
| DO statement WHILE expression_with_paren SEMICOLON	{ PstmtDoWhile($2,$4) }
| FOR LPAREN expression_opt SEMICOLON expression_opt SEMICOLON expression_opt RPAREN statement
    { PstmtFor($3,$5,$7,$9) }
| FOR error statement { PstmtFor(None, None, None, $3) }

jump_statement:
  GOTO identifier SEMICOLON	{ PstmtGoto($2) }
| CONTINUE SEMICOLON	{ PstmtContinue }
| BREAK SEMICOLON	{ PstmtBreak }
| RETURN expression_opt SEMICOLON	{ PstmtReturn($2) }

expression_opt:
  /* opt */	{ None }
| expression	{ Some $1 }

expression:
  assignment_expression { $1 }
| expression COMMA assignment_expression
    { make_exp (PexpComma($1,$3)) }

assignment_expression:
  conditional_expression { $1 }
| unary_expression EQUAL assignment_expression
    { make_exp (PexpAssign($1,$3)) }
| unary_expression mod_assignment_operator assignment_expression
    { make_exp (PexpBinAssign($2,$1,$3)) }

mod_assignment_operator:
  STAREQUAL	{ PbinTimes }
| SLASHEQUAL	{ PbinDiv }
| PERCENTEQUAL	{ PbinModulo }
| PLUSEQUAL	{ PbinPlus }
| MINUSEQUAL	{ PbinMinus }
| LSHIFTEQUAL	{ PbinLshift }
| RSHIFTEQUAL	{ PbinRshift }
| AMPERSANDEQUAL	{ PbinIntAnd }
| HATEQUAL	{ PbinIntXor }
| VBAREQUAL	{ PbinIntOr }

conditional_expression:
  logical_or_expression { $1 }
| logical_or_expression QUESTION expression COLON conditional_expression
    { make_exp (PexpConditional($1,$3,$5)) }

constant_expression_opt:
  /* opt */	{ None }
| constant_expression		{ Some $1 }

constant_expression:
  conditional_expression  { $1 }

logical_or_expression:
  logical_and_expression  { $1 }
| logical_or_expression VBARVBAR logical_and_expression
    { make_exp (PexpBinExpr(PbinLogOr,$1,$3)) }

logical_and_expression:
  inclusive_or_expression { $1 }
| logical_and_expression AMPERSANDAMPERSAND inclusive_or_expression
    { make_exp (PexpBinExpr(PbinLogAnd,$1,$3)) }

inclusive_or_expression:
  exclusive_or_expression { $1 }
| inclusive_or_expression VBAR exclusive_or_expression
    { make_exp (PexpBinExpr(PbinIntOr,$1,$3)) }

exclusive_or_expression:
  and_expression { $1 }
| exclusive_or_expression HAT and_expression
    { make_exp (PexpBinExpr(PbinIntXor,$1,$3)) }

and_expression:
  equality_expression { $1 }
| and_expression AMPERSAND equality_expression
    { make_exp (PexpBinExpr(PbinIntAnd,$1,$3)) }

equality_expression:
  relational_expression { $1 }
| equality_expression EQUALEQUAL relational_expression
    { make_exp (PexpBinExpr(PbinEqual,$1,$3)) }
| equality_expression EXCLEQUAL relational_expression
    { make_exp (PexpBinExpr(PbinNotEqual,$1,$3)) }

relational_expression:
  shift_expression { $1 }
| relational_expression LANGLE shift_expression
    { make_exp (PexpBinExpr(PbinLessThan,$1,$3)) }
| relational_expression RANGLE shift_expression
    { make_exp (PexpBinExpr(PbinGtrThan,$1,$3)) }
| relational_expression LANGLEEQUAL shift_expression
    { make_exp (PexpBinExpr(PbinLessEqual,$1,$3)) }
| relational_expression RANGLEEQUAL shift_expression
    { make_exp (PexpBinExpr(PbinGtrEqual,$1,$3)) }

shift_expression:
  additive_expression { $1 }
| shift_expression LSHIFT additive_expression
    { make_exp (PexpBinExpr(PbinLshift,$1,$3)) }
| shift_expression RSHIFT additive_expression
    { make_exp (PexpBinExpr(PbinRshift,$1,$3)) }

additive_expression:
  multiplicative_expression { $1 }
| additive_expression PLUS multiplicative_expression
    { make_exp (PexpBinExpr(PbinPlus,$1,$3)) }
| additive_expression MINUS multiplicative_expression
    { make_exp (PexpBinExpr(PbinMinus,$1,$3)) }

multiplicative_expression:
  cast_expression { $1 }
| multiplicative_expression STAR cast_expression
    { make_exp (PexpBinExpr(PbinTimes,$1,$3)) }
| multiplicative_expression SLASH cast_expression
    { make_exp (PexpBinExpr(PbinDiv,$1,$3)) }
| multiplicative_expression PERCENT cast_expression
    { make_exp (PexpBinExpr(PbinModulo,$1,$3)) }

cast_expression:
  unary_expression { $1 }
| LPAREN type_name RPAREN cast_expression
    { make_exp (PexpCast($2,$4)) }

unary_expression:
  postfix_expression { $1 }
| PLUSPLUS unary_expression	{ make_exp (PexpPreInc $2) }
| MINUSMINUS unary_expression	{ make_exp (PexpPreDec $2) }
| AMPERSAND cast_expression { make_exp (PexpAddress $2) }
| STAR cast_expression { make_exp (PexpPtrDeref $2) }
| simple_unary_operator cast_expression { make_exp (PexpUnaryExpr($1,$2)) }
| SIZEOF unary_expression	{ make_exp (PexpSizeOfExpr($2)) }
| SIZEOF LPAREN type_name RPAREN	{ make_exp (PexpSizeOfType($3)) }
/* Fail-Safe C extensions */
| TYPEOF LPAREN type_name RPAREN	{ make_exp (PexpTypeOfType($3)) }

simple_unary_operator:
| PLUS	{ UnaryPlus }
| MINUS	{ UnaryMinus }
| TILDE	{ IntNot }
| EXCL	{ LogNot }

postfix_expression:
  primary_expression { $1 }
| postfix_expression LBRACKET expression RBRACKET
    { make_exp (PexpArrayRef($1,$3)) }
| postfix_expression LPAREN argument_expression_list_opt RPAREN
    { make_exp (PexpInvoke($1,$3)) }
| postfix_expression DOT identifier_forced	{ make_exp (PexpField($1,$3)) }
| postfix_expression RARROW identifier_forced	{ make_exp (PexpPtrField($1,$3)) }
| postfix_expression PLUSPLUS	{ make_exp (PexpPostInc $1) }
| postfix_expression MINUSMINUS	{ make_exp (PexpPostDec $1) }

primary_expression:
  identifier	{ make_exp (PexpVar $1) }
| constant	{ make_exp (PexpConstant $1) }
| string	{ make_exp (PexpConstant $1) }
| LPAREN expression RPAREN	{ $2 }

argument_expression_list_opt:
  /* opt */	{ [] }
| argument_expression_list { $1 }

argument_expression_list:
  assignment_expression	{ [$1] }
| argument_expression_list COMMA assignment_expression	{ $1 @ [$3] }

constant:
  integer_constant { $1 }
| character_constant { $1 }
| floating_constant { $1 }

/* terminals */

integer_constant:
  INTEGER_CONSTANT	{ PconstInteger $1 }

character_constant:
  CHARACTER_CONSTANT	{ PconstChar $1 }

floating_constant:
  FLOATING_CONSTANT	{ PconstFloat $1 }

identifier:
  IDENTIFIER	{ $1 }

string:
  string_internal { PconstString($1) }

string_internal:
  STRING	{ [$1] }
| STRING string_internal { $1 :: $2 }

/* enumeration_constant:
  ENUMERATION_CONSTANT	{ PconstEnum($1) } */

identifier_forced:
  IDENTIFIER	{ $1 }
| TYPEDEF_NAME 	{ $1 }

/* extension hook */
extended_declaration_specifiers_opt:
  /* opt */ { [] }
| EXTENSION_INTRO extension_element extended_declaration_specifiers_opt { ($1, $2) :: $3 }

extended_declaration_specifier:
    EXTENSION_INTRO extension_element { ExtendedDeclSpec($1, $2) }

extension_tree_comma_list:
  extension_element_list { [$1] }
| extension_element_list COMMA extension_tree_comma_list { $1 :: $3 }

extension_element_list:
  extension_element_list_internal {
    match $1 with
      [i] -> i
    | l -> Elist l
}

extension_element_list_internal:
  extension_element extension_element_list_internal { $1 :: $2 }
| /* */ { [] }

extension_element:
  LPAREN extension_tree_comma_list RPAREN { Eplist $2 }
| identifier_forced { Eident $1 }
| INTEGER_CONSTANT { Eint $1 }
| STRING { Estring $1 }

%%

let translation_unit (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
  let my_lexfun lexbuf =
    let t = lexfun lexbuf in
    current_token_string := Some (Lexing.lexeme lexbuf);
    t
  in
  parse_error_count := 0;
  let t = translation_unit my_lexfun lexbuf in
  if !parse_error_count > 0 then
    failwith "parser: stopped by syntax error";
  t

