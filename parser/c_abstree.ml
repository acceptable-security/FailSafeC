(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2001 -- 2006. *)

open Locterm

type identifier = string

(* extension *)
type extension_tree = 
    Eplist of extension_tree list  (*  (e1, e2, ...) *)
  | Elist of extension_tree list   (*  e1 e2 ... *)
  | Eident of identifier
  | Estring of string
  | Eint of string

type extension_spec = 
    string * extension_tree

type signedness = 
    Tsigned | Tunsigned

type union_flag = Struct | Union

type binop =
    PbinTimes | PbinDiv | PbinPlus | PbinMinus | PbinModulo | PbinLshift
  | PbinRshift | PbinLogAnd | PbinLogOr | PbinIntAnd | PbinIntOr | PbinIntXor
  | PbinLessThan | PbinLessEqual | PbinGtrThan | PbinGtrEqual
  | PbinEqual | PbinNotEqual

type unaryop =
    UnaryPlus | UnaryMinus | LogNot | IntNot

type storage_class = Auto | Register | Inline | Static | Extern | Typedef

type type_qualifier = Const | Volatile

type builtin_type_specifier =
    Void | Char | Short | Int | Long | Float | Double
  | Signed | Unsigned

and type_specifier = 
    PtypespecBuiltin of builtin_type_specifier
  | PtypespecAlias of identifier
  | PtypespecEnumByDef of identifier option * enumerator list * location
  | PtypespecEnumByName of identifier
  | PtypespecStruct of union_flag * identifier option * struct_declaration list option * extension_spec list * location

and pointer_specifier = Pptrspec of type_qualifier list

and struct_declaration = PstructDecl of declaration_specifier list * struct_declarator list

and struct_declarator =
    PstructDeclNormal of declarator
  | PstructDeclBitfield of declarator option * expr

(* and enum_specifier => merged into type_specifier *)

and enumerator = 
    identifier * expr option

and type_name = Ptypename of specifier_qualifier list * abstract_declarator

and specifier_qualifier = declaration_specifier
(*     PsqSpec of type_specifier*)
(*   | PsqQual of type_qualifier*)

and program = declaration list

and declaration = declaration_desc Locterm.t

and declaration_desc = 
    PdeclFunction of declaration_specifier list * declarator * declaration list * statement
  | PdeclVariable of declaration_specifier list * init_declarator list
  | PdeclPragmatic of string

and parameter_declaration = 
    PpdeclConcrete of declaration_specifier list * declarator
  | PpdeclAbstract of declaration_specifier list * abstract_declarator
  | PpdeclVariant

and init_declarator = PinitDecl of declarator * c_initializer option

and c_initializer_desc = 
    PinitExp of expr
  | PinitList of c_initializer list

and c_initializer = c_initializer_desc Locterm.t

and parameter_type = parameter_declaration

and abstract_declarator = declarator

and declarator = 
  | PdeclAnonymous
  | PdeclIdent of identifier
  | PdeclPointer of type_qualifier list * declarator
  | PdeclArray of declarator * expr option
  | PdeclFuncType of declarator * parameter_type list
  | PdeclFuncIdent of declarator * identifier list

and declaration_specifier = 
    StorageClass of storage_class
  | TypeSpec of type_specifier
  | TypeQualifier of type_qualifier
  | ExtendedDeclSpec of extension_spec

and statement = statement_desc Locterm.t

and statement_desc = 
    PstmtExpr of expr option
  | PstmtLabeled of identifier * statement
  | PstmtCase_Labeled of expr * statement
  | PstmtDefault_Labeled of statement
  | PstmtCompound of declaration list * statement list
  | PstmtIf of expr * statement * statement option
  | PstmtSwitch of expr * statement
  | PstmtWhile of expr * statement
  | PstmtDoWhile of statement * expr
  | PstmtFor of expr option * expr option * expr option * statement
  | PstmtGoto of identifier
  | PstmtContinue
  | PstmtBreak
  | PstmtReturn of expr option

and expr = expr_desc Locterm.t

and expr_desc = 
  | PexpComma of expr * expr
  | PexpAssign of expr * expr
  | PexpBinAssign of binop * expr * expr
  | PexpConditional of expr * expr * expr
  | PexpBinExpr of binop * expr * expr
  | PexpCast of type_name * expr
  | PexpUnaryExpr of unaryop * expr
  | PexpPreInc of expr
  | PexpPreDec of expr
  | PexpPostInc of expr
  | PexpPostDec of expr
  | PexpAddress of expr
  | PexpPtrDeref of expr
  | PexpSizeOfType of type_name
  | PexpSizeOfExpr of expr
  | PexpArrayRef of expr * expr
  | PexpInvoke of expr * expr list
  | PexpField of expr * identifier
  | PexpPtrField of expr * identifier
  | PexpConstant of c_constants
  | PexpVar of identifier
(* Fail-Safe C Extensions *)
  | PexpTypeOfType of type_name

and c_constants = 
  | PconstInteger of string
  | PconstChar of string
  | PconstFloat of string
  | PconstString of string list

(*  | PconstEnum of identifier *)

let c_identifier_regexp = 
  Str.regexp "^[A-Za-z_][A-Za-z0-9_]*$"

let is_valid_identifier i = 
  if Str.string_match c_identifier_regexp i 0 then
    Str.match_beginning () == 0 &&
    Str.match_end () == String.length i
  else
    false
