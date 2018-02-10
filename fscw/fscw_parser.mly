%{
open Fscw_tree
%}

%token<string> CopyChar
%token<string * int> SepCPrologue
%token<string * int> SepFSCPrologue
%token<string * int> SepRequiredDecls
%token<string * int> SepNativeImplementation
%token STRUCT TYPEDEF PROVIDE_FUNCTION REQUIRE_EXTERNAL_FUNCTION
%token REQUIRE_EXTERNAL_VALUE AUTO_GENERATE EMIT_TYPEINFO
%token MODULE_ATTRIBUTE
%token SEMICOLON LPAREN RPAREN
%token<string> IDENTIFIER
%token<string> STRING
%token Eof

%start parse_fscw
%type<Fscw_tree.t> parse_fscw

%%

parse_fscw:
  chars
  SepCPrologue
  chars
  SepFSCPrologue
  chars
  SepRequiredDecls
  required_decls
  SepNativeImplementation
  chars
  Eof
  { { r_prologue = $1 ^ "\n", (!filename, 1);
      r_c_prologue = $3 ^ "\n", $2;
      r_fsc_prologue = $5 ^ "\n", $4;
      r_required_decls = $7, $6;
      r_native_implementation = $9, $8; } }

chars:
  /* empty */ { "" }
| chars CopyChar { $1 ^ $2 }

required_decls:
  /* empty */ { [] }
| required_decls required_decl { $1 @ [$2] }

required_decl:
  TYPEDEF IDENTIFIER SEMICOLON { RequireTypedef $2 }
| STRUCT IDENTIFIER SEMICOLON { RequireStruct $2 }
| PROVIDE_FUNCTION IDENTIFIER SEMICOLON { RequireFunction ($2, true) }
| REQUIRE_EXTERNAL_FUNCTION IDENTIFIER SEMICOLON { RequireFunction ($2, false) }
| REQUIRE_EXTERNAL_VALUE IDENTIFIER SEMICOLON { RequireValue ($2) }
| AUTO_GENERATE IDENTIFIER SEMICOLON { AutoGenerate $2 }
| EMIT_TYPEINFO IDENTIFIER SEMICOLON { EmitTypeinfo $2 }
| MODULE_ATTRIBUTE STRING SEMICOLON { EmitModuleExtension $2 }

%%
