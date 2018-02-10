{
open Fscw_tree
open Fscw_parser

let remove_quotes quote s = 
  let l = String.length s in
  assert(l >= 2);
  assert(s.[0] = quote);
  assert(s.[l - 1] = quote);
  String.sub s 1 (l - 2)

type result =
  Token of token
| Section of section

let copy_char lexbuf =
  let ch = Lexing.lexeme lexbuf in
  if ch = "\n" then lineno := !lineno + 1;
  Token (CopyChar ch)

let start_section sec =
  lineno := !lineno + 2;
  Section sec
}



let identchar = 
  ['A'-'Z' 'a'-'z' '_' '0'-'9']
let identtopchar = 
  ['A'-'Z' 'a'-'z' '_']
let bschar = [^ '0'-'7' 'x' ]

let symbolchar = 
  [',' ';' ':' '.' '{' '}' '[' ']' '(' ')' 
     '=' '?' ':' '|' '^' '&' '+' '-' '*' '/' '%' '~' '!'
     '<' '>']

let digits = ['0'-'9']

let hexchar = ['0'-'9' 'a'-'f' 'A'-'F']

let int_postfix = ['u' 'U' 'l' 'L']

let float_e = ['e' 'E']

let float_postfix = ['f' 'F' 'l' 'L']

let plusminus = ['+' '-']

let inline_spaces = [' ' '\t' '\r'
		       '\012' (* formfeed *) 
		   ]

rule prologue_section = parse
| "\n%%" { separator lexbuf }
| eof { Token Eof }
| _ { copy_char lexbuf }

and c_prologue_section = parse
| "\n%%" { separator lexbuf }
| eof { Token Eof }
| _ { copy_char lexbuf }

and fsc_prologue_section = parse
| "\n%%" { separator lexbuf }
| eof { Token Eof }
| _ { copy_char lexbuf }

and fsc_required_decls = parse
| "\n%%" { separator lexbuf }
| eof { Token Eof }
| "\n" { lineno := !lineno + 1; fsc_required_decls lexbuf }
| inline_spaces+ { fsc_required_decls lexbuf }
| "//" [ ^ '\n' ] * "\n" { lineno := !lineno + 1; fsc_required_decls lexbuf }
| identtopchar identchar *
    {
      let s = Lexing.lexeme lexbuf in
      if s = "struct" then Token STRUCT
      else if s = "typedef" then Token TYPEDEF
      else if s = "require_function" || s = "provide_function" then Token PROVIDE_FUNCTION
      else if s = "require_external_function" then Token REQUIRE_EXTERNAL_FUNCTION
      else if s = "require_external_value" then Token REQUIRE_EXTERNAL_VALUE
      else if s = "auto_generate" then Token AUTO_GENERATE
      else if s = "emit_typeinfo" then Token EMIT_TYPEINFO
      else if s = "module_attribute" then Token MODULE_ATTRIBUTE
      else Token (IDENTIFIER s)
    }
| ";" { Token SEMICOLON }
| "(" { Token LPAREN }
| ")" { Token RPAREN }
| '"' ([^ '\\' '"'] | '\\' ['0'-'7'] ['0'-'7'] ? ['0'-'7'] ?
       | '\\' 'x' hexchar hexchar * | '\\' bschar) * '"' 
    { Token (STRING (remove_quotes '"' (Lexing.lexeme lexbuf))) }

and native_implementation = parse
| "\n%%" { separator lexbuf }
| eof { Token Eof }
| _ { copy_char lexbuf }

and separator = parse
| " "* "C-PROLOGUE" " "* "\n"
 { start_section CPrologue }
| " "* "FSC-PROLOGUE" " "* "\n"
 { start_section FSCPrologue }
| " "* "REQUIRED-DECLS" " "* "\n"
 { start_section RequiredDecls }
| " "* "NATIVE-IMPLEMENTATION" " "* "\n"
 { start_section NativeImplementation }

{
let tokenize section lexbuf =
  let r = match !section with
      Prologue ->  prologue_section lexbuf
    | CPrologue -> c_prologue_section lexbuf
    | FSCPrologue -> fsc_prologue_section lexbuf
    | RequiredDecls -> fsc_required_decls lexbuf
    | NativeImplementation -> native_implementation lexbuf
  in
  match r with
    Token tok -> tok
  | Section sec ->
    section := sec;
    let pos = (!filename, !lineno) in
    match sec with
      Prologue -> assert false
    | CPrologue -> SepCPrologue pos
    | FSCPrologue -> SepFSCPrologue pos
    | RequiredDecls -> SepRequiredDecls pos
    | NativeImplementation -> SepNativeImplementation pos

let make_tokenizer () = tokenize (ref Prologue)
}

