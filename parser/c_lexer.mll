{(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-     AIST.

   This file is written by Yutaka Oiwa in 2001 -- 2009. *)

 open C_abstree
 open C_parserhelper
 open C_parser
 exception Eof

let use_failsafec_extension = ref false
let use_c99_constants = ref false
let extension_keywords = ref []

let identifier_list = lazy (
  [
   "auto", AUTO;
   "register", REGISTER;
   "static", STATIC;
   "inline", INLINE;
   "extern", EXTERN;
   "typedef", TYPEDEF;
   "void", VOID;
   "case", CASE;
   "default", DEFAULT;
   "char", CHAR;
   "short", SHORT;
   "int", INT;
   "long", LONG;
   "float", FLOAT;
   "double", DOUBLE;
   "signed", SIGNED;
   "unsigned", UNSIGNED;
   "const", CONST;
   "volatile", VOLATILE;
   "struct", STRUCT;
   "union", UNION;
   "enum", ENUM;
   "if", IF;
   "else", ELSE;
   "switch", SWITCH;
   "while", WHILE;
   "do", DO;
   "for", FOR;
   "goto", GOTO;
   "continue", CONTINUE;
   "break", BREAK;
   "return", RETURN;
   "sizeof", SIZEOF;
 ] @ 
  (if !use_failsafec_extension then
    [
     "__typeof", TYPEOF;
   ] else
    [ ] )
  @
    (Util.list_map (fun id -> id, EXTENSION_INTRO id) !extension_keywords)
 )
     
let identifier_of s = List.assoc s (Lazy.force identifier_list)

let symbol_of s = List.assoc s
     [
      ",", COMMA; ";", SEMICOLON; ":", COLON; ".", DOT;
      "{", LBRACE; "}", RBRACE; "[", LBRACKET; "]", RBRACKET;
      "(", LPAREN; ")", RPAREN;
      "=", EQUAL; "?", QUESTION; "|", VBAR; "^", HAT; "&", AMPERSAND;
      "<", LANGLE; ">", RANGLE;
      "+", PLUS; "-", MINUS; "*", STAR; "/", SLASH; "%", PERCENT;
      "~", TILDE; "!", EXCL
    ] 
let remove_quotes quote s = 
  let l = String.length s in
  assert(l >= 2);
  assert(s.[0] = quote);
  assert(s.[l - 1] = quote);
  String.sub s 1 (l - 2)

let parse_newline lexbuf = 
  let pos = Lexing.lexeme_end lexbuf in
  register_linestart ~lineno:!lineno pos;
  incr lineno

let three_regexp = Str.regexp ".* 3 "

let parse_sharp_position_directive lexbuf = 
  let str = Lexing.lexeme lexbuf in
  let str = if str.[0] = '#' then "\n" ^ str else str in
  let lno, fname, flags = 
    Scanf.sscanf str "\n# %d \"%s@\"%s@\n" (fun a b c -> a, b, c) in
  filename := fname;
  lineno := lno;
  file_is_trustful := Str.string_match three_regexp (" " ^ flags ^ " ") 0;
(*  Format.eprintf "LINE: %S@\n fname=%S lno=%d flags=%S trust=%b@\n" str fname lno flags !file_is_trustful; *)
  ()
  
(* ; parse_newline lexbuf *)

let failwith_location ~lexbuf s = 
  let pos = Lexing.lexeme_start lexbuf in
  let l = Locterm.strof_location (lookup_location pos) in
  failwith
    (Printf.sprintf "%s: %s" l s)

let hexfloat_constant lexbuf = 
  let s = Lexing.lexeme lexbuf in
  if not !use_c99_constants then
    failwith_location ~lexbuf "not an ANSI-C constant";
  FLOATING_CONSTANT s
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

let hex_prefix = '0' ['X' 'x']

let hexchar = ['0'-'9' 'a'-'f' 'A'-'F']

let int_postfix = ['u' 'U' 'l' 'L']

let float_e = ['e' 'E']

let float_p = ['p' 'P']

let float_postfix = ['f' 'F' 'l' 'L']

let plusminus = ['+' '-']

let inline_spaces = [' ' '\t' '\r'
		       '\012' (* formfeed *) 
		   ]

rule token = parse
| "\n# " digits + " " + '"' [^ '"']+ '"' [^ '\n'] *
    { parse_sharp_position_directive lexbuf; token lexbuf }
| "\n" inline_spaces * "#" inline_spaces * "pragma" inline_spaces + [^ '\n'] *
    { parse_newline lexbuf; 
      let s = Lexing.lexeme lexbuf in
      let p = String.index s '#' in
      PRAGMA (String.sub s p (String.length s - p)) }
| "# " digits + " " + '"' [^ '"']+ '"' [^ '\n'] *
    { 
      if Lexing.lexeme_start lexbuf = 0 then
	(parse_sharp_position_directive lexbuf; token lexbuf)
      else
	failwith_location ~lexbuf "bad # directive"
    }
| '\n' { parse_newline lexbuf; token lexbuf }
| inline_spaces + { token lexbuf }
| '"' ([^ '\\' '"'] | '\\' ['0'-'7'] ['0'-'7'] ? ['0'-'7'] ?
       | '\\' 'x' hexchar hexchar * | '\\' bschar) * '"' 
    { STRING(remove_quotes '"' (Lexing.lexeme lexbuf)) }
| '\'' ([^ '\\' '\''] | '\\' ['0'-'7'] ['0'-'7'] ? ['0'-'7'] ?
       | '\\' 'x' hexchar hexchar * | '\\' bschar) * '\'' 
    { CHARACTER_CONSTANT(remove_quotes '\'' (Lexing.lexeme lexbuf)) }
| '0' ['0' - '7'] * int_postfix *
| ['1' - '9'] digits * int_postfix * { INTEGER_CONSTANT(Lexing.lexeme lexbuf) }
| digits + '.' digits * ( float_e plusminus ? digits + ) ? float_postfix *
| digits * '.' digits + ( float_e plusminus ? digits + ) ? float_postfix *
| digits + float_e plusminus ? digits + float_postfix *
    { FLOATING_CONSTANT(Lexing.lexeme lexbuf) }
| hex_prefix { (if !use_c99_constants then hex_c99 else hex_c90) (Lexing.lexeme lexbuf) lexbuf }
| identtopchar identchar *
    { let s = Lexing.lexeme lexbuf in
    try identifier_of(s) with _ -> 
      if is_typedef_name s then
	TYPEDEF_NAME(s)
      else
	IDENTIFIER(s)
    } 
| "..." { DOTDOTDOT }
| "*=" { STAREQUAL }
| "/=" { SLASHEQUAL }
| "%=" { PERCENTEQUAL }
| "+=" { PLUSEQUAL }
| "-=" { MINUSEQUAL }
| "<<=" { LSHIFTEQUAL }
| ">>=" { RSHIFTEQUAL }
| "&=" { AMPERSANDEQUAL }
| "^=" { HATEQUAL }
| "|=" { VBAREQUAL }
| "||" { VBARVBAR }
| "&&" { AMPERSANDAMPERSAND }
| "==" { EQUALEQUAL }
| "!=" { EXCLEQUAL }
| "<=" { LANGLEEQUAL }
| ">=" { RANGLEEQUAL }
| "<<" { LSHIFT }
| ">>" { RSHIFT }
| "->" { RARROW }
| "++" { PLUSPLUS }
| "--" { MINUSMINUS }
| symbolchar { let s = Lexing.lexeme lexbuf in
  try symbol_of(s) with _ -> IDENTIFIER(s)
	     } 
| eof { EOF }
| _ { failwith_location ~lexbuf (Printf.sprintf "unknown token : \"%s\""
		  (String.escaped (Lexing.lexeme lexbuf)))
    }

and hex_c90 prefix = parse
| hexchar + int_postfix * { INTEGER_CONSTANT(prefix ^ Lexing.lexeme lexbuf) }
| _ { failwith_location ~lexbuf (Printf.sprintf "unknown token : \"%s\""
		  (String.escaped (prefix ^ Lexing.lexeme lexbuf)))
    }

and hex_c99 prefix = parse
| hexchar + int_postfix * { INTEGER_CONSTANT(prefix ^ Lexing.lexeme lexbuf) }
| hexchar + '.' hexchar * float_p plusminus ? digits + float_postfix ?
| hexchar * '.' hexchar + float_p plusminus ? digits + float_postfix ?
| hexchar + float_p plusminus ? digits + float_postfix ?
    { FLOATING_CONSTANT(prefix ^ Lexing.lexeme lexbuf) }
| _ { failwith_location ~lexbuf (Printf.sprintf "unknown token : \"%s\""
		  (String.escaped (prefix ^ Lexing.lexeme lexbuf)))
    }
