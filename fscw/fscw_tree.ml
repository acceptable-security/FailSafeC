type section =
| Prologue
| CPrologue
| FSCPrologue
| RequiredDecls
| NativeImplementation

type pos = string * int

type required_decl =
| RequireStruct of string
| RequireTypedef of string
| RequireFunction of string * bool
| RequireValue of string (* * bool *)
| AutoGenerate of string
| EmitTypeinfo of string
| EmitModuleExtension of string

type t = {
  r_prologue: string * pos;
  r_c_prologue: string * pos;
  r_fsc_prologue: string * pos;
  r_required_decls: required_decl list * pos;
  r_native_implementation: string * pos;
}

let lineno = ref 1
let filename = ref ""

let reset name =
  lineno := 1;
  filename := name
