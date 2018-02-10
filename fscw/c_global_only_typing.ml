open C_abstree
open Ctt_abstree
open Ctt_formatter
open Locterm
open Big_int_infix
open Wl

let find_struct_tag ~genv id =
  let rec iter = function
      [] -> None
    | (nam, v) :: t ->
	if v = id then Some nam else iter t
  in
  iter genv.Ctt_abstree.struct_name_table

let rec list_abstypename_of_declarator d list =
  match d with
  | PdeclAnonymous | PdeclIdent _ -> list
  | PdeclArray(d', _) | PdeclPointer(_, d') | PdeclFuncIdent(d', _) ->
      list_abstypename_of_declarator d' list
  | PdeclFuncType(d', params) ->
      let list' = List.fold_left
        (fun list d -> match d with
           PpdeclConcrete(specs, d) | PpdeclAbstract(specs, d) ->
             list_abstypename_of_decl specs d list
         | _ -> list)
        list params
      in
      list_abstypename_of_declarator d' list'

and list_abstypename_of_decl specs declarator list =
  let list' = list_abstypename_of_declarator declarator list in
  List.fold_left
    (fun list spec -> match spec with
       TypeSpec(PtypespecAlias name) -> name::list
     | _ -> list)
    list' specs

let rec name_of_declarator = function
  PdeclAnonymous -> None
| PdeclIdent name -> Some name
| PdeclArray(d', _) | PdeclPointer(_, d') | PdeclFuncType(d', _) | PdeclFuncIdent(d', _)
   -> name_of_declarator d'


let fold_left_global_var f x decls =
  List.fold_left
    (fun y d -> match locval d with
       PdeclVariable(specs, init_declarators) ->
         List.fold_left
           (fun z (PinitDecl(declarator, _)) ->
              match name_of_declarator declarator with
                Some name -> f z name specs declarator
              | None -> z)
           y init_declarators
     | _ -> y)
    x decls

(*
 * f is called with each global variable decl.
 *)
let map_global_var f decls =
  fold_left_global_var
    (fun list name specs declarator ->
       (f name specs declarator)::list)
    [] decls

(*
 * size_t x(off_t y)
 * --> (x, [size_t; off_t])
 *)
let list_type_dependency decls =
  map_global_var
    (fun name specs declarator ->
       name, list_abstypename_of_decl specs declarator [])
    decls

let list_decl decls =
  map_global_var
    (fun name specs declarator ->
       name, PdeclVariable(specs, [PinitDecl(declarator, None)]))
    decls

let list_ident_of_declarator init_decls =
  List.fold_left
    (fun lis (PinitDecl(declarator, _)) ->
       match name_of_declarator declarator with
         Some name -> name::lis
       | None -> lis)
    [] init_decls

(*
 * typedef ... x;
 * ->
 * typedef ... x; extern x __fscw_typedefname_x;
 *)
let add_variable_of_typedefname decls =
  let typedefnames = ref [] in
  let decls' = fold_left_global_var
    (fun list name specs declarator ->
       match List.mem (StorageClass Typedef) specs with
         false -> list
       | true ->
         let name' = "__fscw_typedefname_" ^ name in
         typedefnames := (name, name')::!typedefnames;
         (locput_dummy
           (PdeclVariable(
             [StorageClass C_abstree.Extern; TypeSpec (PtypespecAlias name)],
             [PinitDecl(PdeclIdent name', None)])))::list)
    [] decls
  in
  !typedefnames, decls@decls'

let get_gb_by_id ~genv id =
  let _, gb =
    List.find
      (fun (id', _) -> id = id')
      genv.global_declarations
  in
  gb

let parse_global_declarations decl =
  let dependency = list_type_dependency decl in
  let orig = list_decl decl in
  let typedefnames, decl = add_variable_of_typedefname decl in
  let genv, t = C_typing.parse_global_declarations decl in
  let gv =
    List.map
      (fun (id, gb) ->
         Wl.WLDeclVar(id, gb.gbind_type, gb.gbind_type))
      genv.global_declarations
  in
  let gvt =
    List.map
      (fun (id, vid) ->
         let gb = get_gb_by_id ~genv vid in
         Wl.WLTypedef(id, gb.gbind_type))
      typedefnames
  in
  let gvs = ref [] in
  Util.earray_iteri
    (fun i desc ->
      let tag = find_struct_tag ~genv i in
      let name = try match List.assoc "named" desc.str_extension with
            C_abstree.Estring name -> Some name
          | _ -> assert false
        with
          Not_found -> None
      in
      gvs := Wl.WLStruct { wl_struct_tag = tag; wl_struct_name = name; wl_struct_id = i}::!gvs)
    genv.Ctt_abstree.struct_table;
    genv, t, gv @ gvt @ !gvs, dependency, orig
