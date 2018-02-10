(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2005-2006. *)

open Util
open Linker_types
open Parse_typeinfo
open Ctt_abstree
open Big_int_infix
open Locterm
open Ils

include Debug.Install (struct let category = 210 end)

type env = {
    new_module_hash : string;
    map_named_to_struct_id : (string * int) list;
    mutable required_basecast_functions : ltype list;
    modules : (string, module_info) Hashtbl.t;
  }

let rec ctype_of_ltype ~env ~s_table =
  function
      LTconcrete s -> begin
	match s with
	  "c" -> type_unsigned_char
	| "s" -> type_unsigned_short
	| "i" -> type_unsigned_int
	| "l" -> type_unsigned_long
	| "q" -> type_unsigned_long_long
	| "f" -> type_float
	| "d" -> type_double
	| "v" -> type_void
	| _ -> failwith "ctype_of_ltype"
      end
    | LTpointer p -> make_c_type ~s_table (Tpointer (ctype_of_ltype ~env ~s_table p))
    | LTarray (s,vt) -> 
	make_c_type ~s_table (Tarray (ctype_of_ltype ~env ~s_table vt, 
			     if s = 0 then None else Some (big_int_of_int s)))
    | LTfunction(at,vp,rt) ->
	make_c_type ~s_table
	  (Tfunction
	     (list_map (ctype_of_ltype ~env ~s_table) at,
	      vp, ctype_of_ltype ~env ~s_table rt))
    | LTstruct (LTShashed(h,i)) ->
	if h <> env.new_module_hash then
	  failwith_p
	    "ctype_of_ltype: panic: bad module hash %s (must be %s)"
	    h env.new_module_hash;
	make_c_type ~s_table (Tstruct i)
    | LTstruct (LTSnamed s) ->
	make_c_type ~s_table (Tstruct (List.assoc s env.map_named_to_struct_id))
    | LTunknownfunc -> failwith "ctype_of_ltype: LTunknownfunc should not appear here"

let construct_struct_table ~env tbl = 
  let debug = !Sys.interactive || Debug.get_debug_flags ~category:210 >= 2 in
  let decide_order tbl = 
    let gather_strictly_depend_structs l = 
      let rec f = function
	  LTconcrete _ -> []
	| LTpointer _ -> []
	| LTfunction _ -> []
	| LTstruct (LTShashed (h,i)) ->
	    assert (h = env.new_module_hash);
	    [i]
	| LTstruct (LTSnamed s) -> [List.assoc s env.map_named_to_struct_id]
	| LTarray(s,t) -> f t
	| LTunknownfunc -> assert false
      in
      Util.map_flatten (fun (_, lt) -> f lt) l
    in
    let depends_list = 
      List.map
	(fun (id, (_, (_, _, declinfo))) ->
	  id, match declinfo with
	    Sabstract -> []
	  | Sspecial -> []
	  | Sconcrete l ->
	      gather_strictly_depend_structs l)
	tbl
    in
    let id_list = 
      List.map fst tbl
    in
    List.rev
      (Topological_sort.topological_sort
	 ~depends:(fun id -> Int_set.of_list (List.assoc id depends_list)) 
	 ~defines:(fun id -> Int_set.singleton id)
	 id_list)
  in
  let order = decide_order tbl in
  if debug then begin
    prerr_endline "output order:";
    prerr_endline (String.concat ", " (List.map string_of_int order));
  end;
  let stable = Earray.empty () in
  List.iter
    (fun id ->
      Earray.set stable id
	{ str_union_p = Struct;
	  str_size = None;
	  str_align = None;
	  str_fields = [];
	  str_fields_byname = [];
	  str_extension = [];
	  str_assignable = true;
	  str_loc = Locterm.dummy_location })
    order;
  let convert_fields s_table flds = 
    let ofs = ref zero_big_int in
    let align = ref unit_big_int in
    let l = Glist.empty () in
    List.iter
      (fun (nam, lt) ->
	if debug then begin
	  prerr_endline ("   " ^ nam ^ ": " ^ strof_ltype lt);
	end;
	let t = ctype_of_ltype ~env ~s_table lt in
	let s, a =
	  match t.ct_size, t.ct_align with
	    Some s, Some a -> s, a | _ -> assert false
	in
	assert ((mod_big_int s a) ==! zero_big_int);
	Glist.put l (!ofs, NormalField { sf_id = nam; sf_type = t; sf_size = s });
	begin 
	  match lt with
	    LTpointer _ ->
	      env.required_basecast_functions <- lt :: env.required_basecast_functions;
	  | _ -> ()
	end;
	if !align <! a then align := a;
	ofs := !ofs +! s;
      )
      flds;
    !ofs, !align, Glist.to_list l
  in  
  let name_table = Glist.empty () in
  let add_struct_to_table id = 
    if debug then begin
      prerr_endline ("output " ^ string_of_int id);
    end;
    let (namopt, (modnam, orgnam, dclinfo)) = List.assoc id tbl in
    Glist.put name_table (orgnam, id);
    let ctt_ext =
      match namopt with
	None -> []
      | Some s -> [ (* "external", C_abstree.Elist [];  (* todo: in future, named may be code-generated *) *)
		    "named", C_abstree.Estring s ]
    in
    let ctt_desc = 
      match dclinfo with
	Sabstract ->
	  { str_union_p = Struct;
	    str_size = None;
	    str_align = None;
	    str_fields = [];
	    str_fields_byname = [];
	    str_extension = ctt_ext;
	    str_assignable = false;
	    str_loc = Locterm.dummy_location }
      | Sspecial ->
	  { str_union_p = Struct;
	    str_size = None;
	    str_align = None;
	    str_fields = [];
	    str_fields_byname = [];
	    str_extension = ("external", C_abstree.Elist []):: ctt_ext;
	    str_assignable = false;
	    str_loc = Locterm.dummy_location }
      | Sconcrete dcl ->
	  let sz, al, fields = convert_fields stable dcl in
	  C_typing.update_fields_cache
	    { str_union_p = Struct;
	      str_size = Some sz;
	      str_align = Some al;
	      str_fields = fields;
	      str_fields_byname = [];
	      str_extension = ctt_ext;
	      str_assignable = true;
	      str_loc = Locterm.dummy_location }
    in
    Earray.set stable id ctt_desc;
  in
  List.iter add_struct_to_table order;
  stable, Glist.to_list name_table

let generate_funcstub ~env ~genv lt id = 
  let loc = Locterm.dummy_location in
  match lt with 
    LTfunction(at,vf,rt) -> begin
      let argnum = List.length at in
      let args = ExtList.List.init
	  argnum
	  (fun i -> "arg" ^ string_of_int (i + 1))
      in
      let ct = ctype_of_ltype ~env ~s_table:genv.struct_table lt in
      let stubf = 
	Translate_to_il3.generate_bridge_function ~genv ~loc (Global []) id ct args
      in
      let stubrec = Translate_to_il3.generate_function_stub ~genv ~loc (Global []) id ct args in
      let proto = 
	Locterm.locput_dummy
	  (Il3.IL3declVariable(Global [], Translate_to_il3.translate_c_type_genv ~genv ct,
			       Translate_to_il3.translate_global_function_name ~genv id ct,
			       None))
      in
      [proto; stubf; stubrec]
  end
  | _ -> []

let generate_funcstubs_for_stdlib ~env ~genv v = 
  let g = Glist.empty () in
  Hashtbl.iter
    (fun id (m, lt) ->
      if ((Hashtbl.find env.modules m).mi_attributes.is_stdlib &&
	  (String.length id < 10 || String.sub id 0 10 <> "__builtin_" ))
      then begin
	dprintf 6 "generating stub for %s" id;
	Glist.append g (generate_funcstub ~env ~genv lt id)
      end)
    v;
  let t = Glist.to_list g in
  dprintf 6 "link-stub: @\n%a@\n" (Il3_formatter.pp_il3_program Il3_formatter.pp_temp_id Il3_formatter.pp_ignore) t;
  dprintf 6 "link-stub: translating to ctt";
  let t = Il3_to_ctt.translate_from_raw_il3 ~genv t in
  dprintf 6 "link-stub: translating to ptree";
  let t = Ctt_to_ptree.convert_program ~genv ~emit_structs:false t in
  t

let type_void_void = make_c_type (Tfunction([], false, type_void))

let generate_stub_for_initializers ~env ~genv mods = 
  let g = Glist.empty () in
  Hashtbl.iter
    (fun name modinfo ->
      match modinfo.mi_attributes.need_initialize with
	None -> ()
      | Some s ->
	  let id = "fsc_modinit_" ^ s in
	  if not (C_abstree.is_valid_identifier id) then
	    failwith_p "link: invalid identifier for module init %s in file %s" s name
	  else ();
	  Glist.put g
	    (locput_dummy
	       (CTTstmtExpr
		  (make_expr ~loc:dummy_location
		     (CTTexpInvoke 
			(make_expr ~loc:dummy_location
			   (CTTexpVar (id, type_void_void))
			   type_void_void,
			 []))
		     type_void_void))))
    mods;
  let def = 
    locput_dummy
      (CTTdeclFunction
	 (Global [],
	  type_void_void,
	  "fsc_init_modules",
	  [],
	  locput_dummy
	    (CTTstmtCompound ([], Glist.to_list g))))
  in
  Ctt_to_ptree.convert_program ~genv ~emit_structs:false [def]

let generate_null_ilc_initializer ~env ~genv ct =
  let rec iter ct = 
    let desc = 
      match ct.ct_ty with
      | Tbuiltin (Tdouble | Tfloat | Tlongdouble) ->
	  ILSinitConstant(ct, ILSinitbaseNone, ILSinitofsFloat 0.0)
      | Tpointer _ | Tbuiltin _ -> ILSinitConstant(ct, ILSinitbaseNone, ILSinitofsInt zero_big_int)
      | Tarray(at, sz) -> ILSinitArray(ct, [ iter at ])
      | Tstruct id -> ILSinitStruct(ct, iter_struct id)
      | Tvoid | Tfunction _ | Tabstract _ -> assert false
    in
    locput_dummy desc
  and iter_struct id = 
    let sd = Earray.get genv.struct_table id in
    map_flatten
      (function
	  (ofs, NormalField { sf_id = nam; sf_type = t }) -> [ (nam, iter t) ]
	| (ofs, BitField _) -> assert false)
      sd.str_fields
  in
  iter ct

let generate_bss ~env ~genv v = 
  let g = Glist.empty () in
  Hashtbl.iter
    (fun id (m, lt) ->
      let ct = ctype_of_ltype ~env ~s_table:genv.struct_table lt in
      let init = generate_null_ilc_initializer ~env ~genv ct in
      let decl = 
	Locterm.locput_dummy
	  (Il.ILdeclVariable(Global [], ct, id, Some init))
      in
      Glist.put g decl)
    v;
  let t = Glist.to_list g in
  dprintf 6 "bss-decl: translating to il3";
  let t = Translate_to_il3.f ~genv t in
  dprintf 6 "bss-decl: @\n%a@\n" (Il3_formatter.pp_il3_program Il3_formatter.pp_temp_id Il3_formatter.pp_ignore) t;
  dprintf 6 "bss-decl: translating to ctt";
  let t = Il3_to_ctt.translate_from_raw_il3 ~genv t in
  dprintf 6 "bss-decl: translating to ptree";
  let t = Ctt_to_ptree.convert_program ~genv ~emit_structs:false t in
  t

let generate r = 
  let start_time = Unix.gettimeofday () in
  dprintf_start "Generating linker stubs...";
  Transutil.compiler_mode := Transutil.MultiLinker;
  let map_named_to_struct_id = 
    Util.map_flatten
      (fun (id, (optnam, _)) ->
	match optnam with
	  None -> []
	| Some s -> [s, id])
      r.Resolve.struct_table
  in
  let env = 
    { new_module_hash = r.Resolve.new_module_hash; 
      map_named_to_struct_id = map_named_to_struct_id;
      required_basecast_functions = [];
      modules = r.Resolve.linked_modules;
    } in
  let ctt_struct_table, struct_name_table = construct_struct_table ~env r.Resolve.struct_table in
  let genv = {
    Ctt_abstree.module_hash = env.new_module_hash;
    struct_table = ctt_struct_table;
    struct_name_table = struct_name_table;
    global_declarations = [];
  }
  in
  let genv = Update_struct_types.reduce_genv genv in
  let s_table = genv.struct_table
  in
  let funcstubs = generate_funcstubs_for_stdlib ~genv ~env r.Resolve.provided_values in
  let initializer_stub = generate_stub_for_initializers ~genv ~env r.Resolve.linked_modules in
  let bss_values = generate_bss ~genv ~env r.Resolve.instanciating_bss_values in
  List.iter
    (fun t ->
      Record_globalnames.require_name
	(Record_globalnames.GNbasecast 
	   (ctype_of_ltype ~env ~s_table t)))
    env.required_basecast_functions;
  Hashtbl.iter 
    (fun t () ->
      Record_globalnames.require_name 
	(Record_globalnames.GNtypeinfo
	   (ctype_of_ltype ~env ~s_table t)))
    r.Resolve.required_typeinfo;
  let rr = Add_support_funcs.f_standalone ~genv (funcstubs @ initializer_stub @ bss_values) in
  dprintf_end "done. (%.3f s)" (Unix.gettimeofday () -. start_time);
  rr
