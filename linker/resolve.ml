(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2005-2006. *)

open Util
open Linker_types
open Parse_typeinfo

include Debug.Install (struct let category = 200 end)

module StrDesc_Set = Set.Make(struct type t = struct_desc let compare = compare end)

type equi_struct = struct_desc * StrDesc_Set.t

type linker_environment = {
    hash : string;
    modules : (string * module_info) list;
    struct_decls : (struct_desc, string * string * struct_declaration_info) Hashtbl.t;
    provided_decls : (string, string * ltype) Hashtbl.t;
    bss_decls : (string, string * ltype) Hashtbl.t;
    mutable equi_struct_rel : equi_struct list;

    (* updated during unification *)
    rename_func_set : (string, ltype) Hashtbl.t;
    referred_provided_decls : (string, string * ltype) Hashtbl.t;
  }

type t = { 
    new_module_hash : string;
    rename_list : 
      (struct_desc, struct_desc) Hashtbl.t;
    renamed_typeinfo :
      (ltype, ltype) Hashtbl.t;
    required_typeinfo :
      (ltype, unit) Hashtbl.t;
    provided_renamed_identifiers : (string * ltype * ltype) list;
    requested_renamed_identifiers : (string * ltype * ltype) list;
    struct_table :
      (int * (string option * (string * string * struct_declaration_info))) list;
    provided_values : (string, string * ltype) Hashtbl.t;
    instanciating_bss_values : (string, string * ltype) Hashtbl.t;
    renamed_unknown_functions : (string * ltype) list;
    required_native_libraries : string list;
    linked_modules : (string, module_info) Hashtbl.t;
  }

exception Unification_failure_internal
exception Unification_failure of ltype * ltype

let assert_unify b = if not b then raise Unification_failure_internal

let strof_strdesc = function
    LTSnamed s -> s
  | LTShashed(h,s) -> h ^ "::" ^ string_of_int s

let add_eqstr ~env s1 s2 = 
  (* s1, s2 is principle *)
  (* in adding stage, s1 should be new principal *)
  let iter_found first_occurence another_equi =
    let rec iter = function
	[] ->
	  dprintf 6 "  we do not found %s as principal. make %s principal, let %s be alias."
	    (strof_strdesc another_equi) (strof_strdesc s1) (strof_strdesc s2);
	  [s1, StrDesc_Set.add s2 (snd first_occurence)]
      | h :: t ->
	  if fst h = another_equi then begin
	    dprintf 6 "  we also find %s as principal. keep %s as principal, let %s be alias." 
	      (strof_strdesc another_equi) (strof_strdesc s1) (strof_strdesc s2);
	    (s1, 
	     StrDesc_Set.union
	       (StrDesc_Set.add s2 (snd first_occurence))
	       (snd h))::t
	  end else
	    h :: iter t
    in
    iter
  in

  let rec iter_not_found = function
      [] ->
	dprintf 6 "add_eqstr::iter_not_found: not found. take %s as principal, %s as alias." 
	  (strof_strdesc s1) (strof_strdesc s2);
	[s1, StrDesc_Set.singleton s2]
    | h :: t ->
	if fst h = s1 then begin
	  dprintf 6 "  we already has %s as principal. search for %s." 
	    (strof_strdesc s1) (strof_strdesc s2);
	  iter_found h s2 t
	end else if fst h = s2 then begin
	  dprintf 6 "  we already has %s as principal. search for %s." 
	    (strof_strdesc s2) (strof_strdesc s1);
	  iter_found h s1 t
	end else begin
	  h :: iter_not_found t
	end
  in
  dprintf 6 "add_eqstr: required to add relation %s <= %s" 
    (strof_strdesc s1) (strof_strdesc s2);
  env.equi_struct_rel <- iter_not_found env.equi_struct_rel

let get_struct_impl ~env s = 
  Hashtbl.find env.struct_decls s

let find_principal ~env s =
  let rec iter = function
      [] -> s
    | (p, l)::t ->
	if p = s then p
	else if StrDesc_Set.mem s l then p else iter t
  in iter env.equi_struct_rel

let rec try_check_eqtype ~env ?(left_nullarray_ok = false) t1 t2 = 
  try
    match t1, t2 with
      LTconcrete c1, LTconcrete c2 -> 
	assert_unify (c1 = c2)
    | LTarray(s1,et1), LTarray(s2,et2) ->
	assert_unify ((s1 = s2 || left_nullarray_ok && s1 = 0));
	try_check_eqtype ~env et1 et2
    | LTpointer et1, LTpointer et2 ->
	try_check_eqtype ~env et1 et2
    | LTstruct s1, LTstruct s2 -> begin
	if (s1 = s2) then () else
	let s1 = find_principal ~env s1 in
	let s2 = find_principal ~env s2 in
	if s1 = s2 then () else
	let (_, nam1, decl1) = get_struct_impl ~env s1 in
	let (_, nam2, decl2) = get_struct_impl ~env s2 in
	
	(* both are equally unnamed or named *)
	assert_unify (nam1 = nam2);
	
	match decl1, decl2 with
	  Sspecial, Sspecial ->
	    (* special structs should be name-equal *)
	    assert_unify false;
	| Sspecial, Sconcrete _ 
	| Sconcrete _, Sspecial ->
	    (* special structs should not be concrete *)
	    assert_unify false;
	| Sconcrete c1, Sconcrete c2 ->
	    add_eqstr ~env s1 s2; (* for recursive check *)
	    check_struct_equality ~env c1 c2
	| (Sconcrete _ | Sspecial), Sabstract ->
	    add_eqstr ~env s1 s2
	| Sabstract, (Sconcrete _ | Sspecial) ->
	    add_eqstr ~env s2 s1
	| Sabstract, Sabstract ->
	    add_eqstr ~env s1 s2
    end
    | LTfunction(at1,vp1,rt1), LTfunction(at2,vp2,rt2) ->
	assert_unify (vp1 = vp2);
	assert_unify (List.length at1 = List.length at2);
	List.iter2 (try_check_eqtype ~env) at1 at2;
	try_check_eqtype ~env rt1 rt2
    | _ -> assert_unify false
  with
    Unification_failure_internal -> raise (Unification_failure (t1, t2))

and check_struct_equality ~env l1 l2 = 
  assert_unify (List.length l1 = List.length l2);
  List.iter2
    (fun (nam1, t1) (nam2, t2) ->
      assert_unify (nam1 = nam2);
      try_check_eqtype ~env t1 t2) l1 l2

let check_eqtype ~env ~symbol ?left_nullarray_ok t1 t2 =
  try
    try_check_eqtype ~env ?left_nullarray_ok t1 t2
  with
    Unification_failure(t1, t2) ->
      failwith_p "error while resolving %s:\n  type %s is not compatible with %s"
	symbol (strof_ltype t1) (strof_ltype t2)

let rec strof_strdecl = 
  function
      [] -> ""
    | (n, t) :: tl -> " " ^ (strof_ltype t) ^ " " ^ n ^ ";" ^ strof_strdecl tl

let gather_struct_decls mods = 
  let ht = Hashtbl.create 16 in
  List.iter
    (fun (modname, modinfo) ->
      List.iter
	(fun (desc, snam, dcl) ->
	  if Hashtbl.mem ht desc then
	    match desc, dcl, Hashtbl.find ht desc with
	      LTSnamed _, (Sspecial | Sconcrete _), (_, _, Sabstract) ->
		Hashtbl.replace ht desc (modname, snam, dcl);
	    | LTSnamed _, Sabstract, (_, _, (Sspecial | Sconcrete _ | Sabstract)) ->
		()
	    | LTSnamed _, Sconcrete t, (omod, _, Sconcrete t') -> 
		if t = t' then ()
		else
		  failwith_p
		    "duplicate declaration of struct %s (%s) between %s / %s: \n declaration do not syntactically match\n   %s\n / %s"
		    snam (strof_strdesc desc)
		    modname omod
		    (strof_strdecl t)
		    (strof_strdecl t')
	    | _, _, (omod, _, _) ->
		failwith_p
		  "duplicate declaration of struct %s %s between %s / %s"
		  snam (strof_strdesc desc)
		  modname omod
	  else
	    Hashtbl.add ht desc (modname, snam, dcl))
	modinfo.mi_structs) mods;
  ht

let gather_provided_values mods = 
  let ht = Hashtbl.create 16 in
  List.iter
    (fun (modname, modinfo) ->
      List.iter
	(fun (nam, typ, _attr) ->
	  if Hashtbl.mem ht nam then
	    failwith_p
	      "panic: duplicate declaration of value %s between %s / %s"
	      nam modname (fst (Hashtbl.find ht nam))
	  else begin
	    Hashtbl.add ht nam (modname, typ);
	  end)
	modinfo.mi_provided_values) mods;
  ht

let unify_typeof_bss_values, unify_typeof_referred_values = 
  let iter ~env ~modname ~bss l =
    dprintf_progress "(%d symbols.)" (List.length l);
    List.iter
      (fun (nam, rtyp) ->
	if Hashtbl.mem env.provided_decls nam then begin
	  let (mname, dtyp) as p = Hashtbl.find env.provided_decls nam in
	  if get_debug_flags () >= 6 then 
	    dprintf 6 "looking for symbol %s as %s ... found %s" nam (strof_ltype rtyp) (strof_ltype dtyp);
	  Hashtbl.replace env.referred_provided_decls nam p;
	  if rtyp = LTunknownfunc then begin
	    match dtyp with
	      LTfunction _ ->
		dprintf 6 "found implementation of unknown function %s as type %s"
		  nam (strof_ltype dtyp);
		Hashtbl.add env.rename_func_set nam dtyp
	    | _ ->
		failwith_p
		  "%s is used as unknown function in module \"%s\", but provided as value in module \"%s\""
		  nam modname mname
	  end else
	    check_eqtype ~env ~left_nullarray_ok:true ~symbol:nam rtyp dtyp
	end else begin
	  if bss then begin
	    dprintf 6 "instanciating BSS %s as %s" nam (strof_ltype rtyp);
	    Hashtbl.add env.bss_decls nam (modname, rtyp);
	    Hashtbl.add env.provided_decls nam (modname, rtyp)
	  end else
	    failwith_p "panic: symbol %s not provided by any modules" nam
	end)
      l
  in
  (fun ~env modules ->
    List.iter
      (fun (modname, modinfo) ->
	dprintf_progress "[BSS:%s]" modname;
	iter ~env ~modname ~bss:true modinfo.mi_bss_values)
      modules),
  (fun ~env modules ->
    List.iter
      (fun (modname, modinfo) ->
	dprintf_progress "[EXT:%s]" modname;
	iter ~env ~modname ~bss:false modinfo.mi_required_values)
      modules;
    (* add main as referred value, if provided *)
    if not (Hashtbl.mem env.referred_provided_decls "main") &&
      Hashtbl.mem env.provided_decls "main" then
      Hashtbl.add env.referred_provided_decls "main"
	(Hashtbl.find env.provided_decls "main");
    ())

let check_equi_relation ~env = 
  (* equi_relation should not contain types from the same module twice. *)
  List.iter
    (fun (p, l) ->
      let t = Hashtbl.create 16 in
      let check = function
	  LTSnamed _ -> ()
	| LTShashed (s, _) ->
	    if Hashtbl.mem t s then
	      failwith_p "two structs from the module %s have been unified during type checking" s
	    else 
	      Hashtbl.replace t s ()
      in
      check p;
      StrDesc_Set.iter check l)
    env.equi_struct_rel

let dump_equi_relation c ~env = 
  prerr_endline ("\nEqui-type list" ^ c ^ ":\n");
  List.iter
    (fun (p, l) ->
      prerr_endline (strof_strdesc p);
      StrDesc_Set.iter
	(fun v -> prerr_endline ("  = " ^ strof_strdesc v)) l;
      prerr_newline ())
    env.equi_struct_rel

exception Exit_StrDesc of struct_desc

let make_rename_list env = 
  let ht = Hashtbl.create 16 in
  let find_representive p l = 
    let check = function
      | (LTSnamed s as r) -> raise (Exit_StrDesc r)
      | _ -> ()
    in
    try
      check p;
      StrDesc_Set.iter check l;
      None
    with 
      Exit_StrDesc r -> Some r
  in
  List.iter
    (fun (p, l) ->
      match find_representive p l with
	None ->
	  StrDesc_Set.iter (fun s -> Hashtbl.replace ht s p) l
      | Some r ->
	  StrDesc_Set.iter
	    (fun s -> if s <> r then Hashtbl.replace ht s r)
	    (StrDesc_Set.add p l))
    env.equi_struct_rel;
  ht

let dump_rename_list renlist = 
  prerr_endline "\nType rename list:\n";
  Hashtbl.iter
    (fun r t ->
      Printf.eprintf "%s ==> %s\n" (strof_strdesc r) (strof_strdesc t)) renlist;
  prerr_newline ()

let dump_typeinfo_list tlist = 
  prerr_endline "\nTypeInfo list:\n";
  let l = Hashtbl.fold (fun x () y -> x::y) tlist [] in
  let l = List.sort compare l in
  List.iter
    (fun r -> prerr_endline (strof_ltype r)) l;
  prerr_newline ()

let dump_decl_rename_list s renlist = 
  prerr_endline ("\n" ^ s ^ " declaration rename list:\n");
  List.iter
    (fun (id, ot, nt) ->
      Printf.eprintf "%s: %s ==> %s\n" id (strof_ltype ot) (strof_ltype nt)) renlist;
  prerr_newline ()

let dump_renamed_func_list renlist = 
  prerr_endline "\nRenamed unknown functions:\n";
  List.iter
    (fun (id, t) ->
      Printf.eprintf "%s : %s\n" id (strof_ltype t)) renlist;
  prerr_newline ()

let rec rename_type ren
    = function
	(LTconcrete _ as c) -> c
      | LTpointer t -> LTpointer (rename_type ren t)
      | LTarray(s, t) -> LTarray(s, rename_type ren t)
      | LTfunction(at,vp,rt) -> LTfunction(List.map (rename_type ren) at, vp, rename_type ren rt)
      | LTstruct s -> LTstruct (rename_struct_type ren s)
      | LTunknownfunc -> LTunknownfunc
and rename_struct_type ren s = 
  try
    Hashtbl.find ren s
  with
    Not_found -> s

let generate_renamed_typeinfo_list ren mods = 
  let gtbl = Hashtbl.create 16 in
  let rtbl = Hashtbl.create 16 in
  List.iter
    (fun (modname, modinfo) ->
      List.iter
	(fun typ ->
	  let ntyp = rename_type ren typ in
	  Hashtbl.replace gtbl ntyp ();
	  if ntyp <> typ then
	    Hashtbl.replace rtbl typ ntyp)
	modinfo.mi_required_typeinfo) mods;
  rtbl, gtbl

let generate_provided_values_table ren vals = 
  let ht = Hashtbl.create 16 in
  Hashtbl.iter
    (fun nam (modname, typ) ->
      Hashtbl.replace ht nam (modname, rename_type ren typ))
    vals;
  ht

let dump_provided_values s table = 
  prerr_endline ("\nList of " ^ s ^ " values:\n");
  Hashtbl.iter
    (fun id (m, lt) ->
      Printf.eprintf "%s: %s (defined in %s)\n" id (strof_ltype lt) m) table;
  prerr_newline ()

let generate_provided_renamed_decls ren mods = 
  let g = Glist.empty () in
  List.iter
    (fun (modname, modinfo) ->
      List.iter
	(fun (nam, typ, _attr) ->
	  let new_type = rename_type ren typ in
	  if new_type <> typ then
	    Glist.put g (nam, typ, new_type))
	modinfo.mi_provided_values) mods;
  Glist.to_list g

let generate_requested_renamed_decls ren mods = 
  let g = Glist.empty () in
  List.iter
    (fun (modname, modinfo) ->
      let f l = 
	List.iter
	  (fun (nam, typ) ->
	    let new_type = rename_type ren typ in
	    if new_type <> typ then
	      Glist.put g (nam, typ, new_type))
	  l
      in 
      f modinfo.mi_required_values;
      f modinfo.mi_bss_values
    ) mods;
  Glist.to_list g

let generate_rename_func_set ren s =
  let g = Glist.empty () in
  Hashtbl.iter
    (fun id lt ->
      Glist.put g (id, rename_type ren lt))
    s;
  Glist.to_list g

let generate_renamed_struct_decl_list ~env ren strdecls = 
  let new_rename_tbl = Hashtbl.create 16 in
  let new_decls = Glist.empty () in
  let idctr = ref 0 in
  Hashtbl.iter
    (fun desc declinfo ->
      dprintf 5 "\niter : %s" (strof_strdesc desc);
      if Hashtbl.mem new_rename_tbl desc then begin
	match desc with
	  LTSnamed s -> ()
	| _ -> assert (rename_struct_type ren desc = desc);
      end else begin
	let canon_desc = rename_struct_type ren desc in
	dprintf 5 "canon : %s" (strof_strdesc canon_desc);
	let updated_desc = 
	  try 
	    Hashtbl.find new_rename_tbl canon_desc
	  with
	    Not_found -> begin
	      let newid = (incr idctr; !idctr) in
	      let name, updated_desc = 
		match canon_desc with
		  LTShashed _ -> None, LTShashed (env.hash, newid)
		| LTSnamed s -> Some s, canon_desc
	      in
	      Glist.put new_decls (newid, (name, Hashtbl.find env.struct_decls canon_desc));
	      Hashtbl.replace new_rename_tbl canon_desc updated_desc;
	      updated_desc
	    end
	in
	dprintf 5 "upd : %s" (strof_strdesc updated_desc);
	Hashtbl.replace new_rename_tbl desc updated_desc
      end)
    strdecls;
  new_rename_tbl,
  List.map
    (fun (id, (namopt, (modnam, orgnam, decl))) ->
      let decl =
	match decl with
	  Sconcrete l ->
	    Sconcrete
	      (List.map
		 (fun (n, t) -> n, rename_type new_rename_tbl t)
		 l)
	| (Sabstract | Sspecial) -> decl
      in
      (id, (namopt, (modnam, orgnam, decl))))
    (Glist.to_list new_decls)

let dump_renamed_struct_decl_list decls = 
  prerr_endline "\nStruct list after rename:\n";
  List.iter
    (fun (id, (nam, (modnam, origname, fldinfos))) ->
      prerr_endline
	(string_of_int id ^ 
	 (match nam with None -> ", "
	 | Some s -> " named \"" ^ s ^ "\", ")
	 ^ "defined in " ^ modnam ^ " as \"" ^ origname ^ "\"");
  match fldinfos with
	Sspecial -> prerr_endline "  Special struct.\n"
      | Sabstract -> prerr_endline "  Abstract (??).\n"
      | Sconcrete l ->
	  prerr_endline 
	    ("  { " ^
	     String.concat ", "
	       (List.map
		  (fun (fn, ft) ->
		    fn ^ ": " ^ strof_ltype ft)
		  l)
	       ^ "}.\n"))
    decls

let collect_required_native_libraries mods = 
  let libs = Hashtbl.create 16 in
  let visited = Hashtbl.create 16 in
  let libdeps = Hashtbl.create 16 in
  List.iter
    (fun (modname, modinfo) ->
      List.iter
	(fun (nam, _typ, attr) ->
	  let rec iter = function
	      [] -> ()
	    | [h] -> Hashtbl.replace libs h ();
	    | h::((tl::_) as l) ->
		Hashtbl.replace libs h ();
		Hashtbl.add libdeps h tl;
		iter l
	  in
	  iter attr.attr_required_native_libraries)
	modinfo.mi_provided_values) mods;
  
  let accum = ref [] in
  let rec emit parents i = 
    if List.mem i parents then
      failwith_p
	"cannot resolve dependency of native library %s: cyclic dependency found" i;
    if Hashtbl.mem visited i then () else begin
      Hashtbl.replace visited i true;
      if Hashtbl.mem libdeps i then begin
	let parents' = i::parents in
	let l = Hashtbl.find_all libdeps i in
	List.iter (emit parents') l;
      end;
      accum := i :: !accum (* C libraries must be in the "reverse" order of dependencies *)
    end
  in
  Hashtbl.iter (fun i () -> emit [] i) libs;
  !accum

let try_reduce_struct = ref true

let optimize_structs ~env =
  let cnt = ref 0 in
  let try_reduce_struct s1 s2 = 
    let s1' = find_principal ~env s1 in
    let s2' = find_principal ~env s2 in
    dprintf 9 "p(%s) = %s, p(%s) = %s"
      (strof_strdesc s1) (strof_strdesc s1') (strof_strdesc s2) (strof_strdesc s2');
    if s1' = s2' then begin
      dprintf 5 "%s and %s is already unified." (strof_strdesc s1) (strof_strdesc s2);
      true
    end
    else
      let old_eqrel = env.equi_struct_rel in
      try
	try_check_eqtype ~env (LTstruct s1) (LTstruct s2);
	incr cnt;
	dprintf 5 "%s and %s unified." (strof_strdesc s1) (strof_strdesc s2);
	true
      with
	Unification_failure (t1, t2) ->
	  dprintf 5 "%s and %s cannot be unified:\n  type %s is not compatible with %s. reverted."
	    (strof_strdesc s1) (strof_strdesc s2)
	    (strof_ltype t1) (strof_ltype t2);
	  env.equi_struct_rel <- old_eqrel;
	  false
  in
  
  let desc_of_str nam c = 
    let e s = string_of_int (String.length s) ^ "_" ^ s ^ "_" in
    let rec d = function (* stringify only "fixed" parts *)
	LTconcrete c -> c
      | LTpointer t -> "P" ^ d t
      | LTarray(i, t) -> "A" ^ d t ^ "_" ^ string_of_int i
      | LTfunction(t, op, rt) ->
	  "F" ^ String.concat "" (List.map d t)
	  ^ (if op then "V" else "") ^ "_" ^ d rt
      | LTstruct (LTSnamed n) -> "Sn" ^ e n ^ "_"
      | LTstruct (LTShashed _ as s) ->
	  let (_, nam, d) = get_struct_impl ~env s in "Sh_" ^ e nam
      | LTunknownfunc -> assert false
    in
    String.concat ""
      (e nam :: List.map (fun (nam, t) -> e nam ^ d t) c)
  in

  let ht = Hashtbl.create 127 in
  let ht_visited = Hashtbl.create 127 in
  let bfr_cnt = ref 0 in
  let bfr_cnt2 = ref 0 in
  Hashtbl.iter
    (fun s (_, nam, decl) ->
      incr bfr_cnt;
      let s' = find_principal ~env s in
      if s <> s' then () else begin
	incr bfr_cnt2;
	match decl with
	  Sconcrete d ->
	    dprintf 9 "p(%s) = %s... adding."
	      (strof_strdesc s) (strof_strdesc s');
	    Hashtbl.add ht (desc_of_str nam d) s
	| Sabstract | Sspecial -> ()
      end)
    env.struct_decls;
  
  dprintf 3 "%d structs in program before linkage, %d before optimization." !bfr_cnt !bfr_cnt2;
  let t = Unix.gettimeofday () in

  let tried_pairs = ref 0 in

  Hashtbl.iter
    (fun desc _ ->
      if Hashtbl.mem ht_visited desc then () else begin
	Hashtbl.add ht_visited desc ();
	let l = Hashtbl.find_all ht desc in
	if get_debug_flags () >= 6 then
	  Printf.eprintf "%40s (%d): %s\n"
	    desc (List.length l) 
	    (String.concat ", " (list_map strof_strdesc l));
	
	let rec outer = function
	    [] | [_] -> ()
	  | h::t ->
	      dprintf 9 "  outer: %s :: [%s]"
		(strof_strdesc h) (String.concat ", " (list_map strof_strdesc t));
	      let rec inner rest = function
		  [] -> rest
		| i::t -> 
		    dprintf 9 "    inner: %s <=> %s" (strof_strdesc h) (strof_strdesc i);
		    incr tried_pairs;
		    if try_reduce_struct h i then
		      inner rest t
		    else inner (i::rest) t
	      in
	      let n = inner [] t in
	      outer n
	in
	outer l
      end)
    ht;

  dprintf 3 "%d structs reduced by optimization, tried %d pairs in %g sec."
    !cnt !tried_pairs (Unix.gettimeofday () -. t);
  ()

let resolve module_list = 
  let debug = get_debug_flags () >= 4 in
  let start_time = Unix.gettimeofday () in
  dprintf_start "resolving symbols...";

  let struct_decls = gather_struct_decls module_list in
  let value_decls = gather_provided_values module_list in
  let hash = Printf.sprintf "%08x" (Hashtbl.hash_param max_int max_int (struct_decls, value_decls)) in
  if debug then Printf.eprintf "Module hash is %s\n\n" hash;
  let env = {
    hash = hash;
    modules = module_list;
    struct_decls = struct_decls;
    provided_decls = value_decls;
    equi_struct_rel = [];

    bss_decls = Hashtbl.create 16;
    rename_func_set = Hashtbl.create 16;
    referred_provided_decls = Hashtbl.create 16  
  } in
  let rename_func_set = Hashtbl.create 16 in

  unify_typeof_bss_values ~env module_list;
  unify_typeof_referred_values ~env module_list;
  check_equi_relation ~env;
  if debug then dump_equi_relation "" ~env;

  if !try_reduce_struct then begin
    dprintf_progress "(OPT)";
    optimize_structs ~env;
    if debug then dump_equi_relation " after optimization" ~env
  end;

  dprintf_progress "(PRN)";
  let pre_rename_list = make_rename_list env in
  if debug then dump_rename_list pre_rename_list;

  dprintf_progress "(RSL)";
  let rename_list, new_struct_decls = 
    generate_renamed_struct_decl_list ~env pre_rename_list env.struct_decls in
  dprintf 3 "%d unique structs in the program." (List.length new_struct_decls);
  if debug then begin
    dump_renamed_struct_decl_list new_struct_decls;
    dump_rename_list rename_list;
  end;

  dprintf_progress "(RTI)";
  let renamed_typeinfo, new_required_typeinfos = 
    generate_renamed_typeinfo_list rename_list module_list in
  if debug then dump_typeinfo_list new_required_typeinfos;

  dprintf_progress "(RDC)";
  let provided_renamed_decls = 
    generate_provided_renamed_decls rename_list module_list in
  if debug then dump_decl_rename_list "provided" provided_renamed_decls;

  let required_renamed_decls = 
    generate_requested_renamed_decls rename_list module_list in
  if debug then dump_decl_rename_list "required" required_renamed_decls;

  dprintf_progress "(PVT)";
  let provided_values_table = 
    generate_provided_values_table rename_list env.referred_provided_decls in
  if debug then dump_provided_values "(referred) provided" provided_values_table;

  dprintf_progress "(BVT)";
  let instanciating_bss_values_table = 
    generate_provided_values_table rename_list env.bss_decls in
  if debug then dump_provided_values "instanciated BSS" instanciating_bss_values_table;

  dprintf_progress "(RFT)";
  let rename_func_table =
    generate_rename_func_set rename_list env.rename_func_set in
  
  if debug then dump_renamed_func_list rename_func_table;

  dprintf_progress "(CNL)";
  let native_libraries = 
    collect_required_native_libraries module_list in

  let linked_modules =
    let ht = Hashtbl.create 17 in
    List.iter
      (function mn, mi -> Hashtbl.add ht mn mi) module_list;
    ht
  in

  dprintf_end "done (%.3f sec)." (Unix.gettimeofday() -. start_time);
  { new_module_hash = hash;
    rename_list = rename_list;
    renamed_typeinfo = renamed_typeinfo;
    required_typeinfo = new_required_typeinfos;
    provided_renamed_identifiers = provided_renamed_decls;
    requested_renamed_identifiers = required_renamed_decls;
    struct_table = new_struct_decls;
    provided_values = provided_values_table;
    instanciating_bss_values = instanciating_bss_values_table;
    renamed_unknown_functions = rename_func_table;
    required_native_libraries = native_libraries;
    linked_modules = linked_modules;
  }

let f modules = 
  resolve modules
