(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

open Util
open Ctt_abstree
open Big_int

(* global names *)

type globalnames = 
    GNtypeinfo of c_type (* typeinfo *)
  | GNstorage of c_type * big_int option (* fsc_storage_<t>_<s>, fsc_initU_<s> *)
  | GNbasecast of c_type (* set_base_castflag_<t> *)
  | GNaddptr of c_type (* set_base_castflag_<t> *)
  | GNalloc of c_type (* alloc_block_<t> *)
  | GNgetrealofs of c_type
  | GNrwmeth of c_type

module T = 
  struct
    type t = globalnames
    let equal a b = match a, b with
    | GNstorage (a,Some sa), GNstorage (b,Some sb) -> 
	C_typing.equal_type a b && eq_big_int sa sb
    | GNstorage (a,None), GNstorage (b,None)
    | GNtypeinfo a, GNtypeinfo b
    | GNbasecast a, GNbasecast b
    | GNaddptr a, GNaddptr b
    | GNalloc a, GNalloc b
    | GNgetrealofs a, GNgetrealofs b
    | GNrwmeth a, GNrwmeth b
      -> C_typing.equal_type a b
    | _ -> false
    let hash = Hashtbl.hash
  end

module H = Hashtbl.Make(T)

let registry = H.create 16

let remove_signedness = function
    Tchar | Tschar | Tuchar -> Tchar
  | Tshort | Tushort -> Tshort
  | Tint | Tuint -> Tint
  | Tlong | Tulong -> Tlong
  | Tlonglong | Tulonglong -> Tlonglong
  | Tdouble -> Tdouble
  | Tlongdouble ->
      if Fsc_config.emit_downcoerce_longdouble then Tdouble else Tlongdouble
  | Tfloat -> Tfloat

let rec remove_flags_type t = 
  let t = { t with ct_const_p = false; ct_volatile_p = false } in
  match t.ct_ty with
    Tpointer t2 ->
      { t with ct_ty = Tpointer (remove_flags_type t2) }
  | Tvoid | Tstruct _ | Tabstract _ -> t
  | Tbuiltin bt -> {t with ct_ty = Tbuiltin (remove_signedness bt) }
  | Tfunction(at,v,rt) -> 
      { t with ct_ty = Tfunction(list_map remove_flags_type at, v, remove_flags_type rt) }
  | Tarray(t2,sz) ->
      { t with ct_ty = Tarray(remove_flags_type t2, sz) }

let canonicalize = function
    GNtypeinfo(t) -> GNtypeinfo(remove_flags_type t)
  | GNstorage(t,s) -> GNstorage(remove_flags_type t, s)
  | GNbasecast(t) -> GNbasecast(remove_flags_type t)
  | GNaddptr(t) -> GNaddptr(remove_flags_type t)
  | GNalloc(t) -> GNalloc(remove_flags_type t)
  | GNgetrealofs(t) -> GNgetrealofs(remove_flags_type t)
  | GNrwmeth(t) -> GNrwmeth(remove_flags_type t)

let rec require_name n = 
  let n = canonicalize n in
  if not (H.mem registry n) then begin
    H.add registry n ();
    traverse_depend n
  end
  else
    ()

and traverse_depend = function
    GNstorage (t, _) ->
      require_name (GNtypeinfo t)
  | GNtypeinfo (t) -> begin
      require_name (GNalloc t);
      match t.ct_ty with
	Tvoid | Tabstract _ | Tbuiltin _ -> ()
      | Tpointer(p) -> 
	  require_name (GNtypeinfo p)
      | Tfunction(at,vp,rt) ->
	  require_name (GNtypeinfo rt);
	  List.iter (fun t -> require_name (GNtypeinfo t)) at
      | Tstruct(id) -> ()
      | Tarray _ -> failwith_p "panic: record_globalnames:1930: %a should not appear here" Ctt_formatter.pp_c_type t (*; assert false*)
  end
  | GNbasecast {ct_ty = Tpointer t} -> begin
      match t.ct_ty with
	Tbuiltin _ | Tvoid | Tabstract _ -> ()
      | _ ->
	  require_name (GNtypeinfo t)
  end
  | GNbasecast {ct_ty = Tstruct t} -> ()
  | GNaddptr ({ct_ty = Tpointer t} as tp) -> begin
      match t.ct_ty with
	Tbuiltin _ | Tvoid | Tabstract _ -> ()
      | _ ->
	  require_name (GNbasecast tp);
  end
  | GNaddptr ({ct_ty = Tstruct _} as t) -> ()
  | GNrwmeth ({ct_ty = Tstruct _} as t) -> require_name (GNtypeinfo t)
  | GNaddptr _ -> assert false
  | GNbasecast _ -> assert false
  | GNrwmeth _ -> assert false
  | GNgetrealofs _ | GNalloc _ -> ()

let to_list () = 
  let r = ref [] in
  H.iter (fun i () -> r := i :: !r) registry;
  !r

let add_struct_dependency ~genv () = 
  let rec walk_type t =
    match t.ct_ty with
      Tpointer _ ->
	require_name (GNbasecast t)
    | Tstruct s ->
	walk_struct s
    | Tarray(t,_) ->
	walk_type t
    | _ -> ()
  and walk_struct sid = 
    let fields = (Earray.get genv.struct_table sid).str_fields_byname in
    List.iter (fun (_, (ct, _)) -> walk_type ct) fields
  in
  List.iter 
    (function
	GNtypeinfo t -> begin
	  match t.ct_ty with
	    Tstruct sid -> walk_struct sid
	  | _ -> ()
	end
      | _ -> ())
    (to_list ())

let clear () =
  H.clear registry;
  require_name (GNtypeinfo (make_c_type (Tpointer type_char)))

(* string constants *)

let string_registry = Hashtbl.create 16
let string_id = ref 0

let get_string_constant_id (s : string) = 
  try
    Hashtbl.find string_registry s
  with Not_found ->
    require_name (GNstorage(type_char, Some (big_int_of_int (String.length s))));
    let id = !string_id in
    incr string_id;
    Hashtbl.add string_registry s id;
    id

let get_global_string_list () = 
  let r = ref [] in
  Hashtbl.iter (fun i s -> r := (i, s) :: !r) string_registry;
  !r
  
