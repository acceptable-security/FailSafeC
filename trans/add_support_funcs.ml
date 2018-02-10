(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006. *)

(* emit support routines and datatypes *)

open Big_int
open Locterm
open Fsc_config
open Ctt_abstree
open C_abstree
open Ctt_to_ptree
open Transutil
open Translate_to_il3
open Record_globalnames
open Util
open ExtList
open Format

include Debug.Install (struct let category = 130 end)
module D_indexed = Debug.Install (struct let category = 131 end)

let abi_revision = 6 (** the revision number of the abi of current compiler. *)
let allowed_minimum_object_abi_revision = 6 (** the lowest revision number of object which the current compiler accepts. *)
let required_minimum_compiler_abi_revision = 6 (** the lowest revision number which the currently-generating object requires. *)

let specially_emit_structs = ref []

let weak_attribute = ExtendedDeclSpec("__attribute__", Eplist [Eplist [Eident "weak"]])

let standard_weakness () = 
  match !Transutil.compiler_mode with
    TrustInterModule | StdlibImplementation -> [ weak_attribute ]
  | _ -> []

let indexed_ofsmap_expand_limit = ref 1

let use_gcc_extension = true

type struct_size = 
    {
     ss_continuous_p : bool;
     ss_real_size : int;
     ss_align : int;
     ss_alignpad : int;
   }

type environment = { 
    genv : Ctt_abstree.environment; 
    struct_size_cache : struct_size earray;
  }

type 'index extfield = 
    Iindexed of 'index
  | Inamed of identifier

let id_of_lvalue = function
    Il.ILlvVar (id, _) -> id
  | _ -> assert false

let d_emit_exp e = locput_dummy e
let d_emit_c_string s =
  d_emit_exp (PexpConstant (PconstString [Ctt_to_ptree.encode_c_string s]))
let d_emit_c_integer i =
  d_emit_exp (PexpConstant (PconstInteger (string_of_int i)))
let d_emit_c_big_integer i =
  d_emit_exp (PexpConstant (PconstInteger (string_of_big_int i)))
let d_emit_c_variable s =
  d_emit_exp (PexpVar s)
let d_emit_invokeident id args = 
  d_emit_exp (PexpInvoke (d_emit_exp (PexpVar id), args))
let c_charptr_type = Ptypename([TypeSpec (PtypespecBuiltin Char)],
			       PdeclPointer([], PdeclAnonymous))
let d_emit_binexp b e1 e2 = 
  d_emit_exp (PexpBinExpr(b, e1, e2))

let d_emit_stmt e = locput_dummy e
let d_emit_decl d = locput_dummy d

let d_emit_expstmt e = d_emit_stmt (PstmtExpr (Some e))
let d_emit_expstmt_t e = d_emit_expstmt (d_emit_exp e)
let d_emit_expreturn e = d_emit_stmt (PstmtReturn (Some e))

let d_emit_initexp e = locput_dummy (PinitExp e)
let d_emit_initlist e = locput_dummy (PinitList e)

let d_emit_compound ds l = 
  d_emit_stmt
    (PstmtCompound
       (list_map
	  (fun (t, e, i) ->
	    d_emit_decl
	      (PdeclVariable
		 (t,
		  [PinitDecl(e, Option.map (fun i -> locput ~loc:(locget i) (PinitExp i)) i)]))) ds,
	l))

let emit_pdeclspec_typedef n = [TypeSpec (PtypespecAlias n)]
let emit_pdeclspec_namedstruct s = [TypeSpec (PtypespecStruct (Struct, Some s, None, [], Locterm.dummy_location))]
let pdeclspec_size_t = emit_pdeclspec_typedef "size_t"
let pdeclspec_base_t = emit_pdeclspec_typedef "base_t"
let pdeclspec_ofs_t = emit_pdeclspec_typedef "ofs_t"
let pdeclspec_signed_dw_ofs_t = emit_pdeclspec_typedef "signed_dw_ofs_t"
let pdeclspec_unsigned_dw_ofs_t = emit_pdeclspec_typedef "unsigned_dw_ofs_t"
let pdeclspec_value_t = emit_pdeclspec_typedef "value"
let pdeclspec_ptrvalue_t = emit_pdeclspec_typedef "ptrvalue"
let pdeclspec_fsc_header = emit_pdeclspec_typedef "fsc_header"
let pdeclspec_typeinfo_t = emit_pdeclspec_typedef "typeinfo_t"
let pdeclspec_void = [TypeSpec (PtypespecBuiltin Void)]

let d_emit_cast_to_typedefname typnam exp = 
  d_emit_exp (PexpCast (Ptypename (emit_pdeclspec_typedef typnam, PdeclAnonymous), exp))

let name_of_struct ~genv id = 
  "struct_" ^ encoded_name_of_struct ~genv id
		
(**************** ACCESS METHODS ****************)

let translate_field_name = Translate_to_il3.translate_field_name

let int x = assert (is_int_big_int x); int_of_big_int x
let injsome = function
    None -> assert false
  | Some s -> s

type indexed_ofsmap_ofsexp = 
    (int * int) option * int
      (* index_id * step + offset *)

type 'a indexed_ofsmap_repeat_entry = 
    { ior_index : int;
      ior_elemsize : int;
      ior_count : int;
      ior_entry : (int * 'a) list
    }

type access_exp = 
    AEpointer of c_type * bool * expr
  | AEdelegate
  | AEdelegateDword of c_type * bool * expr * int

let path_to_exp base path emit_index = 
  List.fold_right
    (fun (fld, ft) l -> 
      match fld with
	Iindexed i ->
	  d_emit_exp (PexpArrayRef (l, emit_index i))
      | Inamed f ->
	  d_emit_exp (PexpField (l, f)))
    base
    path

let tokens_for_wordsize wsize = 
  match wsize, wsize / Fsc_config.sizeof_pointer with
    1,0 -> "byte", "byte", false | 2,0 -> "hword", "hword", false
  | _,1 -> "word", "value", true | _,2 -> "dword", "dvalue", true
  | _ -> assert false

(*================
   New version
================*)

type indexed_bytemap_entry = 
    IBMfield of (indexed_ofsmap_ofsexp extfield * c_type) list * int
  | IBMrepeat of indexed_bytemap_entry indexed_ofsmap_repeat_entry

let strof_ef ef = 
  String.concat ""
    (List.rev_map
       (fun (ef, ct) ->
	 match ef with
	   Iindexed (None, i) ->
	     sprintf "[%d]" i
	 | Iindexed (Some (idx, m), i) ->
	     sprintf "[i%d * %d + %d]" idx m i
	 | Inamed id -> sprintf ".%s" id)
       ef)

let rec dump_indexed_bytemap ~indent bm = 
  List.iter
    (function
	o, IBMfield(f, i) ->
	  eprintf "%s%3d: %s @@ %d@."
	    indent o (strof_ef f) i
      | o, IBMrepeat r ->
	  eprintf "%s%3d: REPEAT i%d in [0--%d] (size %d * %d)@."
	    indent o r.ior_index (r.ior_count - 1) r.ior_elemsize r.ior_count;
	  dump_indexed_bytemap ~indent:(indent ^ "   ") r.ior_entry)
    bm
	    
let make_indexed_bytemap ~genv id = 
  let rec iter_on_str id ~idx ~prefix_rev ~start_offset = 
    let str = Ctt_abstree.get_struct_desc ~genv id in
    let rec iter_on_fields ofs fields = 
      match fields with
	[] -> []
      | (fofs, fld) :: tl ->
	  let fofs = int fofs in
	  assert (ofs = fofs);
	  match fld with
	    BitField bf ->
	      assert false
	  | NormalField fld ->
	      let f = (Inamed (translate_field_name fld.sf_id), fld.sf_type) :: prefix_rev in
	      translate_entry ~idx f (fofs + start_offset) fld.sf_type (int fld.sf_size) @
	      iter_on_fields (ofs + int fld.sf_size) tl
    and translate_entry ~idx f ofs ty sz = 
      match ty.ct_ty with
	Tbuiltin _ | Tpointer _ ->
	  ExtList.List.init sz
	    (fun i -> i + ofs, IBMfield(f, i))
      | Tstruct id ->
	  iter_on_str id ~idx ~prefix_rev:f ~start_offset:ofs
      | Tarray(t, Some elemnum) ->
	  let elemnum = int elemnum in
	  let elemsize = int (injsome t.ct_size) in
	  assert (elemsize * elemnum = sz);
	  [ (ofs, IBMrepeat {ior_index = idx;
			     ior_elemsize = elemsize;
			     ior_count = elemnum;
			     ior_entry 
			       = (translate_entry ~idx:(idx + 1)
				    ((Iindexed (Some (idx, 1), 0), t) :: f)
				    0 t elemsize)}) ]
      | (Tabstract _|Tfunction (_, _, _)|Tvoid|Tarray(_,None)) -> assert false

    in iter_on_fields 0 str.str_fields
  in
  iter_on_str id ~prefix_rev:[] ~start_offset:0 ~idx:0

(** instanciate_bytemap_entry:
    - instanciate <i>th element, regarding to i<index>
      (be careful when i<index> is not outermost)
    - shift offset by <start_offset> + i*size
      (if shift is not required, pass 0 to both start_offset and size )
 *)
let rec instanciate_bytemap_entry index size start_offset i entry = 
  if D_indexed.get_debug_flags () > 2 then begin
    eprintf "    instanciate, idx %d, startofs %d, size %d, i=%d for@." index start_offset size i;
    dump_indexed_bytemap ~indent:"     |" entry
  end;
  let instanciate_fexp l = 
    List.map
      (fun x -> match x with
	Iindexed ((None, _)), _ | Inamed _, _ -> x
      | Iindexed ((Some (index',s),o)), t ->
	  if index <> index' then x else
	  Iindexed((None, i * s + o)), t)
      l
  in
  List.map
    (function
	o, IBMfield(l, ii) ->
	  start_offset + size * i + o, IBMfield(instanciate_fexp l, ii)
      | o, IBMrepeat r ->
	  start_offset + size * i + o, 
	  IBMrepeat { r with ior_entry = instanciate_bytemap_entry index 0 0 i r.ior_entry })
    entry

(** shift_bytemap_entry:
    - replace occurence of i<index> by (i<index> * m + i).
    - shift offset by <start_offset> * <size> bytes
 *)
let rec shift_bytemap_entry index size start_offset m i entry = 
  let instanciate_fexp l = 
    List.map
      (fun x -> match x with
	Iindexed ((None, _)), _ | Inamed _, _ -> x
      | Iindexed ((Some (index',s),o)), t ->
	  if index <> index' then x else
	  Iindexed((Some (index', s * m), s * i + o)), t) l
	    (* assigning <mx' + i> into <x> in <sx + o> gets
	       <s(mx' + i) + o = smx' + (si + o) *)
   in
   List.map
    (function
	o, IBMfield(l, i) ->
	  o + start_offset * size, IBMfield(instanciate_fexp l, i)
      | o, IBMrepeat r ->
	  o + start_offset * size,
	  IBMrepeat { r with ior_entry = shift_bytemap_entry index size 0 m i r.ior_entry })
    entry

(** offset_bytemap_entry:
    simply shift offsets by n *)

let offset_bytemap_entry n l =
   list_map
   (function o, v -> o + n, v) l

(** rotate_bytemap_entry:
    - pick first b bytes from repeation r starting at io
    - roughly speaking, translate 
        [a0---a<b-1> a<b>...a<n>]*r
      into
        a0---a<b-1> [a<b>...a<n> a<0>...a<b-1>]*(r-1) a<b>...a<n>
 *)
let rec rotate_elements_of_bytemap b io r = 
  let loops_to_expand = b / r.ior_elemsize in
  let bytes_to_rotate = b mod r.ior_elemsize in
  let loops_to_keep = r.ior_count - loops_to_expand - (if bytes_to_rotate > 0 then 1 else 0) in
  if loops_to_keep <= 0 then
    (* expand all elements *)
    List.flatten
      (List.init r.ior_count
	 (fun i ->
	   instanciate_bytemap_entry r.ior_index r.ior_elemsize io i r.ior_entry))
  else
    let rec split revacc = function
	((o, _)::_) as l when o = bytes_to_rotate -> List.rev revacc, l
      | (o, _)::_        when o > bytes_to_rotate -> assert false
      | (o, IBMfield _ as fld)::tl -> split (fld::revacc) tl
      | (o, IBMrepeat r as fld)::tl ->
	  if o + r.ior_elemsize * r.ior_count <= bytes_to_rotate then
	    split (fld::revacc) tl
	  else split revacc (rotate_elements_of_bytemap (bytes_to_rotate - o) o r @ tl)
      | [] -> assert false
    in
    let front, tail = split [] r.ior_entry in
    let front1 =
      if loops_to_expand > 0 then [io, IBMrepeat { r with ior_count = loops_to_expand }] else []
    in
    let front2 = 
      instanciate_bytemap_entry r.ior_index r.ior_elemsize io loops_to_expand front
    in
    let middle1 = 
      if loops_to_keep > 0 then
	[io + b, IBMrepeat
	   { r with
	     ior_count = loops_to_keep;
	     ior_entry = 
	     (offset_bytemap_entry (-bytes_to_rotate) tail) @
	     (offset_bytemap_entry (r.ior_elemsize - bytes_to_rotate)
		(shift_bytemap_entry r.ior_index 0 0 1 1 front))
	   }]
      else []
    in
    let tail1 = 
      if bytes_to_rotate = 0 then []
      else
	instanciate_bytemap_entry r.ior_index r.ior_elemsize io (r.ior_count - 1) tail
    in
    front1 @ front2 @ middle1 @ tail1

let rec combine_indexed_bytemap ~genv ~n map = 
  if !indexed_ofsmap_expand_limit < 1 then failwith "panic: indexed_ofsmap_expand_limit must at least 1";
  if D_indexed.get_debug_flags () > 2 then
    eprintf "== processing for size %d.@." n;
  let rec iter = function
      [] -> []
    | ((o, IBMfield _ as hd)::_) as l -> begin
	if D_indexed.get_debug_flags () > 2 then
	  dump_indexed_bytemap ~indent:">O " [hd];
	assert (o mod n = 0);
	let rec iloop ~acc = function
	    [] -> 
	      (* special case for the tail of dword table *)
	      if n = Fsc_config.sizeof_pointer * 2 &&
		(fst (List.hd acc)) mod n = Fsc_config.sizeof_pointer - 1 then
		List.rev acc @ iter []
	      else assert false
	  | (io, IBMfield(f, t) as fld)::tl ->
	      if D_indexed.get_debug_flags () > 2 then
		dump_indexed_bytemap ~indent:">> " [fld];
	      let acc = fld::acc in
	      if io mod n = n - 1 then
		List.rev acc @ iter tl
	      else
		iloop ~acc tl
	  | (io, IBMrepeat r as fld) :: tl ->
	      let pickb = n - (io - o) in
	      if pickb mod r.ior_elemsize <> 0 then begin
		if D_indexed.get_debug_flags () > 2 then begin
		  eprintf "==UNEVEN_SHIFT: pickb = %d, r.esize = %d, n%d, io%d, o%d@." pickb r.ior_elemsize n io o;
		  dump_indexed_bytemap ~indent:">>R" [fld];
		end;
		(* shift by innermost one element *)
		let r = rotate_elements_of_bytemap pickb io r in
		if D_indexed.get_debug_flags () > 2 then begin
		  dump_indexed_bytemap ~indent:"<<R" r;
		end;
		iloop ~acc (r @ tl)
	      end else
	      let pickn = pickb / r.ior_elemsize in
	      let en = min pickn r.ior_count in
	      if D_indexed.get_debug_flags () > 2 then
		dump_indexed_bytemap ~indent:">>E" [fld];
	      let expanded = 
		List.flatten
		  (List.init en
		     (fun i ->
		       instanciate_bytemap_entry r.ior_index r.ior_elemsize io i r.ior_entry))
	      in
	      let remainder = 
		if pickn >= r.ior_count then []
		else
		  [io + en * r.ior_elemsize,
		   IBMrepeat
		     { r with 
		       ior_count = r.ior_count - en;
		       ior_entry = shift_bytemap_entry r.ior_index r.ior_elemsize 0 1 en r.ior_entry }]
	      in
	      iloop ~acc (expanded @ (remainder) @ tl)
	in
	iloop [] l
    end
    | ((o, IBMrepeat r) as f) :: tl ->
	if r.ior_elemsize mod n = 0 && r.ior_count > !indexed_ofsmap_expand_limit then begin
	  if D_indexed.get_debug_flags () > 2 then
 	    dump_indexed_bytemap ~indent:">OE" [f];
	  if D_indexed.get_debug_flags () > 2 then
 	    dump_indexed_bytemap ~indent:">>C" r.ior_entry;
	  let newr = { r with ior_entry = combine_indexed_bytemap ~genv ~n r.ior_entry } in
	  if D_indexed.get_debug_flags () > 2 then
 	    dump_indexed_bytemap ~indent:"<<C" newr.ior_entry;
	  (o, IBMrepeat newr) :: iter tl
	end
	else
	  let combinen = n / (gcd r.ior_elemsize n) in
	  let remaindern = r.ior_count mod combinen in
	  let newcount = r.ior_count / combinen in
	  if D_indexed.get_debug_flags () > 2 then begin
	    dump_indexed_bytemap ~indent:">E " [f];
	    eprintf "   n = %d, combinen = %d, remaindern = %d, newcount = %d@." n combinen remaindern newcount;
	  end;
	  let newr = 
	    if newcount > 0 then
	      let r = 
		if combinen > 1 then
		  { ior_index = r.ior_index;
		    ior_elemsize = r.ior_elemsize * combinen;
		    ior_count = r.ior_count / combinen;
		    ior_entry = 
		    (List.flatten
		       (List.init combinen
			  (fun i -> 
			    shift_bytemap_entry
			      r.ior_index
			      r.ior_elemsize
			      i
			      combinen
			      i
			      r.ior_entry))) }
		else r
	      in 
	      if newcount > !indexed_ofsmap_expand_limit then [o, IBMrepeat r]
	      else
		List.flatten
		  (List.init
		     newcount
		     (fun i -> instanciate_bytemap_entry r.ior_index r.ior_elemsize o i r.ior_entry))
	    else []
	  in
	  let remainder = 
	    if remaindern = 0 then [] else
	    List.flatten (List.init remaindern
			    (fun i ->
			      instanciate_bytemap_entry
				r.ior_index r.ior_elemsize o
				(newcount * combinen + i) r.ior_entry))
	  in
	  if D_indexed.get_debug_flags () > 2 then
	    dump_indexed_bytemap ~indent:"<E " (newr @ remainder);
	  iter (newr @ remainder @ tl)
  in
  iter map

type new_access_methods = 
    NAccField of (indexed_ofsmap_ofsexp extfield * c_type) list
  | NAccDirectPtrField of (indexed_ofsmap_ofsexp extfield * c_type) list * int
  | NAccViaDwordField of (indexed_ofsmap_ofsexp extfield * c_type) list * int
  | NAccViaWordAccess
  | NAccRepeation of new_access_methods_repeation

and new_access_methods_repeation =
    { nar_index : int;
      nar_subtract : int;
      nar_divide : int;
      nar_entry : (int * int * new_access_methods) list }

let rec dump_new_access_methods ~indent nac_table = 
  List.iter
    (function
	o, o', NAccField(f) ->
	  eprintf "%s%3d -- %3d: %s@."
	    indent o o' (strof_ef f)
      | o, o', NAccDirectPtrField(f, ofs) ->
	  eprintf "%s%3d -- %3d: &(%s)[%d]@."
	    indent o o' (strof_ef f) ofs
      | o, o', NAccViaDwordField(f, ofs) ->
	  eprintf "%s%3d -- %3d: DWORD_PTR(%s, %d)@."
	    indent o o' (strof_ef f) ofs
      | o, o', NAccViaWordAccess ->
	  eprintf "%s%3d -- %3d: WORD INDIRECT@."
	    indent o o'
      | o, o', NAccRepeation r ->
	  eprintf "%s%3d -- %3d: REPEAT i%d := (ofs - %d)/%d (%d loops):@."
	    indent o o' r.nar_index r.nar_subtract r.nar_divide ((o' - o)/r.nar_divide);
	  dump_new_access_methods ~indent:(indent ^ "   ") r.nar_entry)
    nac_table

let rec compact_indexed_bytemap map = 
  let g = Glist.empty () in
  let rec iter_no = function
      [] -> ()
    | (o, o', (NAccField _ | NAccDirectPtrField _ | NAccViaDwordField _) as hd)::tl ->
	Glist.put g hd;
	iter_no tl
    | (o, o', NAccRepeation r)::tl ->
	Glist.put g
	  (o, o', 
	   NAccRepeation
	     { r with nar_entry = compact_indexed_bytemap r.nar_entry });
	iter_no tl
    | (o, o', NAccViaWordAccess)::tl ->
	iter_yes o o' tl
  and iter_yes p p' = function
    | (o, o', NAccViaWordAccess)::tl when o = p' + 1 ->
	iter_yes p o' tl
    | l ->
	Glist.put g (p, p', NAccViaWordAccess);
	iter_no l
  in
  iter_no map;
  Glist.to_list g

let make_new_access_method_table ~genv ~id ~n = 
  let rec outer l = 
    let g = Glist.empty () in
    let rec inner next_o = function
	[] -> ()
      | ((o, (IBMfield(((_, typ)::_ as fp), i) as hdfld))::_) as l -> begin
	  assert (o = next_o);
	  assert (o mod n = 0);
	  let direct_ok = ref true in
	  let field_ok = 
	    ref (i = 0 && 
		 n = int (injsome (typ.ct_size))) in
	  let rec iter oi = function
	      tl when oi = o + n -> tl
	    | (oi', IBMfield((_, typ')::_, i'))::tl ->
		assert (oi = oi');
		let p' = Translate_to_il3.parse_type_genv ~genv typ' in
		if p'.is_fat then direct_ok := false;
		if p'.is_floating then field_ok := false;
		iter (oi + 1) tl
	    | [] ->
		if oi mod n = Fsc_config.sizeof_pointer &&
		  n = Fsc_config.sizeof_pointer * 2 
		then
		  (direct_ok := false; field_ok := false; [])
		else 
		  assert false
	    | _ -> assert false
	  in
	  let tl = iter o l in
	  if !field_ok then
	    Glist.put g (o/n, o/n, NAccField fp)
	  else if !direct_ok then
	    Glist.put g (o/n, o/n, NAccDirectPtrField (fp, i))
	  else if n = Fsc_config.sizeof_pointer then begin
	    D_indexed.dprintf 6 "add_support_funcs 384_1: str_id = %d: n = %d, o = %d, fld = %s, typ = %a, i = %d@."
	      id n o (strof_ef fp) Ctt_formatter.pp_c_type typ i;
	    if int (Option.get (typ.ct_size)) = Fsc_config.sizeof_pointer * 2
		&& (i = 0 || i = Fsc_config.sizeof_pointer)
	    then
	      Glist.put g (o/n, o/n, NAccViaDwordField(fp, i))
	    else failwith "add_support_funcs 384_1"
	  end
	  else
	    Glist.put g (o/n, o/n, NAccViaWordAccess);
	  inner (o + n) tl
	end
    | (o, IBMrepeat r) :: tl -> begin
	assert (o = next_o);
	assert (r.ior_elemsize mod n = 0);
	Glist.put g
	  (o/n, (o + r.ior_elemsize * r.ior_count - n)/n,
	   NAccRepeation
	     { nar_index = r.ior_index;
	       nar_subtract = o/n;
	       nar_divide = r.ior_elemsize/n;
	       nar_entry = outer r.ior_entry });
	inner (o + r.ior_elemsize * r.ior_count) tl
    end
    | _ -> assert false
    in
    inner 0 l;
    Glist.to_list g
  in
  let map = combine_indexed_bytemap ~genv ~n (make_indexed_bytemap ~genv id) in
  compact_indexed_bytemap (outer map)

let name_of_index_var i = "i" ^ string_of_int i
let d_emit_indexvar i = d_emit_c_variable (name_of_index_var i)

let d_emit_indexexp = function
    None, s -> d_emit_c_integer s
  | Some (i, m), s ->
      d_emit_exp 
	(PexpBinExpr
	   (PbinPlus,
	    d_emit_exp
	      (PexpBinExpr (PbinTimes, d_emit_indexvar i, d_emit_c_integer m)),
	    d_emit_c_integer s))

let make_fallback_access_method ~genv ~id ~wsize = [] (* dummy *)

let make_new_access_method ~genv ~id ~wsize = 
  let desc = get_struct_desc ~genv id in
  let str_size = (int_of_big_int (Util.someof desc.str_size)) in

  let access_methods = make_new_access_method_table ~genv ~id ~n:wsize in
  if D_indexed.get_debug_flags () > 1 then begin
    eprintf "----- access method table for %d in size %d@." id wsize;
    dump_new_access_methods ~indent:"" access_methods;
    eprintf "-----\n\n@.";
  end;

  (* emit prerequisites *)
  let exp_resv = d_emit_c_variable "result_v" in
  let bp = d_emit_c_variable "bp" in
  let path_to_exp basepath = 
    path_to_exp basepath (d_emit_exp (PexpPtrDeref bp)) d_emit_indexexp
  in
  let exp_val = d_emit_c_variable "val" in
  let wt_name, vt_name, accmeth_is_fat = tokens_for_wordsize wsize in
  let wt_decl = [TypeSpec (PtypespecAlias wt_name)] in
  let vt_decl = [TypeSpec (PtypespecAlias vt_name)] in
  let struct_decl = emit_pdeclspec_namedstruct (name_of_struct ~genv id) in
  let wptrtype = (Ptypename (wt_decl, PdeclPointer([], PdeclAnonymous))) in
  let goto_merge = d_emit_stmt (PstmtGoto "merge") in
  let exp_hdr = d_emit_c_variable "hdr" in
  let exp_val = d_emit_c_variable "val" in
  let exp_b0 = d_emit_c_variable "b0" in
  let exp_base = d_emit_c_variable "base" in
  let exp_ofs = d_emit_c_variable "ofs" in
  let exp_wsize = d_emit_c_integer wsize in
  let wofsvar = d_emit_c_variable "wofs" in
  let exp_ti = d_emit_c_variable "ti" in
  let read_original_args = [exp_base; exp_ofs] in
  let write_original_args = [exp_base; exp_ofs; exp_val; exp_ti] in
  let exp_str_size = d_emit_c_integer str_size in

  (* emit inside case body *)
  let rec translate_body ofsvar l = 
    let l = List.map
	(function o, o', m ->
	  let m = match m with
	    NAccField _ | NAccDirectPtrField _ -> begin
	      let t, is_fat, exp = 
		match m with
		  NAccField fp ->
		    let t = snd (List.hd fp) in
		    let p = parse_type_genv ~genv t in
		    let exp = path_to_exp 
			(if p.is_fat then (Inamed "cv", p.packed_t)::fp else fp)
		    in
		    t, p.is_fat, exp
		| NAccDirectPtrField(fp, 0) ->
		    let pathexp = path_to_exp fp in
		    let addr = d_emit_exp (PexpAddress pathexp) in
		    let castptr = d_emit_exp (PexpCast(wptrtype, addr)) in
		    (if wsize = 1 then type_char else type_short),
		    false,
		    d_emit_exp (PexpPtrDeref(castptr))
		| NAccDirectPtrField(fp, ofs) ->
		    let pathexp = path_to_exp fp in
		    let addr1 = d_emit_exp (PexpAddress pathexp) in
		    let castptr1 = d_emit_exp (PexpCast(c_charptr_type, addr1)) in
		    let addr = d_emit_exp (PexpBinExpr(PbinPlus, castptr1, d_emit_c_integer ofs)) in
		    let castptr = d_emit_exp (PexpCast(wptrtype, addr)) in
		    (if wsize = 1 then type_char else type_short),
		    false,
		    d_emit_exp (PexpPtrDeref(castptr))
		| _ -> assert false
	      in
	      let p = parse_type_genv ~genv t in
	      let rexp = match p.packed_to_generic with
		Some f -> d_emit_invokeident (id_of_lvalue (Il3_constants.lv_reducible_functions ~genv f)) [exp]
	      | None -> exp
	      in
	      let rstmt = 
		if accmeth_is_fat && not is_fat then
		  [ d_emit_expstmt_t (PexpAssign(exp_resv, rexp));
		    goto_merge ]
		else
		  [ d_emit_stmt (PstmtReturn (Some rexp)) ]
	      in
	      let wexp = 
		match p.generic_to_packed with
		  Some f -> 
		    d_emit_invokeident (id_of_lvalue (Il3_constants.lv_reducible_functions ~genv f)) [exp_val]
		| None ->
		    if is_fat || not accmeth_is_fat then exp_val else
		    d_emit_invokeident ("vaddr_of_" ^ vt_name) [exp_val]
	      in
	      let wstmt = 
		if accmeth_is_fat && not p.is_fat then
		  [ d_emit_expstmt_t (PexpAssign(exp, wexp));
		    goto_merge ]
		else 
		  [ d_emit_expstmt_t (PexpAssign(exp, wexp));
		    d_emit_stmt (PstmtReturn None) ]
	      in
	      rstmt, wstmt
	    end
	  | NAccViaWordAccess ->
	      let rfunc_name = "read_" ^ wt_name ^ "_by_word" in
	      let rexp = d_emit_invokeident rfunc_name read_original_args in
	      let wfunc_name = "write_" ^ wt_name ^ "_to_word" in
	      let wexp = d_emit_invokeident wfunc_name write_original_args in
	      [ d_emit_expreturn rexp ],
	      [ d_emit_expstmt wexp; d_emit_stmt (PstmtReturn None) ]
	  | NAccViaDwordField(fp, ofs) ->
	      let t = snd (List.hd fp) in
	      let p = parse_type_genv ~genv t in
	      assert (p.is_fat);
	      assert (p.packed_to_generic = None);
	      let pathexp = path_to_exp 
		  (if p.is_fat then (Inamed "cv", p.packed_t)::fp else fp) in
	      [ d_emit_expreturn
		  (d_emit_invokeident "partial_value_of_dvalue" [pathexp; d_emit_c_integer ofs]) ],
	      [ d_emit_expstmt
		  (d_emit_invokeident "write_partial_value_to_dvalue" 
		     [(d_emit_exp (PexpAddress pathexp)); exp_val; d_emit_c_integer ofs]);
		d_emit_stmt (PstmtReturn None) ]
	  | NAccRepeation r ->
	      let ivar = d_emit_indexvar r.nar_index in
	      let ovar_name = "ofs" ^ string_of_int r.nar_index in
	      let rbody, wbody = translate_body (d_emit_c_variable ovar_name) r.nar_entry in
	      let vars = 
		[pdeclspec_ofs_t, PdeclIdent (name_of_index_var r.nar_index), 
		 Some
		   (d_emit_binexp PbinDiv
		      (d_emit_binexp PbinMinus ofsvar (d_emit_c_integer r.nar_subtract))
		      (d_emit_c_integer r.nar_divide));
		 pdeclspec_ofs_t, PdeclIdent (ovar_name),
		 Some 
		   (d_emit_binexp PbinModulo
		      (d_emit_binexp PbinMinus ofsvar (d_emit_c_integer r.nar_subtract))
		      (d_emit_c_integer r.nar_divide))
	       ]
	      in
	      [ d_emit_compound vars [rbody] ], [ d_emit_compound vars [wbody] ]
	  in
	  (o, o', m))
	l
    in
    let rcasebody, wcasebody = Glist.empty (), Glist.empty () in
    List.iter
      (function
	  o, o', ((rs::rss), (ws::wss)) ->
	    let rec iter oo rs ws = 
	      if oo < o then rs, ws
	      else
		iter (oo - 1)
		  (d_emit_stmt (PstmtCase_Labeled (d_emit_c_integer oo, rs)))
		  (d_emit_stmt (PstmtCase_Labeled (d_emit_c_integer oo, ws)))
	    in
	    let rs, ws =
	      if use_gcc_extension then
		let label = 
		  if o == o' then d_emit_c_integer o
		  else
		    d_emit_exp
		      (PexpConstant 
			 (PconstInteger 
			    (string_of_int o ^ " ... " ^ string_of_int o')))
		in
		(d_emit_stmt (PstmtCase_Labeled (label, rs))),
		(d_emit_stmt (PstmtCase_Labeled (label, ws)))
	      else iter o' rs ws
	    in
	    Glist.append rcasebody (rs :: rss);
	    Glist.append wcasebody (ws :: wss)
	| _, _, _ -> assert false)
      l;
    begin
      let dummy = 
	d_emit_stmt 
	  (PstmtDefault_Labeled 
	     (d_emit_stmt
		(PstmtExpr (Some (d_emit_invokeident "fsc_halt_program" []))))) in
      Glist.put rcasebody dummy;
      Glist.put wcasebody dummy
    end;
    d_emit_stmt (PstmtSwitch (ofsvar, d_emit_compound [] (Glist.to_list rcasebody))),
    d_emit_stmt (PstmtSwitch (ofsvar, d_emit_compound [] (Glist.to_list wcasebody)))
  in
  let rcase, wcase = translate_body wofsvar access_methods in
  (* put code for additional bases *)
  let rbody, wbody = 
    if accmeth_is_fat then
      d_emit_compound
	[wt_decl, PdeclIdent "result_v", Some (d_emit_c_integer 0)]
	[rcase;
	 d_emit_stmt
	   (PstmtLabeled 
	      ("merge",
	       d_emit_stmt
		 (PstmtReturn
		    (Some
		       (d_emit_invokeident ("read_merge_additional_base_" ^ wt_name)
			  [d_emit_c_variable "result_v"; exp_b0; exp_ofs])))))],
      d_emit_compound
	[]
	[wcase;
	 d_emit_stmt
	   (PstmtLabeled 
	      ("merge",
	       d_emit_stmt
		 (PstmtExpr
		    (Some
		       (d_emit_invokeident ("write_additional_base_" ^ wt_name)
			  [exp_b0; exp_ofs; 
			   d_emit_invokeident ("base_of_" ^ vt_name) [exp_val]])))));
	 d_emit_stmt (PstmtReturn None)
       ]
    else
      rcase, wcase
  in
  (* put check for unalignedness *)
  let rbody, wbody = 
    if wsize = 1 then rbody, wbody else
    d_emit_stmt
      (PstmtIf
	 (d_emit_binexp PbinModulo (d_emit_c_variable "ofs_inner") exp_wsize,
	  d_emit_stmt
	    (PstmtReturn
	       (Some (d_emit_invokeident ("read_" ^ wt_name ^ "_offseted_" ^ wt_name)
			read_original_args))),
	  Some rbody)),
    d_emit_stmt
      (PstmtIf
	 (d_emit_binexp PbinModulo (d_emit_c_variable "ofs_inner") exp_wsize,
	  d_emit_compound []
	    [d_emit_expstmt
	       (d_emit_invokeident ("write_" ^ wt_name ^ "_offseted_" ^ wt_name)
		  write_original_args);
	     d_emit_stmt (PstmtReturn None)],
	  Some wbody))
  in
  (* make ofs_outer, ofs_inner *)
  let decls = 
    [(pdeclspec_size_t, PdeclIdent "ofs_outer",
      Some (d_emit_exp (PexpBinExpr (PbinDiv, exp_ofs, exp_str_size))));
     (pdeclspec_size_t, PdeclIdent "ofs_inner",
      Some (d_emit_exp (PexpBinExpr (PbinModulo, exp_ofs, exp_str_size))));
     (pdeclspec_size_t, PdeclIdent "wofs",
      Some (d_emit_exp (PexpBinExpr (PbinDiv, d_emit_c_variable "ofs_inner", exp_wsize))));
     (struct_decl, PdeclPointer ([], PdeclIdent "bp"),
      Some 
	(d_emit_exp
	   (PexpBinExpr
	      (PbinPlus, 
	       d_emit_exp
		 (PexpCast
		    (Ptypename (struct_decl, PdeclPointer([], PdeclAnonymous)), exp_base)),
	       d_emit_c_variable "ofs_outer"))))]
  in
  let rbody, wbody = 
    d_emit_compound decls [rbody], d_emit_compound decls [wbody] in
  (* make check for overflow *)
  let rbody, wbody = 
    let check_exp = 
      d_emit_binexp
	PbinLogOr
	(d_emit_binexp
	   PbinGtrEqual
	   exp_ofs
	   (d_emit_exp
	      (PexpPtrField (exp_hdr, "structured_ofslimit"))))
	(d_emit_binexp
	   PbinGtrThan
	   (d_emit_binexp PbinPlus exp_ofs exp_wsize)
	   (d_emit_exp
	      (PexpPtrField (exp_hdr, "structured_ofslimit"))))
    in
    d_emit_stmt
      (PstmtIf
	 (check_exp,
	  d_emit_stmt
	    (PstmtReturn
	       (Some
		  (d_emit_invokeident ("read_" ^ wt_name ^ "_remainder") read_original_args))),
	  Some rbody)),
    d_emit_stmt
      (PstmtIf
	 (check_exp,
	  d_emit_compound []
	    [d_emit_stmt
	       (PstmtExpr
		  (Some
		     (d_emit_invokeident ("write_" ^ wt_name ^ "_remainder") write_original_args)));
	     d_emit_stmt (PstmtReturn None)],
	  Some wbody))
  in
  (* put base, hdr, and dealloc_check *)
  let rbody, wbody =
    let check_dealloc = 
      d_emit_stmt
	(PstmtExpr
	   (Some
	      (d_emit_exp
		 (PexpInvoke(d_emit_c_variable "dealloc_check_fast", [exp_base; exp_ofs])))))
    in
    let decls = 
      [pdeclspec_base_t, 
       PdeclIdent "base",
       Some (d_emit_invokeident "base_remove_castflag" [exp_b0]);
       pdeclspec_fsc_header,
       PdeclPointer([], PdeclIdent "hdr"),
       Some (d_emit_invokeident "get_header_fast" [exp_base])]
    in
    d_emit_compound decls [check_dealloc; rbody],
    d_emit_compound decls [check_dealloc; wbody]
  in
  let rdecl, wdecl = 
    let weakness = standard_weakness () in
    d_emit_decl
      (PdeclFunction 
	 (vt_decl @ weakness,
	  PdeclFuncType(PdeclIdent ("read_" ^ wt_name ^ "_" ^ 
				    encoded_name_of_struct ~genv id),
			[PpdeclConcrete(pdeclspec_base_t, PdeclIdent "b0");
			 PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs")]),
	  [], rbody)),
    d_emit_decl
      (PdeclFunction 
	 (pdeclspec_void @ weakness,
	  PdeclFuncType(PdeclIdent ("write_" ^ wt_name ^ "_" ^ 
				    encoded_name_of_struct ~genv id),
			[PpdeclConcrete(pdeclspec_base_t, PdeclIdent "b0");
			 PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs");
			 PpdeclConcrete(vt_decl, PdeclIdent "val");
			 PpdeclConcrete(pdeclspec_typeinfo_t, PdeclIdent "ti")
		       ]),
	  [], wbody))
  in
  [rdecl; wdecl]

(*================
   Old version
================*)

let make_offset_map ~genv id =
  let map = 
    let str = Ctt_abstree.get_struct_desc ~genv id in
    let size =
      match str.str_size with 
	Some s -> int s
      | None -> failwith "make_offset_map: cannot handle unsized structure"
    in
    Array.create size None
  in
  let rec iter_on_str id ~prefix_rev ~start_offset = 
    let str = Ctt_abstree.get_struct_desc ~genv id in
    let size = int (injsome str.str_size) in
    dprintf 6 "iter_on_str(#%d) start_ofs:%d" id start_offset;
    let put ofs entry = 
      dprintf 6 "put @@%d (ofs:%d + sofs=%d)" (ofs + start_offset) ofs start_offset;
      map.(ofs + start_offset) <- entry
    in
    let rec iter_on_fields ofs fields = 
      dprintf 6 "iter_fields(%d) ofs:%d" (List.length fields) ofs;
      if ofs >= size then () else
      match fields with
	[] ->
	  put ofs None;
	  iter_on_fields (ofs+1) fields
      | (fofs, fld) :: tl ->
	  let fofs = int fofs in
	  dprintf 6 "iter_fields_2(%d) fofs:%d ofs:%d" (List.length fields) fofs ofs;
	  if fofs > ofs then begin
	    put ofs None;
	    iter_on_fields (ofs+1) fields
	  end else if fofs < ofs then assert false
	  else begin
	    match fld with
	      BitField bf ->
		failwith "unused297mapbitfield"; (* not used in Fail-Safe C; see Il0_translate_bitfield *)
		(*for i = ofs to ofs + (int bf.s_bf_size) - 1 do
		  put i None;
		done;
		iter_on_fields (ofs + int bf.s_bf_size) tl *)
	    | NormalField fld ->
		let rec iter_on_type ofs f t sz = 
		  dprintf 6 "iter_type ofs:%d t:%a" ofs Ctt_formatter.pp_c_type t;
		  match t.ct_ty with
		    Tbuiltin _ | Tpointer _ ->
		      for i = 0 to sz - 1 do
			put (ofs + i) (Some (f, i))
		      done
		  | Tarray(typ, Some count) ->
		      let count = int count in
		      let elemsz = int (injsome (Ctt_abstree.size_of_type ~genv typ))
		      in
		      for i = 0 to count - 1 do
			iter_on_type
			  (ofs + i * elemsz)
			  ((Iindexed (big_int_of_int i), typ) :: f)
			  typ
			  elemsz
		      done
		  | Tstruct id ->
		      iter_on_str id ~prefix_rev:f ~start_offset:(ofs + start_offset)
		  | Tarray(_, None)
		  | Tvoid | Tabstract _ | Tfunction _ -> assert false
		in
		let f = (Inamed (translate_field_name fld.sf_id), fld.sf_type) :: prefix_rev in
		iter_on_type ofs f fld.sf_type (int fld.sf_size);
		iter_on_fields (ofs + int fld.sf_size) tl
	  end
    in
    iter_on_fields 0 str.str_fields
  in
  iter_on_str id ~prefix_rev:[] ~start_offset:0;
  map

type access_type = 
    AccField of (big_int extfield * c_type) list
  | AccComplex
  | AccDirectPtrField of (big_int extfield * c_type) list * int
  | AccDirectRawRegion
  | AccPartialDwordField of (big_int extfield * c_type) list * int

let make_offset_map_for_unaligned_wordsize ~genv ~id ~wsize ~map ~numbytes = 
  (* structure size does not align with access size *)
  let l = (numbytes + wsize - 1) / wsize in
  try
    Array.iter 
      (function
	  None -> ()
	| Some (((name, typ)::_), ofs) ->
	    let p = Translate_to_il3.parse_type_genv ~genv typ in
	    if p.is_fat then raise Exit
	| Some ([], _) ->
	    assert false) map;
    Array.create l AccDirectRawRegion
  with
    Exit ->
    Array.create l AccComplex

let make_offset_map_for_wordsize ~genv id wsize = 
  let map = make_offset_map ~genv id in
  let numbytes = Array.length map in
  if numbytes mod wsize <> 0 then
    make_offset_map_for_unaligned_wordsize ~genv ~id ~wsize ~map ~numbytes
  else begin
    let numwords = numbytes / wsize in
    let newmap = Array.create numwords AccComplex in
    for i = 0 to numwords - 1 do
      let field_path = ref [] in
      let direct_ok = ref true in
      let direct_offseted = ref false in
      let field_ok = ref true in
      let field_ofs = ref None in
      for j = 0 to wsize - 1 do
	match map.(i * wsize + j) with
	  None ->
	    field_ok := false;
	| Some (((name, typ)::_) as path , ofs) ->
	    if j = 0 then begin
	      field_path := path;
	      field_ofs := Some ofs
	    end;
	    let p = Translate_to_il3.parse_type_genv ~genv typ in
	    if p.is_fat then direct_ok := false;
	    if p.is_floating then field_ok := false;
	    if ofs <> j then
	      (field_ok := false; direct_offseted := true);
	    if wsize <> int (injsome typ.ct_size) then
	      field_ok := false
	| Some ([], _) ->
	    assert false
      done;
      newmap.(i) <- begin
	if !field_ok then 
	  AccField !field_path
	else if !direct_ok && wsize <= Fsc_config.sizeof_pointer then
	  if !direct_offseted then
	    AccDirectPtrField(!field_path, someof !field_ofs)
	  else AccDirectPtrField (!field_path, 0)
	else
	  if wsize = Fsc_config.sizeof_pointer then begin
	    match map.(i * wsize) with
	      Some (((name, typ)::_) as path, ofs) ->
		dprintf 6 "add_support_funcs 238: %a %d" Ctt_formatter.pp_c_type typ ofs;
		if 
		  (ofs = 0 || ofs = Fsc_config.sizeof_pointer) &&
		  int_of_big_int (Option.get (typ.ct_size)) = Fsc_config.sizeof_pointer * 2
		then AccPartialDwordField(!field_path, ofs)
		else failwith "add_support_funcs 238_1: make_offset_map failed for machine word size"
	    | _ -> failwith "add_support_funcs 238_2: make_offset_map failed for machine word size"
	  end else
	    AccComplex
      end
    done;
    newmap
  end

let make_access_exp_table ~genv ~wsize ~wptrtype ~id =
  let map = make_offset_map_for_wordsize ~genv id wsize in
  let bp = d_emit_c_variable "bp" in
(*  let offset = d_emit_c_variable "ofs" in
  let base = d_emit_c_variable "base" in *)
  let path_to_exp basepath = 
    path_to_exp basepath (d_emit_exp (PexpPtrDeref bp)) d_emit_c_big_integer
  in
  let f i = function
      AccField (path) ->
	let t =
	  match path with
	    (_,t)::_ -> t
	  | [] -> assert false
	in
	let p = parse_type_genv ~genv t in
	let path = if p.is_fat then (Inamed "cv", p.packed_t)::path else path in
	let exp_t = path_to_exp path in
	AEpointer (t, p.is_fat, exp_t)
    | AccDirectPtrField (path, 0) ->
	(*assert (wsize <= 2);*)
	let pathexp = path_to_exp path in
	let addr = d_emit_exp (PexpAddress pathexp) in
	let castptr = d_emit_exp (PexpCast(wptrtype, addr)) in
	AEpointer ((if wsize = 1 then type_char else type_short), false, d_emit_exp (PexpPtrDeref(castptr)))
    | AccDirectPtrField (path, ofs) ->
	(*assert (wsize <= 2);*)
	let pathexp = path_to_exp path in
	let addr1 = d_emit_exp (PexpAddress pathexp) in
	let castptr1 = d_emit_exp (PexpCast(c_charptr_type, addr1)) in
	let addr = d_emit_exp (PexpBinExpr(PbinPlus, castptr1, d_emit_c_integer ofs)) in
	let castptr = d_emit_exp (PexpCast(wptrtype, addr)) in
	AEpointer ((if wsize = 1 then type_char else type_short), false, d_emit_exp (PexpPtrDeref(castptr)))
    | AccDirectRawRegion ->
        let castptr = d_emit_exp (PexpCast(wptrtype, bp)) in
        AEpointer ((if wsize = 1 then type_char else type_short), false,
		   d_emit_exp (PexpArrayRef(castptr, d_emit_c_integer (i / wsize))))
    | AccComplex ->
	AEdelegate
    | AccPartialDwordField (path, ofs) ->
	let t =
	  match path with
	    (_,t)::_ -> t
	  | [] -> assert false
	in
	let p = parse_type_genv ~genv t in
	let path = if p.is_fat then (Inamed "cv", p.packed_t)::path else path in
	let exp_t = path_to_exp path in
	AEdelegateDword(t, p.is_fat, exp_t, ofs)
  in
  Array.mapi f map

let make_old_access_method_for_struct ~read ~genv ~wsize ~id =
  let desc = get_struct_desc ~genv id in
  let rw_str = if read then "read" else "write" in
  let wt_name, vt_name, may_fat = tokens_for_wordsize wsize in
  let wt_decl = [TypeSpec (PtypespecAlias wt_name)] in
  let vt_decl = [TypeSpec (PtypespecAlias vt_name)] in
  let struct_decl = emit_pdeclspec_namedstruct (name_of_struct ~genv id) in
  let map = make_access_exp_table ~genv ~wsize ~id
      ~wptrtype:(Ptypename (wt_decl, PdeclPointer([], PdeclAnonymous)))
  in
  let exp_resv = d_emit_c_variable "result_v" in
(*  let exp_bp = d_emit_c_variable "bp" in *)
  let exp_b0 = d_emit_c_variable "b0" in
  let exp_base = d_emit_c_variable "base" in
  let exp_ofs = d_emit_c_variable "ofs" in
  let exp_hdr = d_emit_c_variable "hdr" in
  let exp_val = d_emit_c_variable "val" in
  let exp_ti = d_emit_c_variable "ti" in
  let original_args = 
    if read then
      [exp_base; exp_ofs]
    else
      [exp_base; exp_ofs; exp_val; exp_ti]
  in
  let d_emit_stmtlist_ret exp = 
    if read then
      [d_emit_stmt (PstmtReturn (Some exp))]
    else
      [d_emit_stmt (PstmtExpr (Some exp)); d_emit_stmt (PstmtReturn None)]
  in
  let d_emit_stmt_ret exp = 
    if read then
      d_emit_stmt (PstmtReturn (Some exp))
    else
      d_emit_compound []
	[d_emit_stmt (PstmtExpr (Some exp)); d_emit_stmt (PstmtReturn None)]
  in
  let exp_str_size = d_emit_c_integer (int_of_big_int (Util.someof desc.str_size)) in
  let exp_wsize = d_emit_c_integer wsize in
  let casebodies = 
    Array.mapi
      (fun i -> function
	  AEpointer(t, fatfield, exp) ->
	    let accessexp = 
	      if read then begin
		let p = parse_type_genv ~genv t in
		match p.packed_to_generic with
		  Some f -> d_emit_invokeident (id_of_lvalue (Il3_constants.lv_reducible_functions ~genv f)) [exp]
		| None -> exp
	      end else begin
		let p = parse_type_genv ~genv t in
		let exp_val = 
		  match p.generic_to_packed with
		    Some f -> d_emit_invokeident (id_of_lvalue (Il3_constants.lv_reducible_functions ~genv f)) [exp_val]
		  | None ->
		      if fatfield || not may_fat then exp_val else
		      d_emit_invokeident ("vaddr_of_" ^ vt_name) [exp_val]
		in
		d_emit_exp (PexpAssign(exp, exp_val))
	      end
	    in
	    if fatfield || not may_fat then
	      if read then [ d_emit_stmt (PstmtReturn (Some accessexp)) ]
	      else [ d_emit_stmt (PstmtExpr (Some accessexp)); d_emit_stmt (PstmtReturn None) ]
	    else
	      if read then [ d_emit_stmt (PstmtExpr (Some (d_emit_exp (PexpAssign(exp_resv, accessexp)))));
			     d_emit_stmt (PstmtBreak) ]
	      else [ d_emit_stmt (PstmtExpr (Some accessexp)); d_emit_stmt (PstmtBreak) ]
	| AEdelegateDword(t, fatfield, exp, ofs) ->
	    assert (fatfield);
	    let p = parse_type_genv ~genv t in
	    assert (p.packed_to_generic = None);
	    if read then
	      [ d_emit_stmt 
		  (PstmtReturn
		     (Some
			(d_emit_invokeident "partial_value_of_dvalue" [exp; d_emit_c_integer ofs]))) ]
	    else
	      [ d_emit_stmt (PstmtExpr 
			     (Some (d_emit_invokeident "write_partial_value_to_dvalue" 
				      [(d_emit_exp (PexpAddress exp)); exp_val; d_emit_c_integer ofs])));
		d_emit_stmt (PstmtReturn None) ]
	| AEdelegate ->
	    let func_name = rw_str ^ "_" ^ wt_name ^ (if read then "_by_" else "_to_") ^ "word" in
	    let exp = d_emit_invokeident func_name original_args in
	    d_emit_stmtlist_ret exp)
      map
  in
  let casebodies = 
    Array.mapi
      (fun i -> function
	  st::rest ->
	    d_emit_stmt (PstmtCase_Labeled (d_emit_c_integer (i * wsize), st))
	    :: rest
	| [] -> assert false)
      casebodies
  in
  let case = 
    let dummy = 
      d_emit_stmt 
	(PstmtDefault_Labeled 
	   (d_emit_stmt
	      (PstmtExpr (Some (d_emit_invokeident "fsc_halt_program" []))))) in
    d_emit_stmt
      (PstmtSwitch 
	 (d_emit_c_variable "ofs_inner",
	  d_emit_compound [] (List.flatten (Array.to_list casebodies @ [[dummy]]))))
  in
  let mainbody = 
    if may_fat then
      if read then
	d_emit_compound
	  [wt_decl, PdeclIdent "result_v", Some (d_emit_c_integer 0)]
	  [case;
	   d_emit_stmt
	     (PstmtReturn
		(Some
		   (d_emit_invokeident ("read_merge_additional_base_" ^ wt_name)
		      [d_emit_c_variable "result_v"; exp_b0; exp_ofs])))]
      else
	d_emit_compound
	  []
	  [case;
	   d_emit_stmt
	     (PstmtExpr
		(Some
		   (d_emit_invokeident ("write_additional_base_" ^ wt_name)
		      [exp_b0; exp_ofs; 
		       d_emit_invokeident ("base_of_" ^ vt_name) [exp_val]])));
	   d_emit_stmt (PstmtReturn None)
	 ]
    else
      case
  in
  let check_align_stmt = 
    let unaligned_expr = 
      (d_emit_invokeident 
	 (rw_str ^ "_" ^ wt_name ^ "_offseted_" ^ wt_name)
	 original_args)
    in
    if wsize = 1 then mainbody else
    d_emit_stmt
      (PstmtIf
	 (d_emit_exp
	    (PexpBinExpr
	       (PbinModulo, d_emit_c_variable "ofs_inner", exp_wsize)),
	  d_emit_stmt_ret unaligned_expr,
	  Some mainbody))
  in
  let bpgenexp = 
    d_emit_exp
      (PexpBinExpr
	 (PbinPlus, 
	  d_emit_exp
	    (PexpCast
	       (Ptypename (struct_decl, PdeclPointer([], PdeclAnonymous)), exp_base)),
	  d_emit_c_variable "ofs_outer"))
  in
  let check_align = 
    d_emit_compound
      [(pdeclspec_size_t, PdeclIdent "ofs_outer",
	Some (d_emit_exp (PexpBinExpr (PbinDiv, exp_ofs, exp_str_size))));
       (pdeclspec_size_t, PdeclIdent "ofs_inner",
	Some (d_emit_exp (PexpBinExpr (PbinModulo, exp_ofs, exp_str_size))));
       (struct_decl, PdeclPointer ([], PdeclIdent "bp"), Some bpgenexp);
      ]
      [check_align_stmt]
  in
  let check_over = 
    d_emit_stmt
      (PstmtIf 
	 (d_emit_exp
	    (PexpBinExpr
	       (PbinLogOr,
		d_emit_exp
		  (PexpBinExpr
		     (PbinGtrEqual, 
		      exp_ofs,
		      d_emit_exp
			(PexpPtrField (exp_hdr, "structured_ofslimit")))),
		d_emit_exp
		  (PexpBinExpr
		     (PbinGtrThan, 
		      d_emit_exp
			(PexpBinExpr (PbinPlus, exp_ofs, exp_wsize)),
		      d_emit_exp
			(PexpPtrField (exp_hdr, "structured_ofslimit")))))),
	  d_emit_stmt_ret
	    (d_emit_invokeident (rw_str ^ "_" ^ wt_name ^ "_remainder") original_args),
	  Some check_align))
  in
  let check_dealloc = 
    d_emit_stmt
      (PstmtExpr
	 (Some
	    (d_emit_exp
	       (PexpInvoke(d_emit_c_variable "dealloc_check_fast", [exp_base; exp_ofs])))))
  in
  let body = 
    d_emit_compound
      [pdeclspec_base_t, 
         PdeclIdent "base",
         Some (d_emit_invokeident "base_remove_castflag" [exp_b0]);
       pdeclspec_fsc_header,
         PdeclPointer([], PdeclIdent "hdr"),
         Some (d_emit_invokeident "get_header_fast" [exp_base])]
      [check_dealloc; check_over]
  in
  let weakness = standard_weakness () in
  if read then
    d_emit_decl
      (PdeclFunction 
	 (vt_decl @ weakness,
	  PdeclFuncType(PdeclIdent ("read_" ^ wt_name ^ "_" ^ 
				    encoded_name_of_struct ~genv id),
			[PpdeclConcrete(pdeclspec_base_t, PdeclIdent "b0");
			 PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs")]),
	  [], body))
  else
    d_emit_decl
      (PdeclFunction 
	 (pdeclspec_void @ weakness,
	  PdeclFuncType(PdeclIdent ("write_" ^ wt_name ^ "_" ^ 
				    encoded_name_of_struct ~genv id),
			[PpdeclConcrete(pdeclspec_base_t, PdeclIdent "b0");
			 PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs");
			 PpdeclConcrete(vt_decl, PdeclIdent "val");
			 PpdeclConcrete(pdeclspec_typeinfo_t, PdeclIdent "ti")
		       ]),
	  [], body))

(*   static inline struct_S<i> * get_realoffset_S(<i>)(base_t base, ofs_t ofs) {  *)
(*     return (struct_S<i> * )(base + ofs / vsize * rsize);  *)
(*   }  *)
let make_realoffset_method ~genv ~id ~vsize ~rsize = 
  let struct_decl = emit_pdeclspec_namedstruct (name_of_struct ~genv id) in
  let methname = "get_realoffset_" ^ (encoded_name_of_struct ~genv id) in
(*  let strname = name_of_struct ~genv id in *)
  let base = d_emit_c_variable "base" in
  let ofs = d_emit_c_variable "ofs" in
  let gcd = gcd vsize rsize in
  let vsize, rsize = vsize / gcd, rsize / gcd in
  let e_elemidx = 
    if vsize = 1 then ofs 
    else d_emit_exp (PexpBinExpr(PbinDiv, ofs, d_emit_c_integer vsize)) in
  let e_realofs = 
    if rsize = 1 then e_elemidx
    else d_emit_exp (PexpBinExpr(PbinTimes, e_elemidx, d_emit_c_integer rsize)) in
  let addr = 
    d_emit_exp (PexpBinExpr(PbinPlus, base, e_realofs)) in
  let typ = Ptypename (struct_decl, PdeclPointer([], PdeclAnonymous)) in
  let exp = d_emit_exp (PexpCast(typ, addr)) in
  let stmt = d_emit_stmt (PstmtReturn (Some exp)) in
  let body = d_emit_stmt (PstmtCompound ([], [stmt])) in
  d_emit_decl
    (PdeclFunction 
       ((StorageClass Static) :: (StorageClass Inline) :: struct_decl,
	PdeclPointer
	  ([],
	   PdeclFuncType
	     (PdeclIdent (methname),
	      [PpdeclConcrete(pdeclspec_base_t, PdeclIdent "base");
	       PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs");
	     ])),
	[], body))

(*    struct_S<i> read_struct_S<i>(base_t base, ofs_t ofs) { *)
(*      struct_S<i> ret; *)
(*      ret.i.cv = read_word(base, ofs); *)
(*      ret.j = read_hword(base, ofs + 4); *)
(*      for (i0 = 0; i0 < 3; i0++) { *)
(*        ret.a[i0] = read_char(base, ofs + 6 + i0 * 2); *)
(*      } *)
(*      return ret; *)
(*    } *)

let make_rw_method_for_struct ~genv ~id = 
  let weakness = standard_weakness () in 
  let struct_decl = emit_pdeclspec_namedstruct (name_of_struct ~genv id) in
  let rmethname = "read_" ^ (name_of_struct ~genv id) in
  let wmethname = "write_" ^ (name_of_struct ~genv id) in
  let base = d_emit_c_variable "base" in
  let ofs = d_emit_c_variable "ofs" in
  let sv = d_emit_c_variable "sv" in
  let str = Ctt_abstree.get_struct_desc ~genv id in
  let rec d_emit_field ty toppath path oexp = 
    let p = parse_type_genv ~genv ty in
    let path = (toppath, p.initializer_type):: path in
    match ty.ct_ty with
      Tbuiltin _ | Tpointer _ ->
	let path = if p.is_fat then (Inamed "cv", p.packed_t)::path else path in
	let stk, _, _ = tokens_for_wordsize (int (Option.get ty.ct_size)) in
	let generic_read = d_emit_invokeident ("read_" ^ stk) [base; oexp] in
	let rv = 
	  match p.generic_to_packed with
	    Some f -> d_emit_invokeident (id_of_lvalue (Il3_constants.lv_reducible_functions ~genv f)) [generic_read]
	  | None -> generic_read
	in
	let e = path_to_exp path sv (fun x -> x) in
	let wv = 
	  match p.packed_to_generic with
	    Some f -> d_emit_invokeident (id_of_lvalue (Il3_constants.lv_reducible_functions ~genv f)) [e]
	  | None -> e
	in
	let generic_write = d_emit_invokeident ("write_" ^ stk) [base; oexp; wv; d_emit_c_integer 0] in
	d_emit_stmt (PstmtExpr (Some (d_emit_exp (PexpAssign (e, rv))))),
	d_emit_stmt (PstmtExpr (Some generic_write))
    | Tstruct id ->
	let str = Ctt_abstree.get_struct_desc ~genv id in
	let rs, ws = iter oexp path str.str_fields in
	d_emit_stmt (PstmtCompound ([], rs)),
	d_emit_stmt (PstmtCompound ([], ws))
    | Tarray _ -> assert false (* nested arrays are flattened at an earlier stage *)
    | Tfunction _ | Tvoid | Tabstract _ -> assert false
  and iter oexp path = function
      [] -> [], []
    | (fofs, fld) :: tl ->
	let fofs = int fofs in
	let rs, ws = 
	  match fld with
	    BitField bf -> assert false
	  | NormalField ({ sf_type = ty } as fld) ->
	      let oexp_fld = d_emit_exp (PexpBinExpr (PbinPlus, oexp, d_emit_c_integer fofs)) in
	      match ty.ct_ty with
		Tabstract _ | Tfunction (_, _, _) | Tvoid -> assert false
	      | Tbuiltin _ | Tpointer _ | Tstruct _ ->
		  d_emit_field fld.sf_type (Inamed (translate_field_name fld.sf_id)) path oexp_fld
	      | Tarray(et, sz) ->
		  let p = parse_type_genv ~genv et in
		  let i_name = "i" ^ string_of_int (List.length path) in
		  let i = d_emit_c_variable i_name in
		  let oexp_fld = 
		    d_emit_exp
		      (PexpBinExpr 
			 (PbinPlus, oexp_fld, 
			  d_emit_exp
			    (PexpBinExpr 
			       (PbinTimes, i, d_emit_c_big_integer (Option.get et.ct_size))))) in
		  let apath = (Inamed (translate_field_name fld.sf_id),
			       make_c_type (Tarray (p.initializer_type, sz)))::path in
		  let rs, ws = d_emit_field et (Iindexed i) apath oexp_fld in
		  let rs =
		    d_emit_stmt
		      (PstmtFor
			 (Some (d_emit_exp (PexpAssign (i, d_emit_c_integer 0))),
			  Some (d_emit_exp (PexpBinExpr (PbinLessThan, i, d_emit_c_big_integer (Option.get sz)))),
			  Some (d_emit_exp (PexpPostInc i)),
			  rs))
		  in
		  let ws =
		    d_emit_stmt
		      (PstmtFor
			 (Some (d_emit_exp (PexpAssign (i, d_emit_c_integer 0))),
			  Some (d_emit_exp (PexpBinExpr (PbinLessThan, i, d_emit_c_big_integer (Option.get sz)))),
			  Some (d_emit_exp (PexpPostInc i)),
			  ws))
		  in
		  d_emit_stmt
		    (PstmtCompound 
		       ([d_emit_decl (PdeclVariable (pdeclspec_size_t, [PinitDecl (PdeclIdent i_name, None)]))], 
			[rs])),
		  d_emit_stmt
		    (PstmtCompound 
		       ([d_emit_decl (PdeclVariable (pdeclspec_size_t, [PinitDecl (PdeclIdent i_name, None)]))], 
			[ws]))
	in
	let rs', ws' = iter oexp path tl in
	rs :: rs', ws :: ws'
  in
  let rstmts, wstmts = iter ofs [] str.str_fields in
  let rfbody =
    d_emit_stmt 
      (PstmtCompound
	 ([d_emit_decl (PdeclVariable (struct_decl, [PinitDecl(PdeclIdent "sv", None)]))],
	  rstmts @ [d_emit_stmt (PstmtReturn (Some sv))]))
  in
  let wfbody =
    d_emit_stmt 
      (PstmtCompound
	 ([], wstmts))
  in
  [ d_emit_decl
      (PdeclFunction 
	 (struct_decl @ weakness,
	  PdeclFuncType
	    (PdeclIdent rmethname,
	     [PpdeclConcrete(pdeclspec_base_t, PdeclIdent "base");
	    PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs")]),
	  [], rfbody));
      d_emit_decl
      (PdeclFunction 
	 (pdeclspec_void @ weakness,
	  PdeclFuncType
	    (PdeclIdent wmethname,
	     [PpdeclConcrete(pdeclspec_base_t, PdeclIdent "base");
	      PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs");
	      PpdeclConcrete(struct_decl, PdeclIdent "sv")]),
	  [], wfbody)) ]

let make_access_methods_for_struct ~genv ~id =
  if !indexed_ofsmap_expand_limit < 1 then
    [
     make_old_access_method_for_struct ~read:true  ~genv ~wsize:1 ~id;
     make_old_access_method_for_struct ~read:true  ~genv ~wsize:2 ~id;
     make_old_access_method_for_struct ~read:true  ~genv ~wsize:4 ~id;
     make_old_access_method_for_struct ~read:true  ~genv ~wsize:8 ~id;
     make_old_access_method_for_struct ~read:false ~genv ~wsize:1 ~id;
     make_old_access_method_for_struct ~read:false ~genv ~wsize:2 ~id;
     make_old_access_method_for_struct ~read:false ~genv ~wsize:4 ~id;
     make_old_access_method_for_struct ~read:false ~genv ~wsize:8 ~id;
   ] 
  else begin
    make_new_access_method ~genv ~id ~wsize:1 @
    make_new_access_method ~genv ~id ~wsize:2 @
    make_new_access_method ~genv ~id ~wsize:4 @
    make_new_access_method ~genv ~id ~wsize:8 @
    []
  end

(**************** TYPE INFORMATION ****************)

type typeinfo_dat = {
    ti_tiname : string;
    ti_typename : string;
    ti_typekind : string list;
    ti_refertype : string option;
    ti_virtual_size : int;
    ti_real_size : int;
    ti_read_dword : string;
    ti_read_word : string;
    ti_read_hword : string;
    ti_read_byte : string;
    ti_write_dword : string;
    ti_write_word : string;
    ti_write_hword : string;
    ti_write_byte : string;
    ti_additionals : string;
  }

let d_emit_typeinfo ?(is_external = false) ~proto ti = 
  let body = 
    if proto then None else begin
      let typekind = 
	List.fold_left
	  (fun lexp rname ->
	    d_emit_exp (PexpBinExpr(PbinIntOr, lexp, d_emit_c_variable rname)))
	  (d_emit_c_variable (List.hd ti.ti_typekind))
	  (List.tl ti.ti_typekind)
      in
      Some 
	(d_emit_initlist
	   [d_emit_initexp (d_emit_c_variable "EMIT_HEADER_FOR_TYPEINFO");
	    d_emit_initlist
	      [d_emit_initexp (d_emit_c_string ti.ti_typename);
	       d_emit_initexp typekind;
	       d_emit_initexp
		 (match ti.ti_refertype with
		   None -> d_emit_c_variable "NULL"
		 | Some s -> d_emit_exp (PexpAddress 
					 (d_emit_exp (PexpField(d_emit_c_variable s, "val")))
				      ));
	       d_emit_initexp (d_emit_c_integer ti.ti_virtual_size);
	       d_emit_initexp (d_emit_c_integer ti.ti_real_size);
	       d_emit_initexp (d_emit_c_variable ti.ti_read_dword);
	       d_emit_initexp (d_emit_c_variable ti.ti_read_word);
	       d_emit_initexp (d_emit_c_variable ti.ti_read_hword);
	       d_emit_initexp (d_emit_c_variable ti.ti_read_byte);
	       d_emit_initexp (d_emit_c_variable ti.ti_write_dword);
	       d_emit_initexp (d_emit_c_variable ti.ti_write_word);
	       d_emit_initexp (d_emit_c_variable ti.ti_write_hword);
	       d_emit_initexp (d_emit_c_variable ti.ti_write_byte);
	       d_emit_initexp (d_emit_c_variable ti.ti_additionals);
	     ]])
    end
  in
  d_emit_decl
    (PdeclVariable
       ([TypeSpec (PtypespecStruct(Struct, Some "typeinfo_init", None, [], Locterm.dummy_location)) ] 
	@ 
	  (if is_external || !Transutil.compiler_mode = MultiModule then
	    [StorageClass Extern]
	  else if !Transutil.compiler_mode = MultiLinker then
	    []
	  else
	    [weak_attribute]
	  ),
	[PinitDecl (PdeclIdent ti.ti_tiname, body)]))

let encoded_name_of_type ~genv = Translate_to_il3.encoded_name_of_type_genv ~genv

let typeinfo_name ~genv t = "fsc_typeinfo_" ^ encoded_name_of_type ~genv t

let use_alignpad = ref false

let add_alignpad_for_struct vsize (continuous_p, size, align as ret) = 
  if continuous_p || not !use_alignpad then ret, 0
  else
    let m = 
      let rec iter s = if s land 1 == 0 then iter (s / 2) else s in
      iter vsize * align in
    let s = (size + m - 1) / m * m in
    dprintf 5 "alignpad %d %d -> (%d) %d" vsize size m s;
    (continuous_p, s, align), s - size

let roundup_to_align size align = (size + align - 1) / align * align

let rec calculate_real_size_and_align_of_struct ~env id st = 
  if earray_mem env.struct_size_cache id then
    let { ss_continuous_p = cp; ss_real_size = rs; ss_align = al } = earray_get env.struct_size_cache id in
    cp, rs, al
  else begin
    let rec iter continuous_p size align = function
	[] ->
	  dprintf 5 "iter <%d> end: %b %d %d" id continuous_p size align;
	  continuous_p, roundup_to_align size align, align
      | (_, NormalField nf) :: tl ->
	  let ty = nf.sf_type in
	  let cp_field, csize, calign =
	    calculate_real_size_and_align ~env ty in
	  dprintf 5 "iter <%d>: %d %d: %d %d %a" id size align csize calign Ctt_formatter.pp_c_type ty;
	  let start = roundup_to_align size calign in 
	  if start <> size then
	    assert(start - size = 4);
	  iter (continuous_p && cp_field) (start + csize) (max calign align) tl
      | (_, BitField _) :: _ ->
	  failwith "bitfield521: should not happen"
    in
    dprintf 5 "iter start <%d>: %d %d" id 0 1;
    let r = iter true 0 1 st.str_fields in
    let (cp, s, al as r), ap = 
      match st.str_size with
	None -> r, 0
      | Some sz -> add_alignpad_for_struct (int_of_big_int sz) r in
    earray_set env.struct_size_cache id 
      { ss_continuous_p = cp; ss_real_size = s; ss_align = al; ss_alignpad = ap };
    r
  end

and calculate_real_size_and_align ~env ty = 
  let cp, csize, calign = 
  match ty.ct_ty with
    Tbuiltin _ | Tpointer _ ->
      let p = parse_type_genv ~genv:env.genv ty in
      let csize = int_of_big_int (someof ty.ct_size) in
      let calign = int_of_big_int (someof ty.ct_align) in
      let csize = if p.is_fat then csize * 2 else csize in
      let calign = if p.is_fat && p.size = Size_word then calign * 2 else calign in
      not p.is_fat, csize, calign
  | Tstruct id ->
      let st = get_struct_desc ~genv:env.genv id in
      calculate_real_size_and_align_of_struct ~env id st
  | Tarray(ty, Some sz) ->
      let cp, csize, calign = calculate_real_size_and_align ~env ty in
      cp, (int_of_big_int (mult_big_int (big_int_of_int csize) sz)), calign
  | Tarray(ty, None) ->
      assert false
  | Tvoid | Tabstract _ | Tfunction _ ->
      assert false
  in
  dprintf 5 "size <%a>: cont=%b sz=%d algn=%d" Ctt_formatter.pp_c_type ty cp csize calign;
  cp, csize, calign

let make_typeinfo_dat ~genv ~vsize ~rsize
    ?read_dword ?read_word ?read_hword ?read_byte
    ?write_dword ?write_word ?write_hword ?write_byte ?additionals ?functail ~cont_p typ =
  (* check *)
  (*if not (eq_big_int (Option.default zero_big_int typ.ct_size) (big_int_of_int vsize)) then
    failwith_p "add_support_funcs 81: PANIC: size mismatch %s <> %s for %s"
    (Option.default "[no-size]" (Option.map string_of_big_int typ.ct_size))
    (string_of_int vsize) (Ctt_formatter.pp_c_type typ); *)
  let typekind, refertype = match typ.ct_ty with
    Tbuiltin b -> assert false (*; "TI_PRIMITIVE", None*)
  | Tpointer r -> "TI_POINTER", Some (typeinfo_name ~genv r)
  | Tfunction(at,vp,rt) -> "TI_FUNCTION", Some (typeinfo_name ~genv rt)
  | Tarray _ -> assert false;
  | Tstruct _ -> "TI_STRUCT", None;
  | Tabstract _ -> assert false(* ; "TI_SPECIAL", None; *)
  | Tvoid -> assert false;
  in
  let typekind = if cont_p then [typekind; "TI_CONTINUOUS"] else [typekind] in
  let typekind = if vsize = 0 then typekind @ ["TI_NO_USERALLOC"] else typekind in
  let tail dir = match functail with
    Some s -> s
  | None ->
      if vsize = sizeof_char then dir ^ "_byte"
      else if vsize = sizeof_short then dir ^ "_hword"
      else if vsize = sizeof_longlong then dir ^ "_dword"
      else if vsize = sizeof_long then dir ^ "_word"
      else failwith "97 unsupported size"
  in
  let decide size funcopt gen dir =
    match funcopt with
      Some s -> s
    | None ->
	if size = vsize && functail = None then
	  failwith_p "make_typeinfo: PANIC: 80 %d %d %a"
	    size vsize Ctt_formatter.pp_c_type typ
	else gen ^ "_" ^ tail dir
  in
  let additionals = 
    match additionals with
      Some s -> s
    | None -> if cont_p then "FSC_ADDITIONAL_HELPERS_CONTINUOUS" else "FSC_ADDITIONAL_HELPERS_DEFAULT"
  in
  {
   ti_typename = sfprintf "%a" (Ctt_formatter.pp_c_type_long ~genv ()) typ;
   ti_tiname = typeinfo_name ~genv typ;
   ti_typekind = typekind; ti_refertype = refertype;
   ti_virtual_size = vsize;
   ti_real_size = rsize;
   ti_read_byte = decide sizeof_char read_byte "read_byte" "by";
   ti_read_hword = decide sizeof_short read_hword "read_hword" "by";
   ti_read_word = decide sizeof_pointer read_word "read_word" "by";
   ti_read_dword = decide sizeof_longlong read_dword "read_dword" "by";
   ti_write_byte = decide sizeof_char write_byte "write_byte" "to";
   ti_write_hword = decide sizeof_short write_hword "write_hword" "to";
   ti_write_word = decide sizeof_pointer write_word "write_word" "to";
   ti_write_dword = decide sizeof_longlong write_dword "write_dword" "to";
   ti_additionals = additionals;
 }

let make_typeinfo ~env typ = 
  let genv = env.genv in
  match typ.ct_ty with
    Tpointer _ ->
      let dat = 
	make_typeinfo_dat ~genv ~vsize:sizeof_pointer
	  ~rsize:(sizeof_pointer * 2)
	  ~read_word:"read_word_fat_pointer" ~write_word:"write_word_fat_pointer" ~cont_p:false typ
      in
      [d_emit_typeinfo ~proto:true dat],
      (if !Transutil.compiler_mode = MultiModule then [] else [d_emit_typeinfo ~proto:false dat]),
      []
  | Tbuiltin _ | Tvoid -> [], [], []
  | Tstruct id ->
      let st = get_struct_desc ~genv id in
      let functail = encoded_name_of_type ~genv typ in
      let vsize = int_of_big_int (Option.default zero_big_int st.str_size) in
      let cont_p, rsize, _ = calculate_real_size_and_align ~env typ in
      let dat = 
	if cont_p then
	  make_typeinfo_dat ~cont_p ~genv ~vsize
	    ~rsize
	    ~functail:"continuous" typ
	else
	  make_typeinfo_dat ~cont_p ~genv ~vsize
	    ~rsize
	    ~functail typ
      in
      if not (List.mem_assoc "external" st.str_extension) 
	  && not (List.mem_assoc "named" st.str_extension 
		    && not (!Transutil.compiler_mode = MultiLinker)
		    && not (List.mem id !specially_emit_structs)
		 ) then begin
	  (* access methods *)
	let realofsmeth = 
	  if st.str_size = None then []
	  else [make_realoffset_method ~genv ~id ~vsize ~rsize] in
	let accmeth = 
	  if !Transutil.compiler_mode = MultiModule then [] else
	  if cont_p then make_rw_method_for_struct ~genv ~id else
	  make_access_methods_for_struct ~genv ~id
	  @ make_rw_method_for_struct ~genv ~id
	in
	(* typeinfo *)
	[d_emit_typeinfo ~proto:true dat], 
	(if !Transutil.compiler_mode = MultiModule then [] else [d_emit_typeinfo ~proto:false dat]),
	realofsmeth @ accmeth
      end else begin
	let realofsmeth = 
	  if st.str_size = None then []
	  else [make_realoffset_method ~genv ~id ~vsize ~rsize] in
	(* methods for named structure are already built in standard library *)
	[d_emit_typeinfo ~proto:true ~is_external:true dat], [], realofsmeth
      end
  | Tfunction _ ->
      let dat = 
	make_typeinfo_dat ~genv ~vsize:4 ~rsize:8 ~cont_p:false
	  ~read_word:"read_word_noaccess" ~read_dword:"read_dword_noaccess"
	  ~read_hword:"read_hword_noaccess" ~read_byte:"read_byte_noaccess" 
	  ~write_word:"write_word_noaccess" ~write_dword:"write_dword_noaccess"
	  ~write_hword:"write_hword_noaccess" ~write_byte:"write_byte_noaccess"
	  typ
      in
      [d_emit_typeinfo ~proto:true dat],
      (if !Transutil.compiler_mode = MultiModule then [] else [d_emit_typeinfo ~proto:false dat]), 
      []
  | _ -> failwith "unimp144"

(**************** STORAGE ****************)

let name_of_storage ~genv t s = 
  let ss = match s with
    None -> "s"
  | Some s -> string_of_big_int s
  in
  "fsc_storage_" ^ encoded_name_of_type ~genv t
  ^ "_" ^ ss

let name_of_blockheader = "fsc_header"
let name_of_header_in_block = "hdr"
let name_of_data_in_block = "val"

let builtintype_is_fat_in_block = 
  function
      Tdouble | Tfloat | Tlongdouble -> false
    | bt -> Ctt_abstree.size_of_builtin_type bt >= sizeof_pointer

let declspec_of_data_in_block ~genv typ = 
  match typ.ct_ty with
    Tbuiltin bt when builtintype_is_fat_in_block bt ->
      [TypeSpec (PtypespecStruct
		   (Union,
		    Some ("fsc_initU_" ^ Translate_to_il3.encoded_name_of_btype bt), None, [], Locterm.dummy_location))]
  | Tbuiltin bt ->
      builtin_to_typespec bt
  | Tstruct sid ->
      [TypeSpec (PtypespecStruct
		   (Struct, Some (name_of_struct ~genv sid), None, [], Locterm.dummy_location))]
  | Tpointer bt ->
      [TypeSpec (PtypespecStruct(Union, Some "fsc_initUptr", None, [], Locterm.dummy_location))]
  | Tfunction _ -> (* TODO: check *)
      [TypeSpec (PtypespecStruct(Struct, Some "fsc_function_stub", None, [], Locterm.dummy_location))]
  | _ -> 
      failwith_p "panic:asc727: %a" Ctt_formatter.pp_c_type typ (*; assert false*)

let make_storage ~genv typ sizeopt = 
(*  let name = name_of_storage ~genv typ sizeopt in *)
  let decl = 
  [ PstructDecl
      ([TypeSpec (PtypespecStruct(Struct, Some name_of_blockheader, None, [], Locterm.dummy_location))],
       [PstructDeclNormal (PdeclIdent name_of_blockheader)]);
    PstructDecl
      (declspec_of_data_in_block ~genv typ,
       [PstructDeclNormal
	  (match sizeopt with
	    None -> PdeclIdent name_of_data_in_block
	  | Some s -> PdeclArray(PdeclIdent name_of_data_in_block,
				 Some (d_emit_c_big_integer s)))])]
  in
  d_emit_decl
    (PdeclVariable
       ([TypeSpec
	   (PtypespecStruct(Struct, 
			    Some (name_of_storage ~genv typ sizeopt), Some decl, [], Locterm.dummy_location))],
	[]))


(**************** STRUCTURE ****************)

let rec transform_structure ~env ~donemap ~put_f id =
  if not (Earray.get donemap id) then begin
    let genv = env.genv in
    Earray.set donemap id true;
    let st = Ctt_abstree.get_struct_desc ~genv id in
    assert (st.str_union_p = Struct);
      (* special structs do not need any definitions *)
    if List.mem_assoc "external" st.str_extension && st.str_size = None then
      ()
    else begin
      ignore (calculate_real_size_and_align_of_struct ~env id st);
      let alignpad = (earray_get env.struct_size_cache id).ss_alignpad in
      let name = name_of_struct ~genv id in
      let l = 
	let rec iter current_offset current_align = function
	    [] -> 
	      let start = roundup_to_align current_offset current_align in
	      let align_field = 
		if start <> current_offset then begin
		  if start - current_offset <> sizeof_int then
		    failwith "panic 1170: align required other than sizeof(int)"
		  else begin
		    [ PstructDecl ([], [PstructDeclBitfield (None, d_emit_c_integer Fsc_config.max_bitfield_width)]) ]
		  end
		end else []
	      in
	      let alignpad_field = 
		if alignpad = 0 then [] else
		[ PstructDecl 
		    ([TypeSpec (PtypespecBuiltin Char)],
		     [PstructDeclNormal
			(PdeclArray (PdeclIdent "__alignpad" , Some (d_emit_c_integer alignpad)))]) ]
	      in
	      align_field @ alignpad_field
	  | (_, NormalField { sf_id = id; sf_type = typ; sf_size = siz }) :: rest ->
	      let () = 
		match typ.ct_ty with
		  Tstruct id | Tarray({ ct_ty = Tstruct id }, _) ->
		    transform_structure ~env ~donemap ~put_f id;
		| _ -> ()
	      in
	      let _, sz, align = calculate_real_size_and_align ~env typ in
	      let start = roundup_to_align current_offset align in
	      let align_field = 
		if start <> current_offset then begin
		  if start - current_offset <> sizeof_int then
		    failwith "panic 1170: align required other than sizeof(int)"
		  else begin
		    [ PstructDecl ([], [PstructDeclBitfield (None, d_emit_c_integer Fsc_config.max_bitfield_width)]) ]
		  end
		end else []
	      in
	      let current_align = max current_align align in
	      let entry =
		match typ.ct_ty with
		  Tbuiltin _ | Tstruct _ | Tpointer _ ->
		    let sq = declspec_of_data_in_block ~genv typ in
		    let dc = PdeclIdent (translate_field_name id) in
		    PstructDecl (sq, [PstructDeclNormal dc])
		| Tarray(typ, Some sz) ->
		    let sq = declspec_of_data_in_block ~genv typ in
		    let dc = PdeclIdent (translate_field_name id) in
		    let dc = PdeclArray(dc, Some (d_emit_c_big_integer sz)) in
		    PstructDecl (sq, [PstructDeclNormal dc])
		| Tarray _ | Tfunction _ | Tabstract _ | Tvoid -> assert false
	      in
	      align_field @ (entry :: iter (start + sz) current_align rest)
	  | (_, BitField _) :: _ ->
	      failwith "unused437bitfield"
	in
	iter 0 1 st.str_fields
      in
      put_f
	(d_emit_decl
	   (PdeclVariable
	      ([TypeSpec 
		  (PtypespecStruct
		     (Struct, Some name, Some l, [], Locterm.dummy_location))], [])))
    end
  end else ()

let transform_struct_table ~env = 
  let structs = ref [] in
  let l = env.genv.struct_table in
  let donemap = Earray.empty_with_default ~zero:false in
  let put v = structs := !structs @ [v] in
  Earray.iteri
    (fun id decl ->
      transform_structure ~env ~donemap ~put_f:put id) l;
  !structs

(**************** BASE CAST FUNCTION ****************)

(*
base_t basecast_P<tt> (base_t b, ofs_t o) {
  base_t b0 = base_remove_castflag(b);
  if (b0 && (&typeinfo_<tt> == get_header_fast(b0)->tinfo) &&
	(o % sizeof(<tt>) == 0))
     return bb;
  else
     return base_put_castflag(bb);
}
*)

let make_basecast_func ~env t =
  let genv = env.genv in
  match t.ct_ty with
    Tpointer tt ->
      let bc_name = "set_base_castflag_" ^ encoded_name_of_type ~genv t in
      let ptrv_name = "ptrvalue_of_value_" ^ encoded_name_of_type ~genv t in
      let emit = 
	match tt.ct_ty with
	  Tvoid -> false
(*	Tstruct id ->
	  not (List.mem_assoc "external" (get_struct_desc ~genv id).str_extension) *)
	| _ -> true
      in
      if emit then begin
	let p_tt = parse_type_genv ~genv tt in
	let bc_decl = 
	  let b = d_emit_c_variable "b" in
	  let o = d_emit_c_variable "o" in
	  let body =
	    let b0 = d_emit_c_variable "b0" in
	    let hp = d_emit_invokeident "get_header_fast" [b0] in
	    let hti = d_emit_c_variable "tinfo" in
	    let cond1 = b0 in
	    let cond2 = 
	      let cond2_exact =
		let ti1 = d_emit_exp (PexpVar(typeinfo_name ~genv tt)) in
		let ti0 = d_emit_exp (PexpField(ti1, "val")) in
		let ti = d_emit_exp (PexpAddress ti0) in
		d_emit_exp (PexpBinExpr (PbinEqual, ti, hti))
	      in
	      if
		!Transutil.use_optimized_narrow_cast &&
		( match tt.ct_ty with
		  Tbuiltin _ -> not p_tt.is_fat
		| Tstruct _ -> 
		    let cont_p, rsize, _ = calculate_real_size_and_align ~env tt in
		    cont_p
		| _ -> false) &&
		( tt.ct_size <> None )
	      then
		let tiflag = d_emit_exp (PexpPtrField(hti, "kind")) in
		let is_cont = d_emit_exp (PexpBinExpr(PbinIntAnd, tiflag, d_emit_exp (PexpVar "TI_CONTINUOUS"))) in
		let size_ht = d_emit_exp (PexpPtrField(hti, "virtual_size")) in
		let size_chk = 
		  d_emit_exp
		    (PexpBinExpr
		       (PbinEqual, 
			d_emit_c_integer 0,
			d_emit_exp (PexpBinExpr (PbinModulo, size_ht, d_emit_c_big_integer (Option.get tt.ct_size))))) in
		d_emit_exp
		  (PexpBinExpr
		     (PbinLogOr,
		      cond2_exact,
		      (d_emit_exp
			 (PexpBinExpr
			    (PbinLogAnd, is_cont, size_chk)))))
	      else
		cond2_exact
	    in
	    let cond3 =
	      match tt.ct_size with
		Some s ->
		  let sz = d_emit_c_big_integer s in
		  let cmp = d_emit_exp (PexpBinExpr (PbinModulo, o, sz)) in
		  d_emit_exp (PexpBinExpr (PbinEqual, cmp, d_emit_c_integer 0))
	      | None ->
		  d_emit_exp (PexpBinExpr (PbinEqual, o, d_emit_c_integer 0))
	    in
	    let cond = d_emit_exp (PexpBinExpr (PbinLogAnd, cond2, cond3)) in
	    let exp_set = d_emit_invokeident "base_put_castflag" [b0] in
	    let ret_clear = d_emit_stmt (PstmtReturn (Some b0)) in
	    let ret_set = d_emit_stmt (PstmtReturn (Some exp_set)) in
	    let ifstmt = d_emit_stmt (PstmtIf (cond, ret_clear, Some ret_set)) in
	    let main_block = d_emit_compound
		[pdeclspec_typeinfo_t, PdeclIdent "tinfo",
		 Some (d_emit_exp (PexpPtrField (hp, "tinfo")))]
		[ifstmt]
	    in
	    let body = 
	      d_emit_stmt (PstmtIf (cond1, main_block, Some ret_set))
	    in
	    d_emit_compound
	      [pdeclspec_base_t, PdeclIdent "b0",
	       Some (d_emit_invokeident "base_remove_castflag" [b])]
	      [body]
	  in
	  d_emit_decl
	    (PdeclFunction 
	       ((StorageClass Inline) :: (StorageClass Static) :: pdeclspec_base_t,
		PdeclFuncType(PdeclIdent bc_name,
			      [PpdeclConcrete(pdeclspec_base_t, PdeclIdent "b");
			       PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "o")]),
		[], body))
	in
	let ptrv_decl = 
	  (* base_t b = base_of_value(v); ofs_t o = ofs_of_value(v);
             return ptrvalue_of_base_ofs(set_base_castflag_<T>(b, o), o); *)
	  let v = d_emit_c_variable "v" in
	  let b = d_emit_c_variable "b" in
	  let o = d_emit_c_variable "o" in
	  let b_def = d_emit_invokeident "base_of_value" [v] in
	  let o_def = d_emit_invokeident "ofs_of_value" [v] in
	  let retbase = d_emit_invokeident bc_name [b; o] in
	  let retval = d_emit_invokeident "ptrvalue_of_base_ofs" [retbase; o] in
	  let body = d_emit_compound
	      [pdeclspec_base_t, PdeclIdent "b", Some b_def;
	       pdeclspec_ofs_t, PdeclIdent "o", Some o_def]
	      [d_emit_stmt (PstmtReturn (Some retval))]
	  in
	  d_emit_decl
	    (PdeclFunction 
	       ((StorageClass Inline) :: (StorageClass Static) :: pdeclspec_ptrvalue_t,
		PdeclFuncType(PdeclIdent ptrv_name,
			      [PpdeclConcrete(pdeclspec_value_t, PdeclIdent "v")]),
		[], body))
	in
	[ bc_decl; ptrv_decl ]
      end
      else []
  | _ -> assert false

(**************** PROTOTYPE FOR STRUCT READ/WRITE FUNCTIONS *************)

let make_proto_rw_method ~genv ct = 
  let id = match ct.ct_ty with Tstruct id -> id | _ -> assert false in
  let struct_decl = emit_pdeclspec_namedstruct (name_of_struct ~genv id) in
  let rmethname = "read_struct_" ^ (encoded_name_of_struct ~genv id) in
  let wmethname = "write_struct_" ^ (encoded_name_of_struct ~genv id) in
  [
   d_emit_decl
     (PdeclVariable
	((StorageClass Extern) :: struct_decl,
	 [PinitDecl
	    (PdeclFuncType
	       (PdeclIdent (rmethname),
		[PpdeclConcrete(pdeclspec_base_t, PdeclIdent "base");
		 PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs")]),
	     None)]));
   d_emit_decl
     (PdeclVariable
	([StorageClass Extern; TypeSpec (PtypespecBuiltin Void)],
	 [PinitDecl
	    (PdeclFuncType
	       (PdeclIdent (wmethname),
		[PpdeclConcrete(pdeclspec_base_t, PdeclIdent "base");
		 PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "ofs");
		 PpdeclConcrete(struct_decl, PdeclIdent "val")]),
	     None)]))
 ]

(**************** ADD POINTER FUNCTION ****************)

(*
ptrvalue add_fat_pointer_<tt> (base_t b, ofs_t o1, ofs_t o2) {
   unsigned long long o = (o1 + (long long)(long)o2 * (long)sizeof<tt>);
   if (o > 0xffffffff)
     return ptrvalue_of_base_ofs(set_base_castflag_<T>(b), (ofs_t)o);
   else
     return ptrvalue_of_base_ofs(b, (ofs_t)o);
}
*)
let make_addptr_func ~genv t =
  match t.ct_ty with
    Tpointer tt ->
      let ap_name = "add_fat_pointer_" ^ encoded_name_of_type ~genv t in
      let bc_name = "set_base_castflag_" ^ encoded_name_of_type ~genv t in
      let sz = 
	match tt.ct_size with Some s -> d_emit_c_big_integer s | _ -> assert false
      in
      let ap_decl = 
	let b = d_emit_c_variable "b" in
	let o1 = d_emit_c_variable "o1" in
	let o2 = d_emit_c_variable "o2" in
	let body =
	  let long_o2 = d_emit_cast_to_typedefname "signed_ofs_t" o2 in
	  let longlong_o2 = d_emit_cast_to_typedefname "signed_dw_ofs_t" long_o2 in
	  let long_sz = d_emit_cast_to_typedefname "signed_ofs_t" sz in
	  let o2s = d_emit_exp (PexpBinExpr (PbinTimes, longlong_o2, long_sz)) in
	  let o_exp = d_emit_exp (PexpBinExpr (PbinPlus, o1, o2s)) in
	  let o = d_emit_c_variable "o" in
	  let long_o = d_emit_cast_to_typedefname "ofs_t" o in
	  let body_checkpart = 
	    let b_updated = d_emit_invokeident bc_name [b; long_o] in
	    let e1 = d_emit_invokeident "ptrvalue_of_base_ofs" [b_updated; long_o] in
	    d_emit_stmt (PstmtReturn (Some e1))
	  in
	  let body_throughpart = 
	    let e1 = d_emit_invokeident "ptrvalue_of_base_ofs" [b; long_o] in
	    d_emit_stmt (PstmtReturn (Some e1))
	  in
	  let cond = d_emit_exp (PexpBinExpr (PbinGtrThan, o, d_emit_c_variable "MAX_OFS_T")) in
	  let stmt_if = 
	    d_emit_stmt (PstmtIf (cond, body_checkpart, Some body_throughpart))
	  in
	  d_emit_compound
	    [pdeclspec_unsigned_dw_ofs_t, PdeclIdent "o",
	     Some o_exp]
	    [stmt_if]
	in
	d_emit_decl
	  (PdeclFunction 
	     ((StorageClass Inline) :: (StorageClass Static) :: pdeclspec_ptrvalue_t,
	      PdeclFuncType(PdeclIdent ap_name,
			    [PpdeclConcrete(pdeclspec_base_t, PdeclIdent "b");
			     PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "o1");
			     PpdeclConcrete(pdeclspec_ofs_t, PdeclIdent "o2")
			   ]),
	      [], body))
      in
      [ ap_decl ]
  | _ -> assert false

(**************** STRING CONSTANTS ****************)

let make_string_constant ~genv id s =
  let len = String.length s in
  let ci c =
    d_emit_initexp
      (d_emit_exp (PexpConstant (PconstInteger (string_of_int (Char.code c))))) in
  let charsinit = 
    let rec iter p = 
      if p >= len then [] else
      ci s.[p] :: iter (p + 1)
    in
    iter 0
  in
  let name = get_global_string_storage_name ~genv id in
  let name_str = name_of_storage ~genv type_char (Some (big_int_of_int len)) in
  d_emit_decl
    (PdeclVariable
       ([StorageClass Static; TypeSpec (PtypespecStruct (Struct, Some name_str, None, [], Locterm.dummy_location))],
	[PinitDecl
	   (PdeclIdent(name),
	    Some (d_emit_initlist [d_emit_initexp 
			       (d_emit_exp
				  (PexpInvoke
				     (d_emit_exp (PexpVar "EMIT_FSC_HEADER"),
				      [d_emit_exp (PexpField (d_emit_exp (PexpVar "fsc_typeinfo_c"), "val"));
				       d_emit_exp (PexpConstant (PconstInteger (string_of_int len)))])));
			     d_emit_initlist charsinit]))]))

(**************** MAIN ROUTINE ****************)

(* emit order :                             (dependency) *)
(* 1. prototypes for structures, 
   4. prototypes for typeinfo,
   2. structures,                           (1)
   7. basecast and getrealofs.              (4)
   9. add_fat_pointer_                      (7)
   3. access methods for structures         (2, 7)
   5. typeinfo,                             (2, 3, 4)
   6. storage,                              (2, 4)
   8. string constants                      (6)
  10. stub blocks for extern functions
*)

let f_standalone ~genv t = 
  let env = { genv = genv; struct_size_cache = Earray.empty () } in
  Record_globalnames.add_struct_dependency ~genv ();
  let gn = Record_globalnames.to_list () in
  let accmeth = ref [] in
  let tiproto, tinfo = ref [], ref [] in
  let storage = ref [] in
  let funcs = ref [] in
  let funcs2 = ref [] in
  let strings = ref [] in
  let add_l s l = s := l @ !s in
  let add s v = s := v :: !s in
  let extern_decls = ref [] in
  List.iter
    (function
	GNtypeinfo t ->
	  let p, r, am = make_typeinfo ~env t in
	  add_l tiproto p; add_l tinfo r; add_l accmeth am
      | GNstorage(t,sz) ->
	  add storage (make_storage ~genv t sz)
      | GNbasecast(t) ->
	  add_l funcs (make_basecast_func ~env t)
      | GNaddptr(t) ->
	  add_l funcs2 (make_addptr_func ~genv t)
      | GNgetrealofs(t) -> ()
      | _ -> ())
    gn;
  let structs = transform_struct_table ~env in
  List.iter
    (fun (s, id) ->
      add strings (make_string_constant ~genv id s))
    (Record_globalnames.get_global_string_list ());
  List.concat [structs; !tiproto; !funcs; !funcs2; !accmeth; !tinfo; !storage; !strings; !extern_decls; t]

let make_struct_info ~genv = 
  let find_struct_name ~genv id = 
    let rec iter = function
	[] -> "-"
      | (nam, v) :: t ->
	  if v = id then nam else iter t
    in
    iter genv.struct_name_table
  in
  let r = Glist.empty () in
  Earray.iteri
    (fun id desc ->
      Glist.put r (
      assert(desc.str_union_p = Struct);
      match desc.str_size with
	None ->
	  Printf.sprintf "TSA\t%s\t%s" 
	    (encoded_name_of_struct ~genv id)
	    (find_struct_name ~genv id)
      | Some s -> 
	  Printf.sprintf "TSC\t%s\t%s\t%s"
	    (encoded_name_of_struct ~genv id)
	    (find_struct_name ~genv id)
	    (String.concat "" 
	       (List.map
		  (function
		      _, NormalField {sf_type=t; sf_id = s} ->
			string_of_int (String.length s) ^ s ^ "_" ^ (encoded_name_of_type ~genv t) ^ "_"
		    | _, BitField _ -> assert false)
		  desc.str_fields)))
    )
    genv.struct_table;
  Glist.to_list r
    
let encode_extension_linkinfo e = 
  let rec iter = 
    function
	[] -> ""
      | ("require_native_libraries", Estring s)::tl
	-> "\tRequire-native-libraries=" ^ String.escaped s ^ iter tl
      | ("noreturn", Elist []) :: tl -> iter tl
      | ("external", Elist []) :: tl -> iter tl
      | (s, _)::_ -> failwith_p "unimp2333: emitting extension %S" s
  in
  iter e

let make_linker_info ~genv ~mod_exts typeinfo_list orig_t = 
  let decl_info = Util.map_flatten
      (fun d -> match locval d with
	| Il0.IL0declFunction(ModuleStatic, _, _, _, _) -> []
	| Il0.IL0declFunction(Global ext, ct, id, _, _)
	| Il0.IL0declFunction(Ctt_abstree.Extern ext, ct, id, _, _) 
	| Il0.IL0declVariable(Global ext, ct, id, Some _)
	| Il0.IL0declVariable(Ctt_abstree.Extern ext, ct, id, Some _) ->
	    ["D\t" ^ id ^ "\t" ^ encoded_name_of_type ~genv ct ^ encode_extension_linkinfo ext]
	| Il0.IL0declVariable(Global ext, ct, id, None) -> begin
	    match ct.ct_ty with
	      Tfunction _ -> (* used in fscw *)
		["D\t" ^ id ^ "\t" ^ encoded_name_of_type ~genv ct ^ encode_extension_linkinfo ext]
	    | _ ->
		["E\t" ^ id ^ "\t" ^ encoded_name_of_type ~genv ct ^ 
		 "\tbss_data" ^ encode_extension_linkinfo ext]
	end
	| Il0.IL0declVariable(Ctt_abstree.Extern ext, ct, id, None) ->
	    if C_typing.is_type_loose_function ct then
	      ["E\t" ^ id ^ "\t" ^ "Xuf_" ^ encode_extension_linkinfo ext]
	    else
	      ["E\t" ^ id ^ "\t" ^ encoded_name_of_type ~genv ~array_squash:true ct
	       ^ encode_extension_linkinfo ext]
	| Il0.IL0declVariable(ModuleStatic, ct, id, _) -> []
      ) orig_t in
  let linker_info =
    String.concat "\n" (make_struct_info ~genv @ typeinfo_list @ decl_info) in
  let revision_info = 
    (Printf.sprintf "R\t2\tABI=%d,%d" abi_revision required_minimum_compiler_abi_revision)
  in
  let linker_info =
    revision_info ^ (String.concat "" (list_map (fun s -> "\t" ^ s) mod_exts)) ^ "\n" ^ linker_info
  in
  let linker_info = 
    "\n.pushsection __failsafeC_typeinfo, \"S\"\n\n"
    ^ ".asciz\t\"" ^ Ctt_to_ptree.encode_c_string linker_info
    ^ "\"\n\n.popsection\n"
  in
  let linker_info = "__asm__ (\"" ^ Ctt_to_ptree.encode_c_string linker_info ^ "\");\n"
  in
  d_emit_decl
    (PdeclPragmatic linker_info)
    
let f_multifile ~genv ?(mod_exts = []) orig_t t = 
  let env = { genv = genv; struct_size_cache = Earray.empty () } in
  Record_globalnames.add_struct_dependency ~genv ();
  let gn = Record_globalnames.to_list () in
  let realofsmeth = ref [] in
  let tiproto = ref [] in
  let storage = ref [] in
  let funcs = ref [] in
  let funcs2 = ref [] in
  let strings = ref [] in
  let linker_info = ref [] in
  let add_l s l = s := l @ !s in
  let add s v = s := v :: !s in
  let extern_decls = ref [] in
  List.iter
    (function
	GNtypeinfo t ->
	  let p, r, am = make_typeinfo ~env t in
	  assert (r = []);
	  add_l tiproto p; add_l realofsmeth am;
	  add linker_info ("TI\t" ^ (encoded_name_of_type ~genv t))
      | GNbasecast(t) ->
	  add_l funcs (make_basecast_func ~env t)
      | GNstorage(t,sz) ->
	  add storage (make_storage ~genv t sz)
      | GNaddptr(t) ->
	  add_l funcs2 (make_addptr_func ~genv t)
      | GNrwmeth(t) ->
	  add_l funcs2 (make_proto_rw_method ~genv t)
      | _ -> ())
    gn;
  let structs = transform_struct_table ~env in
  List.iter
    (fun (s, id) ->
      add strings (make_string_constant ~genv id s))
    (Record_globalnames.get_global_string_list ());
  let linker_info = make_linker_info ~genv ~mod_exts !linker_info orig_t in
  List.concat [structs; !tiproto; !funcs; !funcs2; !realofsmeth; !storage; !strings; !extern_decls; t; [linker_info]]
  
let f ~genv ~orig_t ?mod_exts t = 
  if !compiler_mode = MultiModule then f_multifile ~genv ?mod_exts orig_t t
  else f_standalone ~genv t
