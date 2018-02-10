(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2005. *)

open Ctt_abstree
open Ctt_visitor
open Locterm
open Big_int

let new_label ~e =
  incr e;
  "__pad" ^ string_of_int !e

let rec repeat e = function
    0 -> []
  | n -> e :: repeat e (n - 1)

let constant_zero_init ~loc =
  locput ~loc
    (CTTinitExp
       (Ctt_abstree.make_expr ~loc
	  (CTTexpConstant (CTTconstInteger zero_big_int)) type_char))

let new_field ~e ~next ~pos = 
  let n = sub_big_int next pos in
  let label = new_label ~e in
  let t = make_c_type ~const:true (Tarray(type_char, Some n)) in
  (pos, 
   NormalField { sf_id = label; sf_type = t; sf_size = n })

let rec iter_on_fields tailpos pos ~e = function
    [] ->
      if eq_big_int tailpos pos then [] else
      [ new_field ~e ~next:tailpos ~pos ]
  | (start, field as f) :: tl when eq_big_int start pos ->
      let fsize = match field with
	NormalField f -> f.sf_size
      | BitField bf -> bf.s_bf_size
      in
      let next = add_big_int start fsize in
      f::(iter_on_fields tailpos next ~e tl)
  | ((start,_) :: _) as l ->
      let new_field = new_field ~e ~next:start ~pos in
      new_field::(iter_on_fields tailpos start ~e l)

let translate_struct_desc desc = 
  match desc.str_size with
    None -> desc
  | Some s -> 
      C_typing.update_fields_cache
	{ desc with
	  str_fields = iter_on_fields s zero_big_int ~e:(ref 0) desc.str_fields
	}

let translate_environment genv =
  { genv with
    struct_table = Util.Earray.map translate_struct_desc genv.struct_table }

let rec translate_struct_initializer ~genv ~loc id inits = 
  let desc = get_struct_desc ~genv id in
  match desc.str_union_p with
    Struct -> begin
      let rec iter tailpos pos fields inits = 
	match fields, inits with
	  [], [] -> begin
	    if eq_big_int tailpos pos then []
	    else
	      let n = int_of_big_int (sub_big_int tailpos pos) in
	      [ locput ~loc (CTTinitList (repeat (constant_zero_init ~loc) n)) ]
	  end
	| (start, f)::tl, (ini :: rest) ->
	    if eq_big_int start pos then begin
	      match f with
		NormalField { sf_type = t; sf_size = sz } ->
		  translate_initializer ~genv t ini ::
		  iter tailpos (add_big_int start sz) tl rest
	      | BitField { s_bf_fields = bf; s_bf_size = sz } ->
		  iter_bfields tailpos tl (add_big_int start sz) bf inits
	    end
	    else begin
	      let n = int_of_big_int (sub_big_int start pos) in
	      let i = 
		locput ~loc
		  (CTTinitList(repeat (constant_zero_init ~loc) n)) in
	      i :: iter tailpos start fields inits
	    end
	| _::_, [] | [], _::_ -> assert false
      and iter_bfields tailpos rest_fields next_start bfields inits = 
	match bfields, inits with
	  [], rest_inits -> iter tailpos next_start rest_fields inits 
	| (_,t,_,_)::tl, (init :: rest) ->
	    (translate_initializer ~genv t init)
	    :: iter_bfields tailpos rest_fields next_start tl rest
	| _::_, [] -> assert false
      in
      match desc.str_size with
	Some s -> iter s zero_big_int desc.str_fields inits
      | None -> inits
    end
  | Union -> 
      visit_struct_initializer ~v_init:(translate_initializer ~genv) ~genv ~loc id inits

and translate_initializer ~genv t =
  Ctt_visitor.visit_initializer
    ~v_expr:(fun x -> x)
    ~v_strinit:(translate_struct_initializer ~genv)
    ~self:(translate_initializer ~genv)
    t

let translate_ldecl ~genv (lsclass, typ, id, initopt) =
  let initopt = Option.map (translate_initializer ~genv typ) initopt in
  (lsclass, typ, id, initopt)

let rec translate_statement ~genv statement =
  visit_statement
    ~v_type:(fun x -> x)
    ~v_expr:(fun x -> x) 
    ~v_ldecl:(translate_ldecl ~genv)
    ~self:(translate_statement ~genv)
    statement

let rec translate_gdecl ~genv decl =
  locmap
    (function
	CTTdeclFunction(gsclass, typ, ident, argnames, body) ->
	  let new_body = translate_statement ~genv body in
	  CTTdeclFunction(gsclass, typ, ident, argnames, new_body)
      | CTTdeclVariable(gsclass, typ, ident, initopt) ->
	  CTTdeclVariable(gsclass, typ, ident,
			  Option.map (translate_initializer ~genv typ) initopt))
    decl

let rec translate_program ~genv prog = 
  let prog = Util.list_map (translate_gdecl ~genv) prog
  in
  let genv = translate_environment genv in
  genv, prog

