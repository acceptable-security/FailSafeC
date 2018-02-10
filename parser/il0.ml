(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2004. *)

open Big_int
open Ctt_abstree
open Cttm_abstree
open Int_set (* open Set_list *)
open Il
open Topological_sort
open Locterm
open Il_formatter
open Format

type il0_loop_label = BREAK | CONTINUE
type identifier = string
type label = string

exception Error

type temp_id = int

type initfield = 
    Iindexed of big_int
  | Inamed of identifier

type il0_desc =
(* basically from statement *)
  | IL0stmtLabel of label
  | IL0stmtDeclAutoScalar of local_storage_class * c_type * identifier * temp_id option
  | IL0stmtDeclBulk of local_storage_class * c_type * identifier * Il.il_initializer option (* TODO *)
  | IL0stmtIf of Il.il_if_type * temp_id * label
  | IL0stmtSwitch of temp_id * ( Il.il_switch_label * label ) list
  | IL0stmtGoto of label
  | IL0stmtReturn of temp_id option
  | IL0stmtAbort of Il.abort_reason
(* basically from expression *)
  | IL0stmtDefTemp of temp_id * c_type * Il.il_expr
  | IL0stmtReadToTemp of temp_id * c_type * temp_id Il.il_lvalue * field list
  | IL0stmtWrite of temp_id Il.il_lvalue * field list * temp_id
  | IL0stmtSequence of il0 list
  | IL0stmtParallel of il0 list (* Data-flow parallel *)

and il0_t = 
    { il0_depends : Int_set.t;
      il0_defines : Int_set.t;
      il0_nobranch : bool;
      il0_t : il0_desc; }

and il0 = il0_t Locterm.t

type il0_function_body = {
    il0_var_types : c_type Util.earray;
    il0_funcbody : il0
  }

type 'init il0_global_declaration_desc = 
    IL0declFunction of global_storage_class * c_type 
	* identifier * identifier list * il0_function_body
  | IL0declVariable of 
      global_storage_class * c_type * identifier * 'init option

type 'init il0_global_declaration = 
    'init il0_global_declaration_desc Locterm.t 

(*
let union_list l = 
  if List.length l < 40 then
    List.fold_left union empty l
  else begin
    let t = Hashtbl.create 16 in
    List.iter
      (fun l -> Set_list.iter (fun x -> Hashtbl.replace t x ()) l) l;
    let r = ref [] in
    Hashtbl.iter (fun x () -> r := x :: !r) t;
    Set_list.of_list !r
  end*)

let union_list l = 
  List.fold_left union empty l

let defines_il0 x = (locval x).il0_defines

let defines_desc = 
  function
      IL0stmtDefTemp(id,_,_) -> singleton id
    | IL0stmtReadToTemp(id,_,_,_) -> singleton id
    | IL0stmtSequence(l) -> union_list (Util.list_map defines_il0 l)
    | IL0stmtParallel(l) -> union_list (Util.list_map defines_il0 l)
    | _ -> empty

let depends_il0 x = x.locterm_v.il0_depends

let depends_il0lvalue =
  function
      ILlvPtr(id) -> singleton id
    | ILlvVar _ -> empty
    | ILlvTemp(id) -> singleton id

let depends_il0expr =
  function
      ILexpCoerce(_,id) -> singleton id
    | ILexpConstant _ -> empty
    | ILexpBinop(_,id1,id2) -> of_list [id1; id2]
    | ILexpUnaryop(_,id) -> singleton id
    | ILexpInvoke(lv,ids) -> union (depends_il0lvalue lv) (of_list ids)
    | ILexpAddress(lv,_) -> depends_il0lvalue lv
    | ILexpIdent(id) -> singleton id
    | ILexpArgument _ -> empty (* assert false *)
    | ILexpUndefined -> empty

let depends_desc = 
  function
      IL0stmtDefTemp(_,_,e) -> depends_il0expr e
    | IL0stmtReadToTemp(_,_,lv,_) -> depends_il0lvalue lv
    | IL0stmtSequence(l) -> union_list (Util.list_map depends_il0 l)
    | IL0stmtParallel(l) -> union_list (Util.list_map depends_il0 l)
    | IL0stmtIf(_,id,_) -> singleton id
    | IL0stmtSwitch(id,_) -> singleton id
    | IL0stmtReturn(Some id) -> singleton id
    | IL0stmtReturn(None)
    | IL0stmtAbort(_) -> empty
    | IL0stmtWrite(lv,_,id) -> union (singleton id) (depends_il0lvalue lv)
    | IL0stmtDeclAutoScalar(_,_,_,Some v) -> of_list [v]
    | IL0stmtDeclAutoScalar(_,_,_,None) -> empty
    | IL0stmtDeclBulk(_,_,_,_) -> empty
    | IL0stmtLabel _ 
    | IL0stmtGoto _ -> empty

let hasnobranch_desc = 
  function
    | IL0stmtLabel _ -> false
    | IL0stmtDeclAutoScalar _ -> true
    | IL0stmtDeclBulk _ -> true
    | IL0stmtIf _ -> false
    | IL0stmtSwitch _ -> false
    | IL0stmtGoto _ -> false
    | IL0stmtReturn _ -> false
    | IL0stmtAbort _ -> false
(* basically from expression *)
    | IL0stmtDefTemp _ -> true
    | IL0stmtReadToTemp _ -> true
    | IL0stmtWrite _ -> true
    | IL0stmtSequence l -> List.for_all (fun t -> t.locterm_v.il0_nobranch) l
    | IL0stmtParallel l -> List.for_all (fun t -> t.locterm_v.il0_nobranch) l

let make_il0 ~loc d = 
  let def = defines_desc d in
  let dep = depends_desc d in
  let dep = subtract dep def in
  locput ~loc
    {
     il0_defines = def;
     il0_depends = dep;
     il0_nobranch = hasnobranch_desc d;
     il0_t = d }

let rec separate_sequence ~loc acc = function
    [] -> 
      if acc = [] then []
      else [make_il0 ~loc (IL0stmtParallel (List.rev acc))]
  | e::tl ->
      if e.locterm_v.il0_nobranch then
	separate_sequence ~loc (e::acc) tl
      else
	if acc = [] then
	  e :: separate_sequence ~loc [] tl
	else
	  make_il0 ~loc (IL0stmtParallel (List.rev acc))
	  :: e :: separate_sequence ~loc [] tl

let enclose_sequence ~loc = function
    [i] -> i
  | is ->
      let is =
	Util.map_flatten
	  (fun i ->
	    match i.locterm_v.il0_t with
	      IL0stmtSequence is -> is
	    | _ -> [i]) is
      in
      make_il0 ~loc (IL0stmtSequence is)

let enclose_parallel ~loc = function
    [i] -> i
  | is -> 
      let is = List.rev (topological_sort ~depends:depends_il0 ~defines:defines_il0 is) in
      (* make_il0 (IL0stmtParallel is) *)
      let l = separate_sequence ~loc [] is in
      match l with
	[i] -> i
      |	is -> enclose_sequence ~loc is

let rec pp_il0_desc ppf = function
  | IL0stmtLabel l ->
      fprintf ppf "%s:" l
  | IL0stmtDeclAutoScalar (lsc, ct, id, None) ->
      fprintf ppf "@[decl_scalar %a %a@ %s@]"
	Ctt_formatter.pp_local_storage_class lsc pp_ctt_type ct id
  | IL0stmtDeclAutoScalar (lsc, ct, id, Some t1) ->
      fprintf ppf "@[decl_scalar %a %a@ %s =@ %a@]"
	Ctt_formatter.pp_local_storage_class lsc pp_ctt_type ct id pp_temp_id t1
  | IL0stmtDeclBulk (lsc, ct, id, None) ->
      fprintf ppf "@[decl_bulk %a %a@ %s@]"
	Ctt_formatter.pp_local_storage_class lsc pp_ctt_type ct id
  | IL0stmtDeclBulk (lsc, ct, id, Some init) ->
      fprintf ppf "@[decl_bulk %a %a %s =@;<1 2>%a]"
	Ctt_formatter.pp_local_storage_class lsc pp_ctt_type ct id
	pp_il_initializer init
  | IL0stmtGoto t -> fprintf ppf "goto %s" t
  | IL0stmtIf(ift, tid, l)
    -> fprintf ppf "%a %a then goto %s"
        pp_il_if_type ift pp_temp_id tid l
  | IL0stmtSwitch(tid, tlist) ->
      fprintf ppf "switch %a:@\n    @[<v>" pp_temp_id tid;
      List.iter
        (function l, t -> fprintf ppf "%a => %s@," pp_il_switch_label l t) tlist;
      fprintf ppf "@]"
  | IL0stmtReturn None -> fprintf ppf "return"
  | IL0stmtReturn (Some t1) -> fprintf ppf "return %a" pp_temp_id t1
  | IL0stmtAbort r -> fprintf ppf "abort %a" pp_il_abort_reason r
  | IL0stmtDefTemp (t1, ct, e) ->
      fprintf ppf "%a = %a\t/* <%a> */" pp_temp_id t1 pp_il_expr e pp_ctt_type ct
  | IL0stmtReadToTemp (tid, ct, lv, flds) ->
      fprintf ppf "%a := READ(%a%a)\t/* <%a> */" 
        pp_temp_id tid
        pp_il_lvalue lv
        pp_fields flds
        pp_ctt_type ct
  | IL0stmtWrite (lv, flds, t1) ->
      fprintf ppf "WRITE(%a%a) <- %a"
	pp_il_lvalue lv
        pp_fields flds
        pp_temp_id t1
  | IL0stmtSequence l
      -> fprintf ppf "SEQUENCE:@\n    @[<v>";
        List.iter (fun i -> fprintf ppf "%a@;" pp_il0 i) l;
        fprintf ppf "@]"
  | IL0stmtParallel l
      -> fprintf ppf "PARALLEL:@\n    @[<v>";
        List.iter (fun i -> fprintf ppf "%a@;" pp_il0 i) l;
        fprintf ppf "@]"
and pp_il0 ppf i = pp_il0_desc ppf (locval i).il0_t

let pp_il0_function ppf f =
  fprintf ppf "FUNCTION:@\n@[<v>    %a@]" pp_il0 f.il0_funcbody

let pp_il0_global_declaration_desc pp_init ppf =
  function
      IL0declFunction(gs, ct, id, argids, b) ->
	fprintf ppf "%a %a %s(%a): "
	  Ctt_formatter.pp_global_storage_class gs
	  pp_ctt_type ct
	  id
	  (pp_list pp_print_string ", ") argids;
	pp_il0_function ppf b;
	fprintf ppf "@."
  | IL0declVariable(gs, ct, id, initopt) ->
      fprintf ppf "%a %a %s"
	Ctt_formatter.pp_global_storage_class gs
	pp_ctt_type ct id;
      match initopt with
	None -> fprintf ppf " : DECL@."
      | Some i -> fprintf ppf " = %a@." pp_init i

let pp_il0_global_declaration pp_init ppf decl =
  pp_il0_global_declaration_desc pp_init ppf (Locterm.locval decl)

let pp_il0_program pp_init ppf p =
  List.iter
    (pp_il0_global_declaration pp_init ppf) p

