(* Part of Fail-Safe C Compiler.
   Written on 2005 by Yutaka OIWA.
   (c) 2005 AIST. *)

(*

1. 関数単位のエスケープ解析:

   解析結果として、各変数に次の情報を割り当てる。

   Temporary: この変数のアドレスは計算されない。
   Local: この変数のアドレスは外部に漏洩しない。
   Escape: この変数のアドレスは外部に漏洩する。
   Escape_If [(f1, v1); (f2, v2); ...]: 
     関数 f1 の第 v1 引数、関数 f2 の第 v2 引数、... の
     いずれかがアドレスを漏らす時、
     この変数のアドレスは外部に漏洩する。
   Global: この変数はグローバル変数である。

   また、各引数にはそれとは別に次の情報を割り当てる。

   Local: この引数に渡されたアドレスは外部に漏洩しない。
   Escape []: この引数に渡されたアドレスは外部に漏洩する。
   Escape [(f1, v1); (f2, v2); ...]: 
     関数 f1 の第 v1 引数、関数 f2 の第 v2 引数、... の
     いずれかがアドレスを漏らす時、
     この引数に渡されたアドレスは外部に漏洩する。

   ある引数のアドレスが漏洩するということは、
   当然にその引数の値のアドレスが漏洩するということである。
   ある変数のアドレスを持つポインタ変数の値が漏洩するのも
   当然にある変数のアドレスが漏洩するということである。

   というわけで、その手の関係を推移的に追跡するのが
   このモジュールの仕事。

2. この結果を突き合わせると、プログラム全体での
   漏洩解析も可能。なお、単純分割コンパイルの際は、
   外部の関数の漏洩性に依存した結果は、
   プロトタイプ宣言に置ける拡張アトリビュートで
   明示されていない限り漏洩すると見做す。

3. この結果を元に、ローカル変数を

   1) ヒープに割り当てる
   2) スタックに割り当てる
   3) 仮想レジスタに割り当てる

   のいずれにするかを決める。IL1 → IL の変換。

*)

let dprintf l t = Debug.dprintf ~category:50 l t

open util

type target = 
    T_LVAR of string (* named local variable *)
  | T_ARGTARGET of int * int 
    (* T_ARGTARGET(n,m) shows mth arguments and all subsidiary of argument #n *)

let source = 
    S_LVAR of string
  | S_TEMP of int
  | S_EXTERNAL

type target_set = analysis_target Set_list.set

type environment = 
  {
   lvar_points_to : (string, target_set) Hashtbl.t;
   lvar_pointed_by : (string, source_set) Hashtbl.t;
   tempvar_points_to : target_set Earray;
   extfunc_receives : ((string * int), target_set) Hashtbl.t;
   mutable extvar_holds : target_set;
 }

let create_lvar_list f = 
  let g = Glist.empty () in
  let rec iter i = match i.il0_t with
      IL0stmtDeclAutoScalar(_,_,v,_) 
    | IL0stmtDeclBulk(_,_,v,_) -> Glist.add g v;
    | IL0stmtSequence l | IL0stmtParallel l -> List.iter iter l
    | _
  in
  iter f.il0_function_body

let var_is_local ~env var = 
  Hashtbl.mem env.lvar_points_to var

let get_lvar ~env var = 
  Hashtbl.find env.lvar_points_to var
    
let add_to_lvar ~env var set = 
  assert (var_is_local ~env var);
  Hashtbl.replace env.lvar_points_to
    (Set_list.union (get_lvar ~env var) set)

let get_temp ~env tid = 
  Earray.get env.tempvar_points_to tid

let add_to_temp ~env tid set = 
  Earray.set env.tempvar_points_to tid
    (Set_list.union (get_temp ~env var) set)

let add_to_unknown ~env set = 
  env.extvar_holds := (Set_list.add env.extvar_holds set)

(*      関数にポインタを渡した時に、遷移的に渡さないといけないんだけど……。 *)

let analyse_function f = 
  let variables_pointsto = Hashtbl.create () in
  let funcargs_pointsto = Hashtbl.create ()  in
  let someone_points_to = ref (Set_list.empty ()) in
  let tempvars_pointsto = Earray.empty_with_default (Set_list.empty ()) in

  let local_var_list = create_lvar_list f in

  let add_to_tempvar v s = 
    Earray.set tempvars_pointsto v 
      (Set_list.union (Earray.get tempvars_pointsto v) s) in
  let get_tempvar v = 
    Earray.get tempvars_pointsto v in
  let add_to_extfuncarg v p s = 
    let o = try Hashtbl.find funcargs_pointsto (v,p)
            with Not_found -> Set_list.empty ()
    in
    Hashtbl.replace funcargs_pointsto (v,p) (Set_list.union o s)
  in
  let get_var v = 
    try Hashtbl.find variables_pointsto v
    with Not_found -> Set_list.empty ()
  in
  let add_to_var v s = 
    Hashtbl.replace variables_pointsto v (Set_list.union (get_var v) s)
  in
  let get = function
      ARGTARGET i -> Set_list.singleton [ARGTARGET i] (* TODO *)
    | LOCALVAR v -> get_var v
    | GLOBALVAR -> !someone_points_to
  in
  let add t s = match t with
    ARGTARGET i -> KKK add_to_unknown s
  | GLOBALVAR -> add_to_unknown s
  | 

  let rec process_instr = function
      ILstmtAssign(target, ctyp, exp) -> begin
	match exp with
	  ILexpCoerce(_, tid) | ILexpIdent tid 
	| ILexpBinop((ILbinPlusPV | ILbinMinusPV), tid, _)
	  ->
	    add_to_tempvar target (get_tempvar tid)
	| ILexpBinop
	    ((ILbinTimes | ILbinDiv
                | ILbinPlusVV | ILbinMinusVV 
		| ILbinMinusPP | ILbinMinusPV
		| ILbinModulo | ILbinLshift
		| ILbinRshift | ILbinLogAnd | ILbinLogOr
		| ILbinIntAnd | ILbinIntOr | ILbinIntXor
		| ILbinLessThan | ILbinLessEqual
		| ILbinGtrThan | ILbinGtrEqual
		| ILbinEqual | ILbinNotEqual), _, _) -> ()
	| ILexpUnaryop _ | ILexpUndefined -> ()
	| ILexpInvoke(ILlvVar (id,_),args) ->
	    Util.list_iteri
	      (fun i v ->
		let v = get_tempvar v in
		add_to_extfuncarg id i v;
		add_to_tempvar target v;
		(* TODO: here assumes argments may be returned as retval *)
	      ) args
	| ILexpInvoke(ILlvPtr _, args) ->
	    List.iter
	      (fun v -> add_to_unknown (get_tempvar v)) args
	      (* TODO: here assumes function pointers may leak any arguments *)
	| ILexpInvoke(ILlvTemp _,_) -> failwith "unimp99"
	| ILexpAddress(ILlvVar (id,_),_) ->
	    if is_local_variable id then
	      add_to_tempvar target (Set_list.singleton LOCALVAR id))
	    else
	      add_to_tempvar target (Set_list singleton GLOBALVAR)
	| ILexpAddress(ILlvPtr tid,_) ->
	    add_to_tempvar target (get_tempvar vid)
	| ILexpAddress(ILlvTemp tid,_) ->
	    failwith "unimp111"
	| ILexpArgument i ->
	    add_to_tempvar target [ARGTARGET i]
      end
    | ILstmtRead(target,ct,ILlvVar(id,_),flds) ->
	if is_local_variable id then
	  add_to_tempvar target (get_var id)
    | ILstmtRead(target,ct,ILlvPtr(tid,_),flds) ->
	Set_list.iter
	  (fun p ->
	    add_to_tempvar target (get p)) (get_tempvar tid)
    | ILstmtRead(target,ct,ILlvTemp _,flds) -> failwith "unimp122"
    | ILstmtWrite(ILlvVar(id,_),flds,tid) ->
	if is_local_variable id then
	  add_to_var id (get_tempvar tid)
	else
	  add_to_unknown id (get_tempvar tid)
    | ILstmtWrite(ILlvPtr(target,_),flds,tid) ->
	Set_list.iter
	  (fun p ->
	    add p (get_tempvar tid)) (get_tempvar target)
    | ILstmtSequence l | ILstmtParallel l ->
	List.iter process_instr l)
  in
  ()
  
