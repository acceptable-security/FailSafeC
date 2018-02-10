(* Part of Fail-Safe C Compiler.
   Written on 2005 by Yutaka OIWA.
   (c) 2005 AIST. *)

(*

1. �ؿ�ñ�̤Υ��������ײ���:

   ���Ϸ�̤Ȥ��ơ����ѿ��˼��ξ���������Ƥ롣

   Temporary: �����ѿ��Υ��ɥ쥹�Ϸ׻�����ʤ���
   Local: �����ѿ��Υ��ɥ쥹�ϳ�����ϳ�̤��ʤ���
   Escape: �����ѿ��Υ��ɥ쥹�ϳ�����ϳ�̤��롣
   Escape_If [(f1, v1); (f2, v2); ...]: 
     �ؿ� f1 ���� v1 �������ؿ� f2 ���� v2 ������... ��
     �����줫�����ɥ쥹��ϳ�餹����
     �����ѿ��Υ��ɥ쥹�ϳ�����ϳ�̤��롣
   Global: �����ѿ��ϥ����Х��ѿ��Ǥ��롣

   �ޤ����ư����ˤϤ���Ȥ��̤˼��ξ���������Ƥ롣

   Local: ���ΰ������Ϥ��줿���ɥ쥹�ϳ�����ϳ�̤��ʤ���
   Escape []: ���ΰ������Ϥ��줿���ɥ쥹�ϳ�����ϳ�̤��롣
   Escape [(f1, v1); (f2, v2); ...]: 
     �ؿ� f1 ���� v1 �������ؿ� f2 ���� v2 ������... ��
     �����줫�����ɥ쥹��ϳ�餹����
     ���ΰ������Ϥ��줿���ɥ쥹�ϳ�����ϳ�̤��롣

   ��������Υ��ɥ쥹��ϳ�̤���Ȥ������Ȥϡ�
   �����ˤ��ΰ������ͤΥ��ɥ쥹��ϳ�̤���Ȥ������ȤǤ��롣
   �����ѿ��Υ��ɥ쥹����ĥݥ����ѿ����ͤ�ϳ�̤���Τ�
   �����ˤ����ѿ��Υ��ɥ쥹��ϳ�̤���Ȥ������ȤǤ��롣

   �Ȥ����櫓�ǡ����μ�δط�����Ū�����פ���Τ�
   ���Υ⥸�塼��λŻ���

2. ���η�̤��ͤ���碌��ȡ��ץ�������ΤǤ�
   ϳ�̲��Ϥ��ǽ���ʤ���ñ��ʬ�䥳��ѥ���κݤϡ�
   �����δؿ���ϳ�����˰�¸������̤ϡ�
   �ץ�ȥ�����������֤����ĥ���ȥ�ӥ塼�Ȥ�
   ��������Ƥ��ʤ��¤�ϳ�̤���ȸ�������

3. ���η�̤򸵤ˡ��������ѿ���

   1) �ҡ��פ˳�����Ƥ�
   2) �����å��˳�����Ƥ�
   3) ���ۥ쥸�����˳�����Ƥ�

   �Τ�����ˤ��뤫����롣IL1 �� IL ���Ѵ���

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

(*      �ؿ��˥ݥ��󥿤��Ϥ������ˡ�����Ū���Ϥ��ʤ��Ȥ����ʤ�������ɡġġ� *)

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
  
