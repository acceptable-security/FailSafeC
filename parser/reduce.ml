(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)

open Big_int_infix
open Ctt_abstree

exception NotConstant

let zero_const_value v = 
  match v with
    CTTconstInteger i -> i ==! zero_big_int
  | CTTconstFloat f -> f = 0.0
  | CTTconstString s -> false
  | CTTconstTypeInfo _ -> false
  | CTTconstAbstract _ -> raise NotConstant
  | CTTconstNull -> true

let const_of_bool = function
    true -> CTTconstInteger unit_big_int
  | false -> CTTconstInteger zero_big_int

let bool_of_const x = not (zero_const_value x)

let apply_numeric intop flop v1 v2 = 
  match v1, v2 with
    CTTconstInteger i1, CTTconstInteger i2 -> CTTconstInteger (intop i1 i2)
  | CTTconstInteger i1, CTTconstFloat f2 -> CTTconstFloat (flop (float_of_big_int i1) f2)
  | CTTconstFloat f1, CTTconstInteger i2 -> CTTconstFloat (flop f1 (float_of_big_int i2))
  | CTTconstFloat f1, CTTconstFloat f2 -> CTTconstFloat (flop f1 f2)
  | _ -> raise NotConstant

let apply_integer intop v1 v2 =
  match v1, v2 with
    CTTconstInteger i1, CTTconstInteger i2 -> CTTconstInteger (intop i1 i2)
  | _ -> raise NotConstant

let apply_boolean bop v1 v2 = 
  const_of_bool (bop (bool_of_const v1) (bool_of_const v2))

let apply_compare intop flop v1 v2 = 
  const_of_bool
    (match v1, v2 with
      CTTconstInteger i1, CTTconstInteger i2 -> intop i1 i2
    | CTTconstInteger i1, CTTconstFloat f2 -> flop (float_of_big_int i1) f2
    | CTTconstFloat f1, CTTconstInteger i2 -> flop f1 (float_of_big_int i2)
    | CTTconstFloat f1, CTTconstFloat f2 -> flop f1 f2
    | _ -> raise NotConstant)

let apply_shift intop v1 v2 = 
  match v1, v2 with
    CTTconstInteger i1, CTTconstInteger i2 -> 
      if is_int_big_int i2 then
	let i2 = int_of_big_int i2 in
	if i2 >= 0 && i2 < Fsc_config.sizeof_longlong * Fsc_config.bits_of_byte then
	  CTTconstInteger (intop i1 i2)
	else
	  raise NotConstant
      else
	raise NotConstant
  | _ -> raise NotConstant

let const_value e1 = 
  match (Locterm.locval e1).expr_t with
    CTTexpConstant c -> c
  | _ -> raise NotConstant

let round_to_type v1 t = 
  match t.ct_ty with
    Tbuiltin (Tfloat | Tdouble | Tlongdouble) ->
      let v = 
	match v1 with
	  CTTconstNull -> 0.0
	| CTTconstInteger i -> float_of_big_int i
	| CTTconstFloat f -> f
	| _ -> assert false
      in
      CTTconstFloat v
  | Tbuiltin (Tchar | Tschar | Tuchar  | Tshort | Tushort |
              Tint | Tuint | Tlong | Tulong | Tlonglong | Tulonglong as bt) ->
      let bits = Fsc_config.bits_of_byte * size_of_builtin_type bt in
      let v = 
	match v1 with
	  CTTconstNull -> zero_big_int
	| CTTconstInteger i -> i
	| CTTconstFloat f -> big_int_of_float f
	| _ -> assert false
      in
      let v = 
	if is_signed_builtin_type bt then
	  round_to_signed_binary_big_int v bits
	else
	  round_to_unsigned_binary_big_int v bits
      in
      CTTconstInteger v
  | Tpointer _ ->
      if zero_const_value v1 then CTTconstNull else raise NotConstant
  | _ -> raise NotConstant

let make_constant ~orig v1 t = 
  let v1 = round_to_type v1 t in
  make_expr (CTTexpConstant v1) t ~loc:(Locterm.locget orig)

let reduce_binary_expression bop e1 e2 = 
  let v1 = const_value e1 in
  let v2 = const_value e2 in
  match bop with
    CTTbinTimes -> apply_numeric ( *! ) ( *. ) v1 v2
  | CTTbinDiv ->
      if zero_const_value v2 then raise NotConstant (* div-zero *)
      else apply_numeric ( Native_int_semantics.div_big_int ) ( /. ) v1 v2
  | CTTbinPlusVV -> apply_numeric ( +! ) ( +. ) v1 v2
  | CTTbinMinusVV -> apply_numeric ( -! ) ( -. ) v1 v2
  | CTTbinModulo -> apply_integer Native_int_semantics.mod_big_int v1 v2
  | CTTbinLshift -> apply_shift ( <<!% ) v1 v2
  | CTTbinRshift -> apply_shift ( >>!% ) v1 v2
  | CTTbinLessThan -> apply_compare ( <! ) ( < ) v1 v2
  | CTTbinLessEqual -> apply_compare ( <=! ) ( <= ) v1 v2
  | CTTbinGtrThan -> apply_compare ( >! ) ( > ) v1 v2
  | CTTbinGtrEqual -> apply_compare ( >=! ) ( >= ) v1 v2
  | CTTbinEqual -> apply_compare ( ==! ) ( = ) v1 v2
  | CTTbinNotEqual -> apply_compare ( <>! ) ( <> ) v1 v2
  | CTTbinIntXor -> apply_integer lxor_big_int v1 v2
  | CTTbinIntOr -> apply_integer lor_big_int v1 v2
  | CTTbinIntAnd -> apply_integer land_big_int v1 v2
  | CTTbinLogOr -> apply_boolean ( || ) v1 v2
  | CTTbinLogAnd -> apply_boolean ( && ) v1 v2

  | CTTbinPostPlusVV | CTTbinPostMinusVV 
  | CTTbinPlusPV | CTTbinMinusPP | CTTbinMinusPV (* added *)
  | CTTbinPostPlusPV | CTTbinPostMinusPV 
      -> raise NotConstant

let reduce_unary_expression uop e1 = 
  let v1 = const_value e1 in
  match uop, v1 with
    UnaryPlus, v1 -> v1
  | UnaryMinus, CTTconstInteger i -> CTTconstInteger (minus_big_int i)
  | UnaryMinus, CTTconstFloat i -> CTTconstFloat (~-. i)
  | UnaryMinus, _ -> raise NotConstant
  | LogNot, v1 ->
      const_of_bool (not (bool_of_const v1))
  | IntNot, CTTconstInteger i -> 
      CTTconstInteger (lnot_big_int i)
  | IntNot, _ -> raise NotConstant

let reduce_expression orig = 
  let typ = (Locterm.locval orig).expr_type in
  match (Locterm.locval orig).expr_t with
  | CTTexpComma(e1,e2) ->
      let _ = const_value e1 in (* NotConstant raised is side-effect exists *)
      let v2 = const_value e2 in
      make_constant ~orig v2 typ
  | CTTexpAssign _
  | CTTexpBinAssign _ -> raise NotConstant
  | CTTexpConditional(e1,e2,e3) ->
      let v1 = const_value e1 in
      let v2 = const_value e2 in
      let v3 = const_value e3 in
      if bool_of_const v1 then
	make_constant ~orig v2 typ
      else
	make_constant ~orig v3 typ
  | CTTexpBinExpr(bop,e1,e2) ->
      let v = reduce_binary_expression bop e1 e2 in
      make_constant ~orig v typ
  | CTTexpCoerce(cty,exp) ->
      let v = const_value exp in
      make_constant ~orig v typ (* does appropriate rounding *)
  | CTTexpUnaryExpr(uop,exp) ->
      let v = reduce_unary_expression uop exp in
      make_constant ~orig v typ
  | CTTexpAddress _
  | CTTexpPtrDeref _
  | CTTexpInvoke _
  | CTTexpField _ -> raise NotConstant
  | CTTexpConstant c ->
      make_constant ~orig c typ
  | CTTexpVar _ -> raise NotConstant
