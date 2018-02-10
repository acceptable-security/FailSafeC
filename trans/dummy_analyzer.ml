(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2003-2007. *)

open Il
open Ils
open Big_int

let do_ilstype = function
    ILStypeVal t -> ILRtypeVal(t, Value_any)
  | ILStypeBase t -> ILRtypeBase(t, { ilr_base_null = Maybe; 
				      ilr_base_cast = Maybe;
				      ilr_base_size = Base_lowerbound zero_big_int })
  | ILStypeBaseTemp t -> ILRtypeBaseTemp(t, { ilr_base_null = Maybe; 
					      ilr_base_cast = Maybe;
					      ilr_base_size = Base_lowerbound zero_big_int })
  | ILStypeOfs t -> ILRtypeOfs(t, Value_any)

let do_block { location = location;
	       predecessor = predecessor;
	       successor = successor;
	       immediate_dominator = immediate_dominator;
	       nest_level = nest_level;
	       phi_function = phi_function;
	       code = code } = 
  { location = location;
    predecessor = predecessor;
    successor = successor;
    immediate_dominator = immediate_dominator;
    nest_level = nest_level;
    phi_function = phi_function;
    code = (code : ils list) }

let do_body = Array.map do_block

let do_attr { original_name = original_name;
	      variable_type = variable_type;
	      storage_class = storage_class } = 
  { original_name = original_name;
    variable_type = do_ilstype variable_type;
    storage_class = storage_class }

let do_env = 
  Array.map do_attr

(*let do_function { body = body; max_variable_number = max_variable_number;
		  variable_environment = variable_environment; arguments = arguments } = 
  { body = do_body body;
    max_variable_number = max_variable_number;
    variable_environment = do_env variable_environment;
    arguments = arguments }*)

let do_function f = 
  { f with
    body = do_body f.body;
    variable_environment = do_env f.variable_environment },
  Util.Earray.empty_with_default ~zero:Bunanalyzed (* replaced by Ils_basetype_analysis *)

let f ~genv = 
  Locterm.locmap_list
    (function
	ILdeclVariable (gsc,t,id,init) -> ILRdeclVariable(gsc,t,id,init)
      | ILdeclFunction (gsc,t,id,arg_id,func) ->
	  ILRdeclFunction (gsc,t,id,arg_id,do_function func))
