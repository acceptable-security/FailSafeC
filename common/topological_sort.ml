(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2004. *)

(*
  type: node list -> node list

  node -> id set (depends)
  node -> id set (define)

*)

open Int_set
open ExtList

type set = Int_set.t

let rec merge_set_excepts ~depends i = 
  function
      [] -> empty
    | hd::tl ->
	if hd == i then merge_set_excepts ~depends i tl
	else union (depends.(hd)) (merge_set_excepts ~depends i tl)

let rec topological_sort_loop ~length ~depends ~defines target =
  if target = [] then [] else
  let other_depends = Array.create length empty in
  List.iter (fun i -> other_depends.(i) <- merge_set_excepts ~depends i target) target;
  let next_target, ok = List.partition (fun i -> has_intersection other_depends.(i) defines.(i)) target in
  if ok = [] then
    failwith "mutual dependency found"
  else
    ok @ (topological_sort_loop ~length ~depends ~defines next_target)
	  
let topological_sort ~(depends : 'node -> set) ~(defines : 'node -> set)
    (li : 'node list) : 'node list =
  let nodes = Array.of_list li in
  let length = Array.length nodes in
  let depends = Array.init length (fun i -> (depends (nodes.(i)))) in
  let defines = Array.init length (fun i -> (defines (nodes.(i)))) in
  let target = Array.to_list (Array.init length (fun i -> i)) in
  let l = topological_sort_loop ~length ~depends ~defines target in
  List.map (fun i -> nodes.(i)) l
    
