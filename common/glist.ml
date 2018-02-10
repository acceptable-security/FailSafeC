(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)

type 'a mut_list = {
    mutable hd: 'a; 
    mutable tl: 'a list
  }
external inj : 'a mut_list -> 'a list = "%identity"
let dummy_node () = { hd = Obj.magic (); tl = [] }

type 'a t = {
    mutable hd_cell : 'a mut_list;
    mutable tl_cell : 'a mut_list;
    mutable alive : bool;
  }

type 'a backpatch_descriptor = {
    mutable list : 'a t;
    mutable cell : 'a mut_list;
  }

let empty () =
  let d = dummy_node () in
  { hd_cell = d; tl_cell = d; alive = true }

(* check will be omited if -noassert *)
let check_alive g = 
  if not g.alive then
    failwith "Glist.put: same glist can be used only once"
  else
    true

let reset_internal g = 
  g.hd_cell.tl <- [];
  g.tl_cell <- g.hd_cell

let reset g = 
  reset_internal g;
  g.alive <- true

let put g x = 
  assert (check_alive g);
  let r = { hd = x; tl = [] } in
  g.tl_cell.tl <- inj r;
  g.tl_cell <- r

let put_backpatchable g x = 
  assert (check_alive g);
  let r = { hd = x; tl = [] } in
  g.tl_cell.tl <- inj r;
  g.tl_cell <- r;
  { list = g; cell = r }

let backpatch r v = 
  assert (check_alive r.list);
  r.cell.hd <- v

let append g l = 
  assert (check_alive g);
  let rec iter dst = function
      [] -> dst
    | hd::tl ->
	let r = { hd = hd; tl = [] } in
	dst.tl <- inj r;
	iter r tl
  in
  g.tl_cell <- iter g.tl_cell l

let to_list g = 
  assert (check_alive g);
  g.alive <- false;
  let l = g.hd_cell.tl in
  reset_internal g;
  l

let of_list l = 
  let g = empty () in
  append g l

let is_empty g = 
  g.hd_cell.tl = []

module Infix = struct
  let ( <::= ) = put
  let ( <@= ) = append
end
