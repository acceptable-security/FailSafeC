(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2003. *)

(* Implementation of type-safe, polymorphic property list *)

type data
type tag_internal = Obj.t
type 'a tag = tag_internal

type t = (tag_internal * data) list ref

let put (tag : 'a tag) (v : 'a) (p : t) =
  let v' = (Obj.magic v : data) in
  p := ((tag : tag_internal), v'):: !p
  
let get (tag : 'a tag) (p : t) = 
  let tag' = (tag : tag_internal) in
  let rec iter = function
      [] -> raise Not_found
    | (t,v) :: tl -> if t == tag' then v else iter tl
  in
  (Obj.magic (iter !p) : 'a)

let mem (tag : 'a tag) (p : t) =
  let tag' = (tag : tag_internal) in
  List.mem_assq tag !p

let make_new_tag () = 
  let tag = Obj.new_block Obj.abstract_tag 1 in
  (tag : 'a tag)

let empty () = (ref [] : t)
