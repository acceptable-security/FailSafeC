(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. *)

(* integer set implemented by using Set, compatible to Set_list *)

(* abstract type *)

open Util

module IntSet = Set.Make (struct type t = int let compare (x:t) (y:t) = compare x y end)

open IntSet

type t = IntSet.t
type int_set = t

(* constructors/deconstructors *)

let empty = empty
let singleton = singleton
let of_list = List.fold_left (fun s x -> add x s) empty

let to_list s = 
  let g = Glist.empty () in
  iter (fun x -> Glist.put g x) s;
  Glist.to_list g

(* basic operations *)

let intersection = inter
let subtract = diff
let has_intersection a b = exists (fun x -> mem x b) a
let union = union
let add s a = add a s

let mem = mem
let iter = iter
