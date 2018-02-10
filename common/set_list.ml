(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2002-2004. *)

(* set implemented by simple list *)

type 'a set = 'a list

(* constructors/deconstructors *)

let rec of_list l = 
  List.sort compare l

let empty = []

let singleton e = [e]

let rec to_list l = l

(* basic operations *)

let rec intersection l1 l2 = 
  match l1, l2 with
    [], _ -> []
  | _, [] -> []
  | (hd1::tl1), (hd2::tl2) -> 
      if hd1 = hd2 then
	hd1 :: intersection tl1 tl2
      else if hd1 < hd2 then
	intersection tl1 l2
      else
	intersection l1 tl2
	  
let rec subtract l1 l2 = 
  match l1, l2 with
    [], _ -> []
  | _, [] -> l1
  | (hd1::tl1), (hd2::tl2) -> 
      if hd1 = hd2 then
	subtract tl1 tl2
      else if hd1 < hd2 then
	hd1 :: (subtract tl1 l2)
      else
	subtract l1 tl2
	  
let rec has_intersection l1 l2 = 
  match l1, l2 with
    [], _ -> false
  | _, [] -> false
  | (hd1::tl1), (hd2::tl2) -> 
      if hd1 = hd2 then
	true
      else if hd1 < hd2 then
	has_intersection tl1 l2
      else
	has_intersection l1 tl2

let rec union l1 l2 = 
  match l1, l2 with
    [], l2 -> l2
  | l1, [] -> l1
  | (hd1::tl1), (hd2::tl2) ->
      if hd1 = hd2
      then hd1::(union tl1 tl2)
      else if hd1 < hd2 
      then hd1::(union tl1 l2)
      else hd2::(union l1 tl2)

let add l1 e = 
  union l1 [e]

(* higher-order operations *)

let mem = List.mem

let iter = List.iter
