(* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   This file is written by Yutaka Oiwa in 2002-2004. *)

(* Helper routines for option type *)

let map f = function
    None -> None
  | Some x -> Some (f x)

let do_option = map

let iter f = function
    None -> ()
  | Some x -> (f x : unit); ()

let default d = function
    None -> d
  | Some v -> v

let get = function
    None -> failwith "Option.get"
  | Some v -> v

let to_list = function
    None -> []
  | Some v -> [v]
