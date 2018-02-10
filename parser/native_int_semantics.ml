(* Part of Fail-Safe C Compiler. (c) 2002-2004 Yutaka Oiwa. *)

open Fsc_config
open Big_int_infix

let quomod_big_int_normal x y = 
  let xp = abs_big_int x in
  let yp = abs_big_int y in
  let q, m = Big_int.quomod_big_int xp yp in
  match sign_big_int x, sign_big_int y with
    -1, -1 ->
      q, ~-! m
  | -1, _ ->
      ~-! q, ~-! m
  | _, -1 ->
      ~-! q, m
  | _, _ ->
      q, m

let quomod_big_int_divisorsign x y = 
  if sign_big_int y = -1 then
    let q, m = Big_int.quomod_big_int (~-! x) (~-! y) in
    q, ~-! m
  else
    Big_int.quomod_big_int x y

let quomod_big_int_modpos = Big_int.quomod_big_int

let quomod_big_int = 
  match Fsc_config.quomod_style with
    Div_round_to_zero ->
      quomod_big_int_normal
  | Mod_match_divisor ->
      quomod_big_int_divisorsign
  | Mod_always_positive ->
      quomod_big_int_modpos

let div_big_int x y = fst (quomod_big_int x y)
let mod_big_int x y = snd (quomod_big_int x y)
let ( /! ) = div_big_int

let test x y = 
  let test f = 
    let q, m = f (big_int_of_int x) (big_int_of_int y) in 
    let q, m = int_of_big_int q, int_of_big_int m in
    let check = x = q * y + m in
    q, m, (if check then 'o' else 'X')
  in
  let q0, m0, c0 = test quomod_big_int in
  let q1, m1, c1 = test quomod_big_int_normal in
  let q2, m2, c2 = test quomod_big_int_divisorsign in
  let q3, m3, c3 = test quomod_big_int_modpos in
  Printf.printf "%3d,%3d%c (%3d,%3d)%c (%3d,%3d)%c (%3d,%3d)%c"
    q0 m0 c0 q1 m1 c1 q2 m2 c2 q3 m3 c3;
  print_newline ()
  
