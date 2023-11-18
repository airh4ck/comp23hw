(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typing

let print_int_scheme = Base.Set.empty (module Base.Int), TArr (TGround Int, TGround Int)
let print_char_scheme = Base.Set.empty (module Base.Int), TArr (TGround Char, TGround Int)
let print_bool_scheme = Base.Set.empty (module Base.Int), TArr (TGround Bool, TGround Int)

(* let print_bool_scheme = Base.Set.empty (module Base.Int), TArr (TGround Bool, TGround Int) *)
let stdlib =
  [ "print_int", print_int_scheme
  ; "print_char", print_char_scheme
  ; "print_bool", print_bool_scheme
  ]
;;
