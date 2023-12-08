(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(** [const_int v] returns an integer constant with value [v]. *)
let const_int num = asprintf "%d" num

(** [declare_function name body] returns a new function with name [name] and body [body]. *)
let declare_function name =
  printf "%s:\n    addi sp,sp,-32\n    sd ra,24(sp)\n    sd s0, 16(sp)\n" name;
  name
;;

let build_ret value =
  printf
    "    li a0,%s\n    ld ra,8(sp)\n    ld s0,0(sp)\n    addi sp,sp,16\n    ret"
    value
;;

(** [build_load value] creates an [ld dest,value] instruction and returns [dest]. *)
let build_load value =
  (* TODO: need to figure out how to choose a register where to load *)
  printf "ld t3,%s" value;
  "t3"
;;
