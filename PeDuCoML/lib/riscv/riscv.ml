(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(** [const_int v] returns an integer constant with value [v]. *)
let const_int num = asprintf "%d" num

(** [define_function name body] returns a new function with name [name] and body [body]. *)
let define_function name body =
  printf "%s:\n\taddi sp,sp,-16\n\tsd ra,8(sp)\n\tsd a0,%s\n\tret" name body;
  name
;;

(** [build_load value] creates an [ld dest,value] instruction and returns [dest]. *)
let build_load value =
  (* TODO: need to figure out how to choose a register where to load *)
  printf "ld t3,%s" value;
  "t3"
;;
