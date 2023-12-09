(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type rv_type =
  | Imm
  | Register
  | Offset
  | Binding

type rv_value =
  { value : string
  ; typ : rv_type
  }

let imm s = { value = s; typ = Imm }
let register s = { value = s; typ = Register }
let offset s = { value = s; typ = Offset }
let binding s = { value = s; typ = Binding }

let get_load_instruction = function
  | Imm -> "li"
  | Register -> "mv"
  | _ -> "ld"
;;

let global_functions : (string, rv_value) Hashtbl.t = Hashtbl.create 5

let _ =
  Hashtbl.add global_functions "peducoml_alloc_closure" (binding "peducoml_alloc_closure")
;;

let _ = Hashtbl.add global_functions "peducoml_apply" (binding "peducoml_apply")
let _ = Hashtbl.add global_functions "print_int" (binding "print_int")

(** [const_int v] returns an integer constant with value [v]. *)
let const_int num = asprintf "%d" num |> imm

(** [declare_function name arg_list] returns a new function with name [name] and arguments [arg_list] and locations where its arguments are stored.

    For example, [declare_function "main" ["arg1", "arg2"]] will return a new function with name ["main"] and a list [[("arg1", "a0"), ("arg2", "a1")]],
    meaning ["arg1"] will be stored in register ["a0"] and ["arg2"] - in register ["a1"]. *)
let declare_function name arg_list =
  (* TODO: stack pointer decrement depends on the number of lets inside *)
  printf
    "    .globl %s\n\
    \    .type %s, @function\n\
     %s:\n\
    \    addi sp,sp,-16\n\
    \    sd ra,8(sp)\n\
    \    sd s0,0(sp)\n"
    name
    name
    name;
  let arg_list =
    Base.List.foldi arg_list ~init:[] ~f:(fun ind acc arg ->
      let storage_location =
        if ind < 8
        then
          "a" ^ string_of_int ind |> register
          (* (ind - 8) * 8 + 16 = (ind - 8) * 8 + 2 * 8 = (ind - 6) * 8 *)
        else string_of_int ((ind - 6) * 8) ^ "(sp)" |> offset
      in
      (arg, storage_location) :: acc)
  in
  binding name, arg_list
;;

(** [lookup_function name] returns [Some f] if a function with name [name] exists, and [None] otherwise. *)
let lookup_function name = Hashtbl.find_opt global_functions name

(** [lookup_function_exn name] returns [f] if a function with name [name] exists, and raises [Not_found] otherwise. *)
let lookup_function_exn name = Hashtbl.find global_functions name

(** [build_call callee arg_list] stores arguments from [arg_list] and creates a [call callee] instruction. *)
let build_call callee arg_list =
  Base.List.iteri arg_list ~f:(fun ind arg ->
    if ind < 8
    then printf "    %s a%d,%s\n" (get_load_instruction arg.typ) ind arg.value
    else printf "    sd %s,%d(sp)\n" arg.value ((ind - 6) * 8));
  printf "    call %s\n" callee.value;
  register "a0"
;;

let build_ret value =
  printf
    "    %s a0,%s\n    ld ra,8(sp)\n    ld s0,0(sp)\n    addi sp,sp,16\n    ret\n"
    (get_load_instruction value.typ)
    value.value
;;

(** [build_load value] creates an [ld dest,value] instruction and returns [dest]. *)
let build_load value =
  (* TODO: need to figure out how to choose a register where to load *)
  printf "    %s t0,%s\n" (get_load_instruction value.typ) value.value;
  register "t0"
;;

let build_store value =
  printf "    sd %s,%d(sp)\n" value 16;
  offset "16(sp)"
;;
