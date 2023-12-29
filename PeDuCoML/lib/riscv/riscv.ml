(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Typing
open Peducoml_runtime
open Peducoml_stdlib

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

let get_load_instruction dest src = function
  | Imm -> asprintf "    li %s,%s" dest src
  | Register -> asprintf "    mv %s,%s" dest src
  | Binding ->
    asprintf "    lui %s, %%hi(%s)\n" dest src
    ^ asprintf "    addi %s, %s, %%lo(%s)" dest dest src
  | Offset -> asprintf "    ld %s,%s" dest src
;;

(* Why 5? *)
(* Should we declare these functions here??? *)
let global_functions : (string, rv_value * int) Hashtbl.t =
  Hashtbl.create (Base.List.length runtime + Base.List.length stdlib)
;;

let _ =
  let rec count_args current = function
    | TArr (_, out_ty) -> count_args (current + 1) out_ty
    | _ -> current
  in
  Base.List.iter (runtime @ stdlib) ~f:(fun (id, fun_type) ->
    let args_count = snd fun_type |> count_args 0 in
    Hashtbl.add global_functions id (binding id, args_count))
;;

let ( >>= ) = Option.bind

(** [lookup_function name] returns [Some f] if a function with name [name] exists, and [None] otherwise. *)
let lookup_function name = Hashtbl.find_opt global_functions name >>= fun (f, _) -> Some f

(** [lookup_function_exn name] returns [f] if a function with name [name] exists, and raises [Not_found] otherwise. *)
let lookup_function_exn name = Hashtbl.find global_functions name |> fst

let free_registers : string Stack.t = Stack.create ()
let _ = Stack.push "t7" free_registers
let _ = Stack.push "t6" free_registers
let _ = Stack.push "t5" free_registers
let _ = Stack.push "t4" free_registers
let _ = Stack.push "t3" free_registers
let _ = Stack.push "t2" free_registers
let _ = Stack.push "t1" free_registers
let _ = Stack.push "t0" free_registers
let current_stack_offset = ref 0
let current_s0_offset = ref 0
let current_condition_number = ref 0

(** [build_load value] creates an [<load_instruction> dest,value] instruction and returns [dest]. *)
let build_load value =
  match value.typ with
  | Register -> value
  | _ ->
    let reg = Stack.pop free_registers in
    printf "%s\n" (get_load_instruction reg value.value value.typ);
    register reg
;;

let build_store value =
  let stack_location = string_of_int !current_s0_offset ^ "(s0)" in
  current_s0_offset := !current_s0_offset - 8;
  printf "    sd %s,%s\n" value.value stack_location;
  offset stack_location
;;

(** [const_int v] returns an integer constant with value [v]. *)
let const_int num = asprintf "%d" num |> imm

(** [declare_function name arg_list] returns a new function with name [name] and arguments [arg_list] and locations where its arguments are stored.

    For example, [declare_function "main" ["arg1", "arg2"]] will return a new function with name ["main"] and a list [[("arg1", "a0"), ("arg2", "a1")]],
    meaning ["arg1"] will be stored in register ["a0"] and ["arg2"] - in register ["a1"]. *)
let declare_function name arg_list local_variables_number =
  let stack_offset = (local_variables_number * 8) + 16 in
  printf
    "    .globl %s\n\
    \    .type %s, @function\n\
     %s:\n\
    \    addi sp,sp,-%d\n\
    \    sd ra,%d(sp)\n\
    \    sd s0,%d(sp)\n\
    \    addi s0,sp,%d\n"
    name
    name
    name
    stack_offset
    (stack_offset - 8)
    (stack_offset - 16)
    stack_offset;
  current_stack_offset := stack_offset;
  current_s0_offset := -24;
  let arg_list =
    Base.List.foldi arg_list ~init:[] ~f:(fun ind acc arg ->
      printf "    sd a%d,%d(s0)\n" ind !current_s0_offset;
      let storage_location = string_of_int !current_s0_offset ^ "(s0)" |> offset in
      current_s0_offset := !current_s0_offset - 8;
      (arg, storage_location) :: acc)
  in
  let binding = binding name in
  Hashtbl.add global_functions name (binding, Base.List.length arg_list);
  binding, arg_list
;;

(** [build_call callee arg_list] stores arguments from [arg_list] and creates a [call callee] instruction. *)
let build_call callee arg_list =
  Base.List.iteri arg_list ~f:(fun ind arg ->
    let dest = "a" ^ Int.to_string ind in
    printf "%s\n" (get_load_instruction dest arg.value arg.typ));
  printf "    call %s\n" callee.value;
  build_store (register "a0")
;;

let build_compute_binop instruction left_operand right_operand =
  let left_operand = build_load left_operand in
  let right_operand = build_load right_operand in
  printf
    "    %s %s,%s,%s\n"
    instruction
    left_operand.value
    left_operand.value
    right_operand.value;
  Stack.push left_operand.value free_registers;
  Stack.push right_operand.value free_registers;
  build_store left_operand
;;

let build_compare_binop instruction left_operand right_operand =
  let left_operand = build_load left_operand in
  let right_operand = build_load right_operand in
  printf
    "    %s %s,%s,true_branch%d\n"
    instruction
    left_operand.value
    right_operand.value
    !current_condition_number;
  printf "    li %s,0\n" left_operand.value;
  printf "    beq zero,zero,after_true_branch%d\n" !current_condition_number;
  printf "    true_branch%d:\n" !current_condition_number;
  printf "    li %s,1\n" left_operand.value;
  printf "    after_true_branch%d:\n" !current_condition_number;
  current_condition_number := !current_condition_number + 1;
  Stack.push left_operand.value free_registers;
  Stack.push right_operand.value free_registers;
  build_store left_operand
;;

let build_add = build_compute_binop "add"
let build_sub = build_compute_binop "sub"
let build_mul = build_compute_binop "mul"
let build_div = build_compute_binop "div"
let build_and = build_compute_binop "and"
let build_or = build_compute_binop "or"
let build_eq = build_compare_binop "beq"
let build_neq = build_compare_binop "bne"
let build_lt = build_compare_binop "blt"
let build_gte = build_compare_binop "bge"
let build_lte left_operand right_operand = build_gte right_operand left_operand
let build_gt left_operand right_operand = build_lt right_operand left_operand

(** [build_ret v] creates a [ret] instruction. *)
let build_ret value =
  if value.value <> "a0"
  then printf "%s\n" (get_load_instruction "a0" value.value value.typ);
  printf
    "    ld ra,%d(sp)\n    ld s0,%d(sp)\n    addi sp,sp,%d\n    ret\n"
    (!current_stack_offset - 8)
    (!current_stack_offset - 16)
    !current_stack_offset
;;

(** [params f] return [Some n], where [n] is the number of arguments of function [f] if there is such function, and [None] otherwise. *)
let params f =
  match Hashtbl.find_opt global_functions f.value with
  | Some (_, arg_number) -> Some arg_number
  | None -> None
;;

let build_neg value =
  let value = build_load value in
  printf "    neg %s,%s\n" value.value value.value;
  build_store value
;;

let build_not value =
  let value = build_load value in
  printf "    xori %s,%s,-1\n" value.value value.value;
  build_store value
;;
