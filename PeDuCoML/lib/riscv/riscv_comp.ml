(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast
open Arg_counter
open Result
open Riscv

let ( >>= ) = bind
let ( >>| ) m f = m >>= fun x -> ok (f x)

type riscv_error = UnboundVariable of unique_id

let unbound id = UnboundVariable id

module Environment = struct
  let find = Base.Map.Poly.find
  let empty = Base.Map.Poly.empty
  let set = Base.Map.Poly.set
end

let ( let* ) = bind

open Environment

let string_of_unique_id = function
  | AnfId id -> string_of_int id
  | GlobalScopeId id -> id
;;

let build_binary_operation = function
  | Add -> build_add
  | Sub -> build_sub
  | Mul -> build_mul
  | Div -> build_div
  | AND -> build_and
  | OR -> build_or
  | Eq -> build_eq
  | NEq -> build_neq
  | LT -> build_lt
  | GTE -> build_gte
  | LTE -> build_lte
  | GT -> build_gt
;;

let codegen_immexpr args_number env = function
  | ImmInt num -> ok (const_int num, 0)
  | ImmChar c -> ok (Base.Char.to_int c |> const_int, 0)
  | ImmBool v -> ok (Base.Bool.to_int v |> const_int, 0)
  | ImmId (GlobalScopeId id) -> ok (lookup_function_exn id, 0)
  | ImmId (AnfId id) ->
    (match find env (AnfId id) with
     | None -> AnfId id |> unbound |> error
     | Some value ->
       let number_of_args =
         match find args_number (AnfId id) with
         | Some n -> n
         | None -> 0
       in
       ok (build_load value, number_of_args))
  | _ -> failwith "TODO"
;;

let codegen_cexpr args_number env = function
  | CImm immexpr -> codegen_immexpr args_number env immexpr >>| fst
  | CBinaryOperation (bop, left_imm, right_imm) ->
    let* left, _ = codegen_immexpr args_number env left_imm in
    let* right, _ = codegen_immexpr args_number env right_imm in
    (match bop with
     | Div -> ok @@ build_call (lookup_function_exn "peducoml_divide") [ left; right ]
     | _ ->
       let result = build_binary_operation bop left right in
       ok result)
  | CApplication (callee, arg) ->
    let* callee, number_of_args = codegen_immexpr args_number env callee in
    let number_of_args =
      match params callee with
      | Some num -> num
      | None -> number_of_args
    in
    let func_ptr =
      build_call
        (lookup_function_exn "peducoml_alloc_closure")
        [ callee; const_int number_of_args ]
    in
    let* arg, _ = codegen_immexpr args_number env arg in
    ok @@ build_call (lookup_function_exn "peducoml_apply") [ func_ptr; arg ]
  | _ -> failwith "TODO"
;;

let rec codegen_aexpr args_number env = function
  | ACExpr cexpr -> codegen_cexpr args_number env cexpr
  | ALet (id, cexpr, aexpr) ->
    let* cexpr_rv_value = codegen_cexpr args_number env cexpr in
    let env = set env ~key:id ~data:cexpr_rv_value in
    codegen_aexpr args_number env aexpr
;;

let codegen_global_scope_function args_numbers env (func : global_scope_function) =
  let function_name, arg_list, body = func in
  let rec count_local_variables acc = function
    (* TODO: не учитывает if-expressions *)
    | ALet (_, body, nested_aexpr) ->
      let acc = acc + 1 in
      (* local var *)
      let acc =
        acc
        +
        match body with
        | CApplication _ | CBinaryOperation _ -> 1
        | _ -> 0
      in
      count_local_variables (acc + 1) nested_aexpr
    | _ -> acc
  in
  let func_rv_value, args =
    declare_function
      function_name
      arg_list
      (count_local_variables 0 body + Base.List.length arg_list)
  in
  let env =
    Base.List.fold_right args ~init:env ~f:(fun (arg, location) acc ->
      set acc ~key:arg ~data:location)
  in
  let* body = codegen_aexpr (Base.Map.find_exn args_numbers function_name) env body in
  build_ret body;
  ok func_rv_value
;;

let codegen program : (unit, riscv_error) Result.t =
  (* Format.printf "    .option pic\n"; *)
  let args_number = gather_args_numbers program in
  let rec codegen env = function
    | [] -> ok ()
    | head :: tail ->
      let* _ = codegen_global_scope_function args_number env head in
      codegen env tail
  in
  codegen empty program
;;
