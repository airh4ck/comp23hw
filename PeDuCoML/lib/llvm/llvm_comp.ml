(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast
open Llvm

let context = global_context ()
let builder = builder context
let the_module = create_module context "PeDuCoML"
let i64 = i64_type context
let lookup_function_exn id llmodule = Option.get @@ lookup_function id llmodule

type llvm_error = UnboundVariable of unique_id

(* Smart constructors *)
let unbound id = UnboundVariable id
(* ------------------ *)

open Result

let ( let* ) = bind

let string_of_unique_id = function
  | AnfId id -> string_of_int id
  | GlobalScopeId id -> id
;;

let rec codegen_immexpr env = function
  | ImmInt num -> ok @@ const_int i64 num
  | ImmString str -> ok @@ const_string context str
  | ImmChar c -> ok @@ const_int i64 (Base.Char.to_int c)
  | ImmBool v -> ok @@ const_int i64 (Base.Bool.to_int v)
  | ImmId id ->
    (match Base.Map.Poly.find env id, id with
     | _, GlobalScopeId id -> ok @@ lookup_function_exn id the_module
     | None, _ -> error @@ unbound id
     | Some llvalue, AnfId id ->
       ok @@ build_load i64 llvalue (string_of_int id ^ "_n") builder)
  | ImmList imm_list ->
    let rec helper list_ptr_llv =
      let add = lookup_function_exn "peducoml_add_to_list" the_module in
      let fnty = function_type i64 [| i64; i64 |] in
      function
      | head :: tail ->
        let* elem = codegen_immexpr env head in
        helper
          (build_call fnty add [| list_ptr_llv; elem |] "peducoml_add_to_list_n" builder)
          tail
      | _ -> ok list_ptr_llv
    in
    let alloc_list = lookup_function_exn "peducoml_alloc_list" the_module in
    let fnty = function_type i64 [||] in
    let allocated_list =
      build_call fnty alloc_list [||] "peducoml_alloc_list_n" builder
    in
    helper allocated_list (Base.List.rev imm_list)
  | ImmTuple imm_list ->
    let rec helper tuple_ptr_llv =
      let fill = lookup_function_exn "peducoml_fill_tuple" the_module in
      let fnty = function_type i64 [| i64; i64 |] in
      function
      | head :: tail ->
        let* elem = codegen_immexpr env head in
        helper
          (build_call fnty fill [| tuple_ptr_llv; elem |] "peducoml_fill_tuple_n" builder)
          tail
      | _ -> ok tuple_ptr_llv
    in
    let alloc_tuple = lookup_function_exn "peducoml_alloc_tuple" the_module in
    let fnty = function_type i64 [| i64 |] in
    let allocated_tuple =
      build_call
        fnty
        alloc_tuple
        [| Base.List.length imm_list |> const_int i64 |]
        "peducoml_alloc_tuple_n"
        builder
    in
    helper allocated_tuple imm_list
;;

let build_binary_operation = function
  | Add -> build_add
  | Sub -> build_sub
  | Mul -> build_mul
  | Div -> build_udiv
  | Eq -> build_icmp Icmp.Eq
  | NEq -> build_icmp Icmp.Ne
  | GT -> build_icmp Icmp.Sgt
  | GTE -> build_icmp Icmp.Sge
  | LT -> build_icmp Icmp.Slt
  | LTE -> build_icmp Icmp.Sle
  | AND -> build_and
  | OR -> build_or
;;

let codegen_cexpr env = function
  | CImm imm_expr -> codegen_immexpr env imm_expr
  | CBinaryOperation (bop, left, right) ->
    let* left = codegen_immexpr env left in
    let* right = codegen_immexpr env right in
    let result =
      build_binary_operation
        bop
        left
        right
        (show_binary_operator bop ^ "result_n")
        builder
    in
    let result = build_zext result i64 "zext_n" builder in
    ok @@ result
  | CApplication (func, arg) ->
    let* callee = codegen_immexpr env func in
    let* arg = codegen_immexpr env arg in
    let func_ptr =
      let alloc_closure = lookup_function_exn "peducoml_alloc_closure" the_module in
      let fnty = function_type i64 [| i64; i64 |] in
      build_call
        fnty
        alloc_closure
        [| build_pointercast callee i64 "ptr_to_i64_n" builder
         ; params callee |> Base.Array.length |> const_int i64
        |]
        "peducoml_alloc_closure_n"
        builder
    in
    let apply = lookup_function_exn "peducoml_apply" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    ok @@ build_call fnty apply [| func_ptr; arg |] "peducoml_apply_n" builder
  | _ -> failwith "TODO"
;;

let rec codegen_aexpr env = function
  | ACExpr cexpr -> codegen_cexpr env cexpr
  | ALet (id, cexpr, aexpr) ->
    let alloca = build_alloca i64 (string_of_unique_id id ^ "_n") builder in
    let* cexpr = codegen_cexpr env cexpr in
    build_store cexpr alloca builder |> ignore;
    let env = Base.Map.Poly.set env ~key:id ~data:alloca in
    codegen_aexpr env aexpr
;;

let codegen_global_scope_function env (func : global_scope_function) =
  let id, arg_list, body = func in
  let fnty = function_type i64 (Array.make (Base.List.length arg_list) i64) in
  let func = declare_function id fnty the_module in
  Base.Array.iter
    (Base.Array.zip_exn (Base.List.to_array arg_list) (params func))
    ~f:(fun (name, value) ->
      match name with
      | ImmId id -> set_value_name (string_of_unique_id id ^ "_n") value
      | _ -> failwith "Do arguments need to be immexprs?");
  let basic_block = append_block context "entry" func in
  position_at_end basic_block builder;
  let env_with_args =
    Base.Array.fold_right
      (Base.Array.zip_exn (Base.List.to_array arg_list) (params func))
      ~init:env
      ~f:(fun (name, value) acc ->
        match name with
        | ImmId id ->
          let alloca = build_alloca i64 (string_of_unique_id id ^ "_n") builder in
          build_store value alloca builder |> ignore;
          Base.Map.Poly.set acc ~key:id ~data:alloca
        | _ -> failwith "Do they?")
  in
  let* return_val = codegen_aexpr env_with_args body in
  let _ = build_ret return_val builder in
  ok (func, env)
;;

open Peducoml_stdlib

let codegen program =
  let env =
    Base.List.map stdlib ~f:(fun (id, _) ->
      declare_function id (function_type i64 [| i64 |]) the_module)
  in
  let env =
    declare_function
      "peducoml_alloc_closure"
      (function_type i64 [| i64; i64 |])
      the_module
    :: declare_function "peducoml_apply" (function_type i64 [| i64; i64 |]) the_module
    :: declare_function "peducoml_alloc_list" (function_type i64 [||]) the_module
    :: declare_function
         "peducoml_add_to_list"
         (function_type i64 [| i64; i64 |])
         the_module
    :: declare_function "peducoml_field" (function_type i64 [| i64; i64 |]) the_module
    :: declare_function "peducoml_tail" (function_type i64 [| i64 |]) the_module
    :: declare_function "peducoml_length" (function_type i64 [| i64 |]) the_module
    :: declare_function "peducoml_alloc_tuple" (function_type i64 [| i64 |]) the_module
    :: declare_function
         "peducoml_fill_tuple"
         (function_type i64 [| i64; i64 |])
         the_module
    :: env
  in
  let rec codegen acc env = function
    | [] -> ok @@ acc
    | head :: tail ->
      let* head, env = codegen_global_scope_function env head in
      codegen (head :: acc) env tail
  in
  (* let init_env = Base.Map.Poly.singleton (GlobalScopeId "print_int") rt_print_int in *)
  let* result = codegen env Base.Map.Poly.empty program in
  ok @@ Base.List.rev result
;;
