(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast
open Llvm

let context = global_context ()
let the_module = create_module context "PeDuCoML"
let builder = builder context
let i64 = i64_type context
let lookup_function_exn id llmodule = Option.get @@ lookup_function id llmodule
(* let void = void_type context *)
(* let i64_array = array_type i64 *)

(* let create_entry_block_alloca func var =
   let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block func)) in
   Llvm.build_alloca i64 var builder
   ;; *)

type llvm_error = UnboundVariable of unique_id

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
  | ImmBool v -> ok @@ const_int i64 (Bool.to_int v)
  | ImmId id ->
    (match Base.Map.Poly.find env id, id with
     | _, GlobalScopeId id -> ok @@ lookup_function_exn id the_module
     | None, _ -> error @@ UnboundVariable id
     | Some llvalue, AnfId id -> ok @@ build_load2 i64 llvalue (string_of_int id) builder)
  | ImmList imm_list | ImmTuple imm_list ->
    let* arr =
      Base.List.fold_right imm_list ~init:(ok []) ~f:(fun imm acc ->
        let* acc = acc in
        let* imm = codegen_immexpr env imm in
        ok @@ (imm :: acc))
    in
    ok @@ const_array i64 (Base.List.to_array arr)
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
    let rez = build_binary_operation bop left right "boptmp" builder in
    let rez = build_zext rez i64 "zext" builder in
    ok @@ rez
  | CApplication (func, arg) ->
    let* callee = codegen_immexpr env func in
    let* arg = codegen_immexpr env arg in
    let fnty = function_type i64 [| i64 |] in
    ok @@ build_call2 fnty callee [| arg |] "tmp_call1" builder
  | _ -> failwith "TODO"
;;

let rec codegen_aexpr env = function
  | ACExpr cexpr -> codegen_cexpr env cexpr
  | ALet (id, cexpr, aexpr) ->
    let alloca = build_alloca i64 (string_of_unique_id id) builder in
    let* cexpr = codegen_cexpr env cexpr in
    build_store cexpr alloca builder |> ignore;
    let env = Base.Map.Poly.set env ~key:id ~data:alloca in
    codegen_aexpr env aexpr
;;

let codegen_proto name args =
  let arg_types = Array.make (Base.List.length args) i64 in
  let function_type = function_type i64 arg_types in
  ok @@ declare_function name function_type the_module
;;

let codegen_global_scope_function env (func : global_scope_function) =
  let id, arg_list, body = func in
  let* func = codegen_proto id arg_list in
  let basic_block = append_block context "entry" func in
  position_at_end basic_block builder;
  let* return_val = codegen_aexpr env body in
  let _ = build_ret return_val builder in
  ok func
;;

(* let build_example =
  let func = declare_function "main" (function_type i64 [||]) the_module in
  let basic_block = append_block context "entry" func in
  position_at_end basic_block builder;
  let callee_type = function_type i64 [| i64 |] in
  (* let callee = Option.get @@ lookup_function "print_int" the_module in *)
  let callee = rt in
  let arguments = [| const_int i64 42 |] in
  let _ = build_call2 callee_type callee arguments "tmp_call" builder in
  let _ = build_ret (const_int i64 0) builder in
  func
;; *)

open Peducoml_stdlib

let codegen program =
  let env =
    Base.List.map stdlib ~f:(fun (id, _) ->
      declare_function id (function_type i64 [| i64 |]) the_module)
  in
  let rec codegen acc env = function
    | [] -> ok @@ acc
    | head :: tail ->
      let* head = codegen_global_scope_function env head in
      codegen (head :: acc) env tail
  in
  (* let init_env = Base.Map.Poly.singleton (GlobalScopeId "print_int") rt_print_int in *)
  let* result = codegen env Base.Map.Poly.empty program in
  ok @@ Base.List.rev result
;;
