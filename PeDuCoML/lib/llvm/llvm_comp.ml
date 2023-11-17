(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Llvm

let context = global_context ()
let the_module = create_module context "PeDuCoML"
let builder = builder context
let i64 = i64_type context

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
  | ImmChar c -> ok @@ const_string context (String.make 1 c)
  | ImmBool v -> ok @@ const_int i64 (Bool.to_int v)
  | ImmId id ->
    (match Base.Map.Poly.find env id with
     | None -> error @@ UnboundVariable id
     | Some llvalue -> ok @@ build_load2 i64 llvalue (string_of_unique_id id) builder)
  | ImmList imm_list | ImmTuple imm_list ->
    let* arr =
      Base.List.fold_right imm_list ~init:(ok []) ~f:(fun imm acc ->
        let* acc = acc in
        let* imm = codegen_immexpr env imm in
        ok @@ (imm :: acc))
    in
    ok @@ const_array i64 (Base.List.to_array arr)
;;

let codegen_cexpr env = function
  | CImm imm_expr -> codegen_immexpr env imm_expr
  | _ -> failwith "TODO"
;;

let codegen_aexpr env = function
  | ACExpr cexpr -> codegen_cexpr env cexpr
  | _ -> failwith "TODO"
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

let codegen =
  let rec codegen acc env = function
    | [] -> ok acc
    | head :: tail ->
      let* head = codegen_global_scope_function env head in
      codegen (head :: acc) env tail
  in
  codegen [] Base.Map.Poly.empty
;;
