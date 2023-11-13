(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type unique_id =
  | AnfId of int
  | GlobalScopeId of string

type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmChar of char
  | ImmBool of bool
  | ImmId of unique_id

type cexpr =
  | CBinaryOperation of binary_operator * imm_expr * imm_expr
  | CUnaryOperation of unary_operator * imm_expr
  | CApplication of imm_expr * imm_expr
  | CList of imm_expr list
  | CTuple of imm_expr list
  | CIf of imm_expr * imm_expr * imm_expr
  | CConstructList of imm_expr * imm_expr
  | CImm of imm_expr

type aexpr =
  | ALet of unique_id * cexpr * aexpr
  | ACExpr of cexpr

type global_scope_function = string * imm_expr list * aexpr

(* Smart constructors *)
(* unique_id *)
let anf_id n = AnfId n
let global_scope_id name = GlobalScopeId name

(* imm_expr *)
let imm_int num = ImmInt num
let imm_string str = ImmString str
let imm_char sym = ImmChar sym
let imm_bool b = ImmBool b
let imm_id id = ImmId id

(* cexpr *)
let cbinary_operation bop left right = CBinaryOperation (bop, left, right)
let cunary_operation uop expr = CUnaryOperation (uop, expr)
let cimm imm_expr = CImm imm_expr
let capplication fun_imm arg_imm = CApplication (fun_imm, arg_imm)
let clist imm_expr_list = CList imm_expr_list
let ctuple imm_expr_list = CTuple imm_expr_list
let cif condition true_branch false_branch = CIf (condition, true_branch, false_branch)
let cconstruct_list operand imm_expr_list = CConstructList (operand, imm_expr_list)

(* aexpr *)
let alet id cexpr aexpr = ALet (id, cexpr, aexpr)
let acexpr cexpr = ACExpr cexpr
let acimm imm_expr = acexpr @@ cimm imm_expr
(* ------------------ *)

open State
open Match_elim

(* Runtime fuctions (unavailable to users)
   | name        | args         |
   ------------------------------
   | `head       | list         |
   | `tail       | list         |
   | `length     | list         |
   | `at_list    | list index   |
   | `at_tuple   | tuple index  |
   ------------------------------*)

let rec anf (env : (string, unique_id, Base.String.comparator_witness) Base.Map.t) expr k =
  match expr with
  | MFLiteral literal ->
    (match literal with
     | LInt num -> k (imm_int num)
     | LString str -> k (imm_string str)
     | LChar sym -> k (imm_char sym)
     | LBool b -> k (imm_bool b))
  | MFIdentifier x -> k @@ imm_id (Base.Map.find_exn env x)
  | MFBinaryOperation (bop, left, right) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id (anf_id fresh_var) in
    anf env left (fun limm ->
      anf env right (fun rimm ->
        return @@ alet (anf_id fresh_var) (cbinary_operation bop limm rimm) body))
  | MFUnaryOperation (uop, expr) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id (anf_id fresh_var) in
    anf env expr (fun imm ->
      return @@ alet (anf_id fresh_var) (cunary_operation uop imm) body)
  | MFApplication (fun_expr, arg_expr) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id (anf_id fresh_var) in
    anf env fun_expr (fun imm_fun_expr ->
      anf env arg_expr (fun imm_arg_expr ->
        return @@ alet (anf_id fresh_var) (capplication imm_fun_expr imm_arg_expr) body))
  | MFList expr_list ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id (anf_id fresh_var) in
    let rec helper curr_list = function
      | head :: tail -> anf env head (fun imm -> helper (imm :: curr_list) tail)
      | _ -> return @@ alet (anf_id fresh_var) (clist @@ Base.List.rev curr_list) body
    in
    helper [] expr_list
  | MFTuple (first_elem, second_elem, other_elems) ->
    let all_elems = first_elem :: second_elem :: other_elems in
    let* fresh_var = fresh in
    let* body = k @@ imm_id (anf_id fresh_var) in
    let rec helper curr_list = function
      | head :: tail -> anf env head (fun imm -> helper (imm :: curr_list) tail)
      | _ -> return @@ alet (anf_id fresh_var) (ctuple @@ Base.List.rev curr_list) body
    in
    helper [] all_elems
  | MFIf (condition, true_branch, false_branch) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id (anf_id fresh_var) in
    anf env condition (fun condition_imm ->
      anf env true_branch (fun true_branch_imm ->
        anf env false_branch (fun false_branch_imm ->
          return
          @@ alet
               (anf_id fresh_var)
               (cif condition_imm true_branch_imm false_branch_imm)
               body)))
  | MFConstructList (operand, expr_list) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id (anf_id fresh_var) in
    anf env operand (fun operand_imm ->
      anf env expr_list (fun expr_list_imm ->
        return @@ alet (anf_id fresh_var) (cconstruct_list operand_imm expr_list_imm) body))
  | MFLetIn (first_declaration, other_declarations, body) ->
    let* fresh_var = fresh in
    let name, expr = first_declaration in
    let new_env = Base.Map.set env ~key:name ~data:(anf_id fresh_var) in
    anf env expr (fun imm_expr ->
      match other_declarations with
      | head :: tail ->
        let* body = anf new_env (MFLetIn (head, tail, body)) k in
        return @@ alet (anf_id fresh_var) (cimm imm_expr) body
      | _ ->
        let* body = anf new_env body k in
        return @@ alet (anf_id fresh_var) (cimm imm_expr) body)
;;

let process_declaration env declaration =
  let update_map env args_list =
    Base.List.fold_right args_list ~init:(return env) ~f:(fun id acc ->
      let* fresh_var = fresh in
      let* acc = acc in
      return @@ Base.Map.set acc ~key:id ~data:(anf_id fresh_var))
  in
  let gen_imm_id env name = imm_id @@ Base.Map.find_exn env name in
  let gen_global_scope_function env name args_list expr =
    let* env = update_map env args_list in
    let* anf_representation = anf env expr (fun imm -> return @@ acimm imm) in
    return (name, List.map (gen_imm_id env) args_list, anf_representation)
  in
  match declaration with
  | MFDeclaration (name, args_list, expr) ->
    let* global_scope_f = gen_global_scope_function env name args_list expr in
    return global_scope_f
  | MFRecursiveDeclaration (name, args_list, expr) ->
    let env = Base.Map.set env ~key:name ~data:(global_scope_id name) in
    let* global_scope_f = gen_global_scope_function env name args_list expr in
    return global_scope_f
;;

let anf_conversion program =
  let rec helper env current_list = function
    | head :: tail ->
      let* ((name, _, _) as global_scope_f) = process_declaration env head in
      let env = Base.Map.set env ~key:name ~data:(global_scope_id name) in
      helper env (global_scope_f :: current_list) tail
    | _ -> return @@ List.rev current_list
  in
  let env = Base.Map.empty (module Base.String) in
  let env = Base.Map.set env ~key:"`head" ~data:(global_scope_id "`head") in
  let env = Base.Map.set env ~key:"`tail" ~data:(global_scope_id "`tail") in
  let env = Base.Map.set env ~key:"`length" ~data:(global_scope_id "`length") in
  let env = Base.Map.set env ~key:"`at_list" ~data:(global_scope_id "`at_list") in
  let env = Base.Map.set env ~key:"`at_tuple" ~data:(global_scope_id "`at_tuple") in
  helper env [] program
;;

let run_anf_conversion program : global_scope_function list =
  run @@ anf_conversion program
;;
