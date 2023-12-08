(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Result
open Riscv

type riscv_error = UnboundVariable of unique_id

let unbound id = UnboundVariable id

module Environment = struct
  type t = (unit, riscv_error) Base.Map.Poly.t

  let find = Base.Map.Poly.find
  let empty = Base.Map.Poly.empty
end

let ( let* ) = bind

open Environment

let string_of_unique_id = function
  | AnfId id -> string_of_int id
  | GlobalScopeId id -> id
;;

let codegen_immexpr env = function
  | ImmInt num -> ok @@ const_int num
  | ImmChar c -> ok @@ const_int (Base.Char.to_int c)
  | ImmBool v -> ok @@ const_int (Base.Bool.to_int v)
  | ImmId (AnfId id) ->
    (match find env (AnfId id) with
     | None -> AnfId id |> unbound |> error
     | Some value -> ok @@ build_load value)
  | _ -> failwith "TODO"
;;

let codegen_cexpr env = function
  | CImm immexpr -> codegen_immexpr env immexpr
  | _ -> failwith "TODO"
;;

let codegen_aexpr env = function
  | ACExpr cexpr -> codegen_cexpr env cexpr
  | _ -> failwith "TODO"
;;

let codegen_global_scope_function env (func : global_scope_function) =
  let function_name, _, body = func in
  let* body = codegen_aexpr env body in
  let func = declare_function function_name in
  build_ret body;
  ok func
;;

let codegen : global_scope_function list -> (unit, riscv_error) Result.t =
  (* Format.asprintf ".text\n"; *)
  let rec codegen env = function
    | [] -> ok ()
    | head :: tail ->
      let* _ = codegen_global_scope_function env head in
      codegen env tail
  in
  codegen empty
;;
