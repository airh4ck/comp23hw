(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open PeDuCoML.Parser
open PeDuCoML.Inferencer
open PeDuCoML.Typing
open PeDuCoML.Pprintanf
open PeDuCoML.Anf
open PeDuCoML.Closure_conversion
open PeDuCoML.Lambda_lift

let print_anf code =
  match parse code with
  | Ok ast ->
    (match R.run (check_types ast) with
     | Ok _ ->
       let closure = run_closure ast in
       let defunced = run_lambda_lifting closure in
       let anf = run_anf_conversion defunced in
       Base.List.iter
         ~f:(fun func -> Format.printf "%a\n" pp_global_scope_function func)
         anf
     | Error err -> print_type_error err)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  print_anf code
;;
