open PeDuCoML.Parser
open PeDuCoML.Pprintast
open PeDuCoML.Closure_conversion
open PeDuCoML.Lambda_lift
open PeDuCoML.Ast

type func =
  | FFun of expression
  | FLet of declaration

let ffun expr = FFun expr
let flet decl = FLet decl

let check_nested_functions =
  let rec check_decl depth failed = function
    | (DDeclaration (_, _, body) | DRecursiveDeclaration (_, _, body)) as original ->
      let failed = if depth > 0 then flet original :: failed else failed in
      check_expr (depth + 1) failed body
  and check_expr depth failed =
    let next_depth = depth + 1 in
    function
    | EBinaryOperation (_, left, right) ->
      check_expr next_depth (check_expr next_depth failed left) right
    | EUnaryOperation (_, operand) -> check_expr next_depth failed operand
    | EApplication (func, arg) ->
      check_expr next_depth (check_expr next_depth failed func) arg
    | EFun (_, _, body) as original ->
      let failed = if depth > 0 then ffun original :: failed else failed in
      check_expr next_depth failed body
    | EList expr_list ->
      (match expr_list with
       | [] -> failed
       | head :: tail ->
         check_expr next_depth (check_expr next_depth failed head) (elist tail))
    | EConstructList (head, tail) ->
      check_expr next_depth (check_expr next_depth failed head) tail
    | ETuple (first_elem, second_elem, other_elems) ->
      check_expr depth failed (elist @@ (first_elem :: second_elem :: other_elems))
    | ELetIn (first_decl, other_decls, body) ->
      let failed = check_decl depth failed first_decl in
      (match other_decls with
       | head :: tail -> check_expr next_depth failed (eletin head tail body)
       | [] -> check_expr next_depth failed body)
    | EIf (condition, true_branch, false_branch) ->
      let failed = check_expr next_depth failed condition in
      check_expr next_depth (check_expr next_depth failed true_branch) false_branch
    | EMatchWith (matched, (_, action), other_cases) ->
      let failed = check_expr next_depth failed action in
      (match other_cases with
       | head :: tail -> check_expr next_depth failed (ematchwith matched head tail)
       | [] -> check_expr next_depth failed matched)
    | _ -> failed
  in
  let rec helper acc = function
    | [] -> acc
    | head :: tail ->
      (match head with
       | DDeclaration (_, _, body) | DRecursiveDeclaration (_, _, body) ->
         helper (check_expr 0 [] body) tail)
  in
  helper []
;;

let pp_func fmt = function
  | FFun expr -> pp_expression fmt expr
  | FLet decl -> pp_declaration fmt decl
;;

let print_anf code debug =
  match parse code with
  | Ok ast ->
    let closure = run_closure ast in
    let defunced = run_lambda_lifting closure in
    if debug
    then List.iter (fun decl -> Format.printf "%a\n" pp_declaration decl) defunced
    else (
      match check_nested_functions defunced with
      | [] -> Format.printf "Ok!\n"
      | failed -> List.iter (fun func -> Format.printf "%a\n" pp_func func) failed)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  let debug = Array.length Sys.argv > 1 && Sys.argv.(1) = "debug" in
  print_anf code debug
;;
