open PeDuCoML.Parser
open PeDuCoML.Pprintast
open PeDuCoML.Closure_conversion
open PeDuCoML.Ast
open PeDuCoML.Util
open PeDuCoML.Inferencer
open PeDuCoML.Typing

type func =
  | FFun of expression
  | FLet of declaration

let ffun expr = FFun expr
let flet decl = FLet decl

let check_closures =
  let find_all_identifiers pattern_list =
    Base.List.fold_right
      pattern_list
      ~f:(fun pattern acc -> Base.Set.union (find_identifiers_pattern pattern) acc)
      ~init:(Base.Set.empty (module Base.String))
  in
  let rec check_closures_decl env failed = function
    | ( DDeclaration (_, pattern_list, body)
      | DRecursiveDeclaration (_, pattern_list, body) ) as original ->
      let arg_ids = find_all_identifiers pattern_list in
      let env = Base.Set.union env arg_ids in
      let free_vars = Base.Set.diff (find_identifiers body) env in
      let failed =
        if Base.Set.is_empty free_vars then failed else flet original :: failed
      in
      check_closures_expr env failed body
  and check_closures_expr env failed = function
    | EBinaryOperation (_, left, right) ->
      check_closures_expr env (check_closures_expr env failed left) right
    | EUnaryOperation (_, operand) -> check_closures_expr env failed operand
    | EApplication (func, arg) ->
      check_closures_expr env (check_closures_expr env failed func) arg
    | EFun (first_arg, other_args, body) as original ->
      let arg_ids = find_all_identifiers (first_arg :: other_args) in
      let env = Base.Set.union env arg_ids in
      let free_vars = Base.Set.diff (find_identifiers body) env in
      let failed =
        if Base.Set.is_empty free_vars then failed else ffun original :: failed
      in
      check_closures_expr env failed body
    | EList expr_list ->
      (match expr_list with
       | head :: tail ->
         check_closures_expr env (check_closures_expr env failed head) (elist tail)
       | [] -> failed)
    | EConstructList (head, tail) ->
      check_closures_expr env (check_closures_expr env failed head) tail
    | ETuple (first_elem, second_elem, other_elems) ->
      check_closures_expr env failed (elist @@ (first_elem :: second_elem :: other_elems))
    | ELetIn (first_decl, other_decls, body) ->
      let failed = check_closures_decl env failed first_decl in
      (match other_decls with
       | head :: tail -> check_closures_expr env failed (eletin head tail body)
       | [] -> check_closures_expr env failed body)
    | EIf (condition, true_branch, false_branch) ->
      let failed = check_closures_expr env failed condition in
      check_closures_expr env (check_closures_expr env failed true_branch) false_branch
    | EMatchWith (matched, first_case, other_cases) ->
      let pattern, action = first_case in
      let failed =
        check_closures_expr
          (Base.Set.union env (find_identifiers_pattern pattern))
          failed
          action
      in
      (match other_cases with
       | head :: tail -> check_closures_expr env failed (ematchwith matched head tail)
       | [] -> check_closures_expr env failed matched)
    | _ -> failed
  in
  let rec helper acc = function
    | [] -> acc
    | head :: tail ->
      (match head with
       | DDeclaration (_, pattern_list, body)
       | DRecursiveDeclaration (_, pattern_list, body) ->
         let arg_ids = find_all_identifiers pattern_list in
         helper (check_closures_expr arg_ids [] body) tail)
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
    (match R.run (check_types ast) with
     | Ok _ ->
       let closure = run_closure ast in
       if debug
       then List.iter (fun decl -> Format.printf "%a\n" pp_declaration decl) closure
       else (
         match check_closures closure with
         | [] -> Format.printf "Ok!\n"
         | failed -> List.iter (fun func -> Format.printf "%a\n" pp_func func) failed)
     | Error err -> print_type_error err)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  let debug = Array.length Sys.argv > 1 && Sys.argv.(1) = "debug" in
  print_anf code debug
;;
