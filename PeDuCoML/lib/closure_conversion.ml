open Util
open Ast

(* module ClosureEnv = struct
  type t =
    ( id
    , (id, Base.String.comparator_witness) Base.Set.t
    , Base.String.comparator_witness )
    Base.Map.t
end *)

let closure_conversion =
  let rec closure_declaration env =
    let get_constructor = function
      | DDeclaration _ -> ddeclaration
      | DRecursiveDeclaration _ -> drecursivedeclaration
    in
    function
    | ( DDeclaration (name, pattern_list, body)
      | DRecursiveDeclaration (name, pattern_list, body) ) as original ->
      let pattern_args =
        List.fold_right
          (fun pattern acc -> Base.Set.union (find_identifiers_pattern pattern) acc)
          pattern_list
          empty
      in
      let free_vars = Base.Set.diff (find_identifiers body) pattern_args in
      let closed_args =
        Base.Set.fold
          free_vars
          ~f:(fun acc var -> pidentifier var :: acc)
          ~init:pattern_list
      in
      let decl = get_constructor original in
      let env = Base.Map.set env ~key:name ~data:free_vars in
      decl name closed_args (closure_expression env body), env
  and closure_expression env = function
    | EFun (first_arg, other_args, body) as original ->
      let pattern_list = first_arg :: other_args in
      let pattern_args =
        List.fold_right
          (fun pattern acc -> Base.Set.union (find_identifiers_pattern pattern) acc)
          pattern_list
          empty
      in
      let free_vars = Base.Set.diff (find_identifiers body) pattern_args in
      let closed_args =
        Base.Set.fold
          free_vars
          ~f:(fun acc var -> pidentifier var :: acc)
          ~init:pattern_list
      in
      (match closed_args with
       | [] -> original
       | head :: tail ->
         Base.Set.fold_right
           free_vars
           ~f:(fun var acc -> eapplication acc (eidentifier var))
           ~init:(efun head tail body))
    | ELetIn (first_decl, other_decls, body) ->
      let decl, env = closure_declaration env first_decl in
      let rec helper decl_list acc env =
        match decl_list with
        | head :: tail ->
          let head_decl, env = closure_declaration env head in
          helper tail (head_decl :: acc) env
        | _ -> acc
      in
      eletin decl (helper other_decls [] env) (closure_expression env body)
    | EIdentifier id as original ->
      (match Base.Map.find env id with
       | None -> original
       | Some free_vars ->
         Base.Set.fold
           free_vars
           ~f:(fun acc var -> eapplication acc (eidentifier var))
           ~init:original)
    | EBinaryOperation (bop, left, right) ->
      let left = closure_expression env left in
      let right = closure_expression env right in
      ebinary_operation bop left right
    | EUnaryOperation (operator, operand) ->
      let operand = closure_expression env operand in
      eunary_operation operator operand
    | EApplication (func, arg) ->
      let func = closure_expression env func in
      let arg = closure_expression env arg in
      eapplication func arg
    | EList expr_list ->
      let expr_list =
        Base.List.map expr_list ~f:(fun expr -> closure_expression env expr)
      in
      elist expr_list
    | EConstructList (head, tail) ->
      let head = closure_expression env head in
      let tail = closure_expression env tail in
      econstruct_list head tail
    | ETuple (first_elem, second_elem, other_elems) ->
      let first_elem = closure_expression env first_elem in
      let second_elem = closure_expression env second_elem in
      let other_elems =
        Base.List.map other_elems ~f:(fun elem -> closure_expression env elem)
      in
      etuple first_elem second_elem other_elems
    | EIf (expr, true_branch, false_branch) ->
      let expr = closure_expression env expr in
      let true_branch = closure_expression env true_branch in
      let false_branch = closure_expression env false_branch in
      eif expr true_branch false_branch
    | EMatchWith (matched, first_case, other_cases) ->
      let matched = closure_expression env matched in
      let first_case = fst first_case, closure_expression env (snd first_case) in
      let other_cases =
        Base.List.map other_cases ~f:(fun (pattern, action) ->
          pattern, closure_expression env action)
      in
      ematchwith matched first_case other_cases
    | expr -> expr
  in
  closure_expression
;;

let run_closure =
  let get_constructor = function
    | DDeclaration _ -> ddeclaration
    | DRecursiveDeclaration _ -> drecursivedeclaration
  in
  let env = Base.Map.empty (module Base.String) in
  let rec helper acc = function
    | [] -> acc
    | head :: tail ->
      (match head with
       | ( DDeclaration (id, pattern_list, body)
         | DRecursiveDeclaration (id, pattern_list, body) ) as original ->
         let decl = get_constructor original in
         let body = closure_conversion env body in
         helper (decl id pattern_list body :: acc) tail)
  in
  helper []
;;
