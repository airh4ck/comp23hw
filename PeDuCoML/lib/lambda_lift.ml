open Ast
open Anf.State

let lambda_lift =
  let ll_expr expr =
    let rec ll_decl lifted_decls = function
      | _ -> failwith "TODO"
    and ll_expr lifted_decls = function
      | EFun (first_arg, other_args, body) ->
        let* fresh_var = fresh in
        let id = "`ll_" ^ Base.Int.to_string fresh_var in
        return
        @@ (eidentifier id, ddeclaration id (first_arg :: other_args) body :: lifted_decls)
      | ELetIn (first_decl, other_decls, body) ->
        let lifted_decls = first_decl :: lifted_decls in
        let lifted_decls =
          Base.List.fold_right
            other_decls
            ~f:(fun decl acc -> decl :: acc)
            ~init:lifted_decls
        in
        let* body, lifted_decls = ll_expr lifted_decls body in
        return (body, lifted_decls)
      | EBinaryOperation (bop, left, right) ->
        let* left, lifted = ll_expr lifted_decls left in
        let* right, lifted = ll_expr lifted right in
        return @@ (ebinary_operation bop left right, lifted)
      | EUnaryOperation (operation, operand) ->
        let* operand, lifted = ll_expr lifted_decls operand in
        return @@ (eunary_operation operation operand, lifted)
      | EApplication (func, arg) ->
        let* func, lifted = ll_expr lifted_decls func in
        let* arg, lifted = ll_expr lifted arg in
        return @@ (eapplication func arg, lifted)
      | EList expr_list ->
        let* expr_list, lifted =
          Base.List.fold_right
            expr_list
            ~f:(fun expr acc ->
              let* expr_acc, lifted_acc = acc in
              let* expr, lifted_from_expr = ll_expr lifted_acc expr in
              return @@ (expr :: expr_acc, lifted_from_expr))
            ~init:(return ([], lifted_decls))
        in
        return @@ (elist expr_list, lifted)
      | EConstructList (head, tail) ->
        let* head, lifted = ll_expr lifted_decls head in
        let* tail, lifted = ll_expr lifted tail in
        return @@ (econstruct_list head tail, lifted)
      | ETuple (first_elem, second_elem, other_elems) ->
        let* first, lifted = ll_expr lifted_decls first_elem in
        let* second, lifted = ll_expr lifted second_elem in
        let* other_elems, lifted =
          Base.List.fold_right
            other_elems
            ~f:(fun expr acc ->
              let* expr_acc, lifted_acc = acc in
              let* expr, lifted_from_expr = ll_expr lifted_acc expr in
              return @@ (expr :: expr_acc, lifted_from_expr))
            ~init:(return ([], lifted))
        in
        return @@ (etuple first second other_elems, lifted)
      | EIf (condition, true_branch, false_branch) ->
        let* condition, lifted = ll_expr lifted_decls condition in
        let* true_branch, lifted = ll_expr lifted true_branch in
        let* false_branch, lifted = ll_expr lifted false_branch in
        return @@ (eif condition true_branch false_branch, lifted)
      | EMatchWith (matched, (first_pattern, first_action), other_cases) ->
        let* matched, lifted = ll_expr lifted_decls matched in
        let* first_action, lifted = ll_expr lifted first_action in
        let* other_cases, lifted =
          Base.List.fold_right
            other_cases
            ~f:(fun (pattern, action) acc ->
              let* case_acc, lifted_acc = acc in
              let* action, lifted = ll_expr lifted_acc action in
              return @@ ((pattern, action) :: case_acc, lifted))
            ~init:(return ([], lifted))
        in
        return @@ (ematchwith matched (first_pattern, first_action) other_cases, lifted)
      | expr -> return (expr, lifted_decls)
    in
    ll_expr [] expr
  in
  let get_constructor = function
    | DDeclaration _ -> ddeclaration
    | DRecursiveDeclaration _ -> drecursivedeclaration
  in
  let rec ll_program = function
    | [] -> return []
    | head :: tail ->
      (match head with
       | ( DDeclaration (name, pattern_list, body)
         | DRecursiveDeclaration (name, pattern_list, body) ) as original ->
         let* body, lifted_decls = ll_expr body in
         let decl = get_constructor original in
         let* tail = ll_program tail in
         return @@ lifted_decls @ [ decl name pattern_list body ] @ tail)
  in
  ll_program
;;

let run_lambda_lifting program = run (lambda_lift program)
