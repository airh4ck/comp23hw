open Ast
open Anf.State

let lambda_lift =
  let get_constructor = function
    | DDeclaration _ -> ddeclaration
    | DRecursiveDeclaration _ -> drecursivedeclaration
  in
  let ll_expr expr =
    let rec ll_decl env lifted_decls = function
      | ( DDeclaration (name, pattern_list, body)
        | DRecursiveDeclaration (name, pattern_list, body) ) as original ->
        let* body, lifted = ll_expr env lifted_decls body in
        let decl = get_constructor original in
        let* fresh_var = fresh in
        let fresh_name = "`ll_" ^ Base.Int.to_string fresh_var in
        return @@ (name, fresh_name, decl fresh_name pattern_list body, lifted)
    and ll_expr env lifted_decls = function
      | EFun (first_arg, other_args, body) ->
        let* fresh_var = fresh in
        let id = "`ll_" ^ Base.Int.to_string fresh_var in
        let* body, lifted = ll_expr env lifted_decls body in
        return
        @@ (eidentifier id, ddeclaration id (first_arg :: other_args) body :: lifted)
      | ELetIn (first_decl, other_decls, body) ->
        let* old_name, fresh_name, first, lifted = ll_decl env lifted_decls first_decl in
        let lifted = first :: lifted in
        let* env, lifted =
          Base.List.fold_right
            other_decls
            ~f:(fun decl acc ->
              let* env, lifted = acc in
              let* old_name, fresh_name, decl, lifted = ll_decl env lifted decl in
              return @@ (Base.Map.set env ~key:old_name ~data:fresh_name, decl :: lifted))
            ~init:(return (Base.Map.set env ~key:old_name ~data:fresh_name, lifted))
        in
        let* body, lifted = ll_expr env lifted body in
        return (body, lifted)
      | EBinaryOperation (bop, left, right) ->
        let* left, lifted = ll_expr env lifted_decls left in
        let* right, lifted = ll_expr env lifted right in
        return @@ (ebinary_operation bop left right, lifted)
      | EUnaryOperation (operation, operand) ->
        let* operand, lifted = ll_expr env lifted_decls operand in
        return @@ (eunary_operation operation operand, lifted)
      | EApplication (func, arg) ->
        let* func, lifted = ll_expr env lifted_decls func in
        let* arg, lifted = ll_expr env lifted arg in
        return @@ (eapplication func arg, lifted)
      | EList expr_list ->
        let* expr_list, lifted =
          Base.List.fold_right
            expr_list
            ~f:(fun expr acc ->
              let* expr_acc, lifted_acc = acc in
              let* expr, lifted_from_expr = ll_expr env lifted_acc expr in
              return @@ (expr :: expr_acc, lifted_from_expr))
            ~init:(return ([], lifted_decls))
        in
        return @@ (elist expr_list, lifted)
      | EConstructList (head, tail) ->
        let* head, lifted = ll_expr env lifted_decls head in
        let* tail, lifted = ll_expr env lifted tail in
        return @@ (econstruct_list head tail, lifted)
      | ETuple (first_elem, second_elem, other_elems) ->
        let* first, lifted = ll_expr env lifted_decls first_elem in
        let* second, lifted = ll_expr env lifted second_elem in
        let* other_elems, lifted =
          Base.List.fold_right
            other_elems
            ~f:(fun expr acc ->
              let* expr_acc, lifted_acc = acc in
              let* expr, lifted_from_expr = ll_expr env lifted_acc expr in
              return @@ (expr :: expr_acc, lifted_from_expr))
            ~init:(return ([], lifted))
        in
        return @@ (etuple first second other_elems, lifted)
      | EIf (condition, true_branch, false_branch) ->
        let* condition, lifted = ll_expr env lifted_decls condition in
        let* true_branch, lifted = ll_expr env lifted true_branch in
        let* false_branch, lifted = ll_expr env lifted false_branch in
        return @@ (eif condition true_branch false_branch, lifted)
      | EMatchWith (matched, (first_pattern, first_action), other_cases) ->
        let* matched, lifted = ll_expr env lifted_decls matched in
        let* first_action, lifted = ll_expr env lifted first_action in
        let* other_cases, lifted =
          Base.List.fold_right
            other_cases
            ~f:(fun (pattern, action) acc ->
              let* case_acc, lifted_acc = acc in
              let* action, lifted = ll_expr env lifted_acc action in
              return @@ ((pattern, action) :: case_acc, lifted))
            ~init:(return ([], lifted))
        in
        return @@ (ematchwith matched (first_pattern, first_action) other_cases, lifted)
      | EIdentifier id ->
        (match Base.Map.find env id with
         | None -> return @@ (eidentifier id, lifted_decls)
         | Some new_id -> return @@ (eidentifier new_id, lifted_decls))
      | expr -> return (expr, lifted_decls)
    in
    ll_expr (Base.Map.empty (module Base.String)) [] expr
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
