open Ast
open Util

type unique_id = int [@@deriving eq, show { with_path = false }]

type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmChar of char
  | ImmBool of bool
  | ImmUnit
  | ImmId of unique_id

type cexpr =
  | CBinaryOperation of binary_operator * imm_expr * imm_expr
  | CUnaryOperation of unary_operator * imm_expr
  | CApplication of imm_expr * imm_expr
  | CList of imm_expr list
  | CTuple of imm_expr list
  | CIf of imm_expr * imm_expr * imm_expr
  | CConstructList of imm_expr * imm_expr
  (* | CMatchWith of imm_expr * (pattern * aexpr) list *)
  | CImm of imm_expr

and aexpr =
  | ALet of unique_id * cexpr * aexpr
  | ACExpr of cexpr

type global_scope_function = string * imm_expr list * aexpr

(* Smart constructors *)
(* imm_expr *)
let imm_int num = ImmInt num
let imm_string str = ImmString str
let imm_char sym = ImmChar sym
let imm_bool b = ImmBool b
let imm_unit = ImmUnit
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
(* let cmatch_with imm_expr case_list = CMatchWith (imm_expr, case_list) *)

(* aexpr *)
let alet id cexpr aexpr = ALet (id, cexpr, aexpr)
let acexpr cexpr = ACExpr cexpr
let acimm imm_expr = acexpr @@ cimm imm_expr
(* ------------------ *)

module State : sig
  type 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  val fresh : int t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a
end = struct
  type 'a t = int -> 'a * int

  let fresh last = last, last + 1

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let value, st = m state in
    (f value) st
  ;;

  let bind m f = m >>= f
  let return value st = value, st
  let ( let* ) = ( >>= )
  let run monad = fst @@ monad 0

  let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
    fun m f state ->
    let value, st = m state in
    f value, st
  ;;
end

open State

(* Runtime fuctions (unavailable to users)
   | name        | args         | id   |
   -------------------------------------
   | `head       | list         | -1   |
   | `tail       | list         | -2   |
   | `length     | list         | -3   |
   | `at_list    | list index   | -4   |
   | `at_tuple   | tuple index  | -4   |
   -------------------------------------*)

let rec rewrite_match = function
  | EBinaryOperation (bop, left, right) ->
    let left_rewritten = rewrite_match left in
    let right_rewritten = rewrite_match right in
    ebinary_operation bop left_rewritten right_rewritten
  | EUnaryOperation (uop, operand) ->
    let operand_rewritten = rewrite_match operand in
    eunary_operation uop operand_rewritten
  | EApplication (fun_expr, arg_expr) ->
    let fun_expr_rewritten = rewrite_match fun_expr in
    let arg_expr_rewritten = rewrite_match arg_expr in
    eapplication fun_expr_rewritten arg_expr_rewritten
  | EList expr_list -> elist @@ List.map (fun e -> rewrite_match e) expr_list
  | ETuple (first_elem, second_elem, other_elems) ->
    let first_elem_rewritten = rewrite_match first_elem in
    let second_elem_rewritten = rewrite_match second_elem in
    etuple first_elem_rewritten second_elem_rewritten
    @@ List.map (fun e -> rewrite_match e) other_elems
  | EIf (condition, true_branch, false_branch) ->
    let condition_rewritten = rewrite_match condition in
    let true_branch_rewritten = rewrite_match true_branch in
    let false_branch_rewritten = rewrite_match false_branch in
    eif condition_rewritten true_branch_rewritten false_branch_rewritten
  | EConstructList (operand, expr_list) ->
    let operand_rewritten = rewrite_match operand in
    let expr_list_rewritten = rewrite_match expr_list in
    econstruct_list operand_rewritten expr_list_rewritten
  | ELetIn (first_declaration, other_declarations, body) ->
    let get_constructor = function
      | DDeclaration _ -> ddeclaration
      | DRecursiveDeclaration _ -> drecursivedeclaration
    in
    let process_declaration = function
      | ( DDeclaration (name, pattern_list, expr)
        | DRecursiveDeclaration (name, pattern_list, expr) ) as original ->
        if Base.List.length pattern_list = 0
        then get_constructor original name pattern_list @@ rewrite_match expr
        else
          failwith
            "AST to ANF convertion error: internal let-expressions must not have any \
             args."
    in
    eletin
      (process_declaration first_declaration)
      (List.map process_declaration other_declarations)
      (rewrite_match body)
  | EMatchWith (matched_expression, first_case, other_cases) ->
    let cases = first_case :: other_cases in
    let rec get_match_condition matched_expression = function
      | PLiteral literal -> ebinary_operation Eq matched_expression (eliteral literal)
      | PWildcard -> eliteral @@ lbool true
      | PList pattern_list ->
        let pattern_list_length = List.length pattern_list in
        let partial_condition =
          eif
            (ebinary_operation
               NEq
               (eapplication (eidentifier "`length") matched_expression)
               (eliteral @@ lint pattern_list_length))
            (eliteral @@ lbool false)
        in
        let elems_condition =
          Base.List.fold_right
            ~init:(eliteral @@ lbool true, 0)
            pattern_list
            ~f:(fun pattern (cond, ind) ->
              let pattern_condition =
                get_match_condition
                  (eapplication
                     (eapplication (eidentifier "`at_list") matched_expression)
                     (eliteral @@ lint ind))
                  pattern
              in
              ebinary_operation AND cond pattern_condition, ind + 1)
        in
        partial_condition (fst elems_condition)
      | PTuple (first_pattern, second_pattern, pattern_list) ->
        let pattern_list = first_pattern :: second_pattern :: pattern_list in
        let pattern_list_length = List.length pattern_list in
        let elems_condition =
          Base.List.fold_right
            ~init:(eliteral @@ lbool true, 0)
            pattern_list
            ~f:(fun pattern (cond, ind) ->
              let pattern_condition =
                get_match_condition
                  (eapplication
                     (eapplication (eidentifier "`at_tuple") matched_expression)
                     (eliteral @@ lint ind))
                  pattern
              in
              ebinary_operation AND cond pattern_condition, ind + 1)
        in
        fst elems_condition
      | PConstructList (first_pattern, second_pattern) ->
        let partial_condition =
          eif
            (ebinary_operation
               NEq
               (eapplication (eidentifier "`length") matched_expression)
               (eliteral @@ lint pattern_list_length))
            (eliteral @@ lbool false)
        in
      | PIdentifier id -> failwith ""
    in
    failwith ""
  | EFun _ ->
    failwith
      "AST to ANF convertion error: EFun constructor cannot be used in the program after \
       closure conversion and lambda lifting."
  | e -> e
;;

(* let rec anf (env : (string, 'a, Base.String.comparator_witness) Base.Map.t) expr k
  : aexpr State.t
  =
  match expr with
  | ELiteral literal ->
    (match literal with
     | LInt num -> k (imm_int num)
     | LString str -> k (imm_string str)
     | LChar sym -> k (imm_char sym)
     | LBool b -> k (imm_bool b)
     | LUnit -> k imm_unit)
  | EIdentifier x -> k @@ imm_id (Base.Map.find_exn env x)
  | EBinaryOperation (bop, left, right) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    anf env left (fun limm ->
      anf env right (fun rimm ->
        return @@ alet fresh_var (cbinary_operation bop limm rimm) body))
  | EUnaryOperation (uop, expr) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    anf env expr (fun imm -> return @@ alet fresh_var (cunary_operation uop imm) body)
  | EApplication (fun_expr, arg_expr) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    anf env fun_expr (fun imm_fun_expr ->
      anf env arg_expr (fun imm_arg_expr ->
        return @@ alet fresh_var (capplication imm_fun_expr imm_arg_expr) body))
  | EList expr_list ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let rec helper curr_list = function
      | head :: tail -> anf env head (fun imm -> helper (imm :: curr_list) tail)
      | _ -> return @@ alet fresh_var (clist @@ Base.List.rev curr_list) body
    in
    helper [] expr_list
  | ETuple (first_elem, second_elem, other_elems) ->
    let all_elems = first_elem :: second_elem :: other_elems in
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let rec helper curr_list = function
      | head :: tail -> anf env head (fun imm -> helper (imm :: curr_list) tail)
      | _ -> return @@ alet fresh_var (ctuple @@ Base.List.rev curr_list) body
    in
    helper [] all_elems
  | EIf (condition, true_branch, false_branch) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    anf env condition (fun condition_imm ->
      anf env true_branch (fun true_branch_imm ->
        anf env false_branch (fun false_branch_imm ->
          return
          @@ alet fresh_var (cif condition_imm true_branch_imm false_branch_imm) body)))
  | EConstructList (operand, expr_list) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    anf env operand (fun operand_imm ->
      anf env expr_list (fun expr_list_imm ->
        return @@ alet fresh_var (cconstruct_list operand_imm expr_list_imm) body))
  | ELetIn (first_declaration, other_declarations, body) ->
    let* fresh_var = fresh in
    (match first_declaration with
     | DDeclaration (name, pattern_list, expr) ->
       if Base.List.length pattern_list = 0
       then (
         let new_env = Base.Map.set env ~key:name ~data:fresh_var in
         anf env expr (fun imm_expr ->
           match other_declarations with
           | head :: tail ->
             let* body = anf new_env (eletin head tail body) k in
             return @@ alet fresh_var (cimm imm_expr) body
           | _ ->
             let* body = anf new_env body k in
             return @@ alet fresh_var (cimm imm_expr) body))
       else
         failwith
           "AST to ANF convertion error: internal let-expressions must not have any args."
     | DRecursiveDeclaration (name, pattern_list, expr) ->
       if Base.List.length pattern_list = 0
       then (
         let env = Base.Map.set env ~key:name ~data:fresh_var in
         anf env expr (fun imm_expr ->
           match other_declarations with
           | head :: tail ->
             let* body = anf env (eletin head tail body) k in
             return @@ alet fresh_var (cimm imm_expr) body
           | _ ->
             let* body = anf env body k in
             return @@ alet fresh_var (cimm imm_expr) body))
       else
         failwith
           "AST to ANF convertion error: internal let-expressions must not have any args.")
  | EMatchWith (matched_expression, first_case, other_cases) ->
    (* let case_list = first_case :: other_cases in
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let update_map env patt =
      let identifiers = Util.find_identifiers_pattern patt in
      Base.Set.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
        let* fresh_var = fresh in
        let* acc = acc in
        return @@ Base.Map.set acc ~key:id ~data:fresh_var)
    in
    let rec helper imm_matched env curr_list = function
      | head :: tail ->
        let* new_env = update_map env (fst head) in
        let* case_anf = anf new_env (snd head) (fun imm -> return @@ acimm imm) in
        helper imm_matched env ((fst head, case_anf) :: curr_list) tail
      | _ ->
        return @@ alet fresh_var (cmatch_with imm_matched @@ Base.List.rev curr_list) body
    in
    anf env matched_expression (fun imm_matched -> helper imm_matched env [] case_list) *)
    let case_list = first_case :: other_cases in
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let update_map env patt =
      let identifiers = Util.find_identifiers_pattern patt in
      Base.Set.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
        let* fresh_var = fresh in
        let* acc = acc in
        return @@ Base.Map.set acc ~key:id ~data:fresh_var)
   in
    let rec helper imm_matched env curr_condition = function
      | head :: tail ->
        let* new_env = update_map env (fst head) in
        let* case_anf = anf new_env (snd head) (fun imm -> return @@ acimm imm) in
        helper imm_matched env ((fst head, case_anf) :: curr_list) tail
      | _ ->
        return @@ alet fresh_var (cmatch_with imm_matched @@ Base.List.rev curr_list) body
    in
    anf env matched_expression (fun imm_matched -> helper imm_matched env [] case_list)

  | EFun _ ->
    failwith
      "AST to ANF convertion error: EFun constructor cannot be used in the program after \
       closure conversion and lambda lifting."
;;

let process_declaration env declaration =
  let find_all_pattern_ids pattern_list =
    let rec helper acc = function
      | head :: tail -> helper (Base.Set.union acc (find_identifiers_pattern head)) tail
      | _ -> acc
    in
    helper empty pattern_list
  in
  let update_map env all_pattern_ids =
    Base.Set.fold_right all_pattern_ids ~init:(return env) ~f:(fun id acc ->
      let* fresh_var = fresh in
      let* acc = acc in
      return @@ Base.Map.set acc ~key:id ~data:fresh_var)
  in
  let get_function env pattern_list expr =
    let* env = update_map env @@ find_all_pattern_ids pattern_list in
    let* function_body_aexpr = anf env expr (fun imm -> return @@ acimm imm) in
    let rec helper current_aexpr = function
      | head :: tail ->
        FunctionWithArgs
          (fun imm -> helper (acexpr @@ cmatch_with imm [ head, current_aexpr ]) tail)
      | _ -> FunctionNoArgs current_aexpr
    in
    return @@ helper function_body_aexpr pattern_list
  in
  let* fresh_var = fresh in
  match declaration with
  | DDeclaration (name, pattern_list, expr) ->
    return @@ (name, fresh_var, get_function env pattern_list expr)
  | DRecursiveDeclaration (name, pattern_list, expr) ->
    let env = Base.Map.set env ~key:name ~data:fresh_var in
    return @@ (name, fresh_var, get_function env pattern_list expr)
;;

let anf_conversion (program : declaration list)
  : (string, global_scope_function, Base.String.comparator_witness) Base.Map.t State.t
  =
  let rec helper env current_map = function
    | head :: tail ->
      let* name, fresh_var, anf_function = process_declaration env head in
      let* anf_function = anf_function in
      let env = Base.Map.set env ~key:name ~data:fresh_var in
      let current_map = Base.Map.set current_map ~key:name ~data:anf_function in
      helper env current_map tail
    | _ -> return @@ current_map
  in
  helper
    (Base.Map.empty (module Base.String))
    (Base.Map.empty (module Base.String))
    program
;;

let run_anf_conversion program
  : (string, global_scope_function, Base.String.comparator_witness) Base.Map.t
  =
  run @@ anf_conversion program
;; *)
