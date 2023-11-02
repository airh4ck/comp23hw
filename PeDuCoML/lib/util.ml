open Ast
open Base.Set

let empty = empty (module Base.String)
let singleton = singleton (module Base.String)

(** Finds all identifiers in an expression. Used in pattern-matching inference *)
let rec find_identifiers = function
  | EBinaryOperation (_, left_operand, right_operand) ->
    union (find_identifiers left_operand) (find_identifiers right_operand)
  | EUnaryOperation (_, operand) -> find_identifiers operand
  | EApplication (left_operand, right_operand) ->
    union (find_identifiers left_operand) (find_identifiers right_operand)
  | EIdentifier id -> singleton id
  | EList expression_list ->
    Base.List.fold_right
      ~f:(fun expression acc -> union (find_identifiers expression) acc)
      ~init:empty
      expression_list
  | ETuple (first_elem, second_elem, other_elems) ->
    Base.List.fold_right
      ~f:(fun expression acc -> union (find_identifiers expression) acc)
      ~init:empty
    @@ (first_elem :: second_elem :: other_elems)
  | EConstructList (operand, list) ->
    union (find_identifiers operand) (find_identifiers list)
  | _ -> empty
;;

let find_identifiers_pattern =
  let rec helper acc = function
    | PIdentifier id -> add acc id
    | PList pattern_list ->
      (match pattern_list with
       | head :: tail -> helper (helper acc head) (PList tail)
       | _ -> acc)
    | PTuple (first_pattern, second_pattern, other_patterns) ->
      helper acc @@ plist (first_pattern :: second_pattern :: other_patterns)
    | PConstructList (operand, list) -> helper (helper acc operand) list
    | _ -> acc
  in
  helper empty
;;
