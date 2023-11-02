open Ast
open Base

(** Finds all identifiers in an expression. Used in pattern-matching inference *)
let rec find_identifiers = function
  | EBinaryOperation (_, left_operand, right_operand) ->
    find_identifiers left_operand @ find_identifiers right_operand
  | EUnaryOperation (_, operand) -> find_identifiers operand
  | EApplication (left_operand, right_operand) ->
    find_identifiers left_operand @ find_identifiers right_operand
  | EIdentifier id -> [ id ]
  | EList expression_list ->
    List.fold_right
      ~f:(fun expression acc -> find_identifiers expression @ acc)
      ~init:[]
      expression_list
  | ETuple (first_elem, second_elem, other_elems) ->
    List.fold_right ~f:(fun expression acc -> find_identifiers expression @ acc) ~init:[]
    @@ (first_elem :: second_elem :: other_elems)
  | EConstructList (operand, list) -> find_identifiers operand @ find_identifiers list
  | _ -> []
;;

let find_identifiers_pattern =
  let rec helper arr = function
    | PIdentifier id -> id :: arr
    | PList pattern_list ->
      (match pattern_list with
       | head :: tail -> helper (helper [] head @ arr) (PList tail)
       | _ -> [])
    | PTuple (first_pattern, second_pattern, other_patterns) ->
      helper arr @@ plist (first_pattern :: second_pattern :: other_patterns)
    | PConstructList (operand, list) -> helper (helper [] operand) list
    | _ -> []
  in
  helper []
;;
