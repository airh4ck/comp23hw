open Ast

type unique_id = int [@@deriving eq, show { with_path = false }]

type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmChar of char
  | ImmBool of bool
  | ImmUnit
  | ImmWildcard
  | ImmId of unique_id

type cexpr =
  | CBinaryOperation of binary_operator * imm_expr * imm_expr
  | CUnaryOperation of unary_operator * imm_expr
  | CApplication of imm_expr * imm_expr
  | CList of imm_expr list
  | CTuple of imm_expr list
  | CIf of imm_expr * imm_expr * imm_expr
  | CConstructList of imm_expr * imm_expr
  | CFun of pattern list * imm_expr
  | CMatchWith of imm_expr * (pattern * imm_expr) list
  | CImm of imm_expr

and cpatt =
  | CPImm of cexpr
  | CPWildcard
  | CPTuple of cpatt list
  | CPList of pattern list
  | CPConstructList of pattern * pattern

type aexpr =
  | ALet of unique_id * cexpr * aexpr
  | ACExpr of cexpr

type let_fun =
  | FunctionCarry of cexpr * let_fun
  | Function of cexpr * cexpr

(* Smart constructors *)
(* imm_expr *)
let imm_int num = ImmInt num
let imm_string str = ImmString str
let imm_char sym = ImmChar sym
let imm_bool b = ImmBool b
let imm_unit = ImmUnit
let imm_wildcard = ImmWildcard
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
let cfun imm_expr_list imm_expr = CFun (imm_expr_list, imm_expr)
let cmatch_with imm_expr case_list = CMatchWith (imm_expr, case_list)

(* aexpr *)
let alet id cexpr aexpr = ALet (id, cexpr, aexpr)
let acexpr cexpr = ACExpr cexpr
let acimm imm_expr = acexpr @@ cimm imm_expr
(* ------------------ *)

module AnfConvert : sig
  type 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  val fresh : int t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a
end = struct
  type 'a t = int -> 'a * int

  let fresh last = last + 1, last

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

open AnfConvert

let rec expression_anf
  (env : (string, 'a, Base.String.comparator_witness) Base.Map.t)
  expr
  k
  : aexpr AnfConvert.t
  =
  match expr with
  | ELiteral literal ->
    (match literal with
     | LInt num -> k (imm_int num)
     | LString str -> k (imm_string str)
     | LChar sym -> k (imm_char sym)
     | LBool b -> k (imm_bool b)
     | LUnit -> k imm_unit)
  | EIdentifier x ->
    (match Base.Map.find env x with
     | Some id -> k @@ imm_id id
     | _ -> failwith "Not reachable. If it is, kill Danya and Ilya")
  | EBinaryOperation (bop, left, right) ->
    let* fresh_var = fresh in
    expression_anf env left (fun limm ->
      expression_anf env right (fun rimm ->
        let* body = k @@ imm_id fresh_var in
        return @@ alet fresh_var (cbinary_operation bop limm rimm) body))
  | EUnaryOperation (uop, expr) ->
    let* fresh_var = fresh in
    expression_anf env expr (fun imm ->
      let* body = k @@ imm_id fresh_var in
      return @@ alet fresh_var (cunary_operation uop imm) body)
  | EApplication (fun_expr, arg_expr) ->
    let* fresh_var = fresh in
    expression_anf env fun_expr (fun imm_fun_expr ->
      expression_anf env arg_expr (fun imm_arg_expr ->
        let* body = k @@ imm_id fresh_var in
        return @@ alet fresh_var (capplication imm_fun_expr imm_arg_expr) body))
  | EList expr_list ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let rec helper curr_list = function
      | head :: tail ->
        expression_anf env head (fun imm -> helper (imm :: curr_list) tail)
      | _ -> return @@ alet fresh_var (clist @@ List.rev curr_list) body
    in
    helper [] expr_list
  | ETuple expr_list ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let rec helper curr_list = function
      | head :: tail ->
        expression_anf env head (fun imm -> helper (imm :: curr_list) tail)
      | _ -> return @@ alet fresh_var (ctuple @@ List.rev curr_list) body
    in
    helper [] expr_list
  | EIf (condition, true_branch, false_branch) ->
    let* fresh_var = fresh in
    expression_anf env condition (fun condition_imm ->
      expression_anf env true_branch (fun true_branch_imm ->
        expression_anf env false_branch (fun false_branch_imm ->
          let* body = k @@ imm_id fresh_var in
          return
          @@ alet fresh_var (cif condition_imm true_branch_imm false_branch_imm) body)))
  | EConstructList (operand, expr_list) ->
    let* fresh_var = fresh in
    expression_anf env operand (fun operand_imm ->
      expression_anf env expr_list (fun expr_list_imm ->
        let* body = k @@ imm_id fresh_var in
        return @@ alet fresh_var (cconstruct_list operand_imm expr_list_imm) body))
  | ELetIn (bindings_list, expr) ->
    let* fresh_var = fresh in
    (match bindings_list with
     | ( DDeclaration (name, pattern_list, expr)
       | DRecursiveDeclaration (name, pattern_list, expr) )
       :: tail ->
       let env = Base.Map.update env name ~f:(fun _ -> fresh_var) in
       expression_anf env (efun pattern_list expr) (fun imm ->
         let* body = expression_anf env (eletin tail expr) k in
         return @@ alet fresh_var (cimm imm) body)
     | _ -> expression_anf env expr k)
  | EFun (arguments, expr) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let update_map env patt =
      let identifiers = Util.find_identifiers_pattern patt in
      Base.List.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
        let* fresh_var = fresh in
        let* acc = acc in
        return @@ Base.Map.update acc id ~f:(fun _ -> fresh_var))
    in
    let* env =
      Base.List.fold_right arguments ~init:(return env) ~f:(fun patt acc ->
        let* acc = acc in
        update_map acc patt)
    in
    expression_anf env expr (fun imm ->
      return @@ alet fresh_var (cfun arguments imm) body)
  | EMatchWith (matched_expression, case_list) ->
    let* fresh_var = fresh in
    let* body = k @@ imm_id fresh_var in
    let update_map env patt =
      let identifiers = Util.find_identifiers_pattern patt in
      Base.List.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
        let* fresh_var = fresh in
        let* acc = acc in
        return @@ Base.Map.update acc id ~f:(fun _ -> fresh_var))
    in
    let rec helper imm_matched env curr_list = function
      | head :: tail ->
        let* new_env = update_map env (fst head) in
        expression_anf new_env (snd head) (fun imm ->
          helper imm_matched env ((fst head, imm) :: curr_list) tail)
      | _ -> return @@ alet fresh_var (cmatch_with imm_matched @@ List.rev curr_list) body
    in
    expression_anf env matched_expression (fun imm_matched ->
      helper imm_matched env [] case_list)
;;

let declaration_anf env = function
  | DDeclaration (name, pattern_list, expr)
  | DRecursiveDeclaration (name, pattern_list, expr) ->
    let* fresh_var = fresh in
    let env = Base.Map.set env ~key:name ~data:fresh_var in
    expression_anf env (efun pattern_list expr) (fun imm -> return @@ acimm imm)
;;

let convert (program : declaration list) =
  let rec helper env = function
    | head :: tail ->
      let* head_declaration = declaration_anf env head in
      let name =
        match head with
        | DDeclaration (name, _, _) | DRecursiveDeclaration (name, _, _) -> name
      in
      let* fresh_var = fresh in
      let* tail_declarations = helper (Base.Map.set env ~key:name ~data:fresh_var) tail in
      return @@ (head_declaration :: tail_declarations)
    | _ -> return []
  in
  helper (Base.Map.empty (module Base.String)) program
;;

let run_convert program = run @@ convert program
(*
let%test _ =
  run_convert @@ EBinaryOperation (Div, ELiteral (LInt 4), ELiteral (LInt 2))
  = ALet (1, CBinaryOperation (Div, ImmInt 4, ImmInt 2), ACExpr (CImm (ImmId 1)))
;;

let%test _ =
  run_convert
  @@ EBinaryOperation
       (Add, EIdentifier "x", EBinaryOperation (Sub, EIdentifier "x", EIdentifier "y"))
  = ALet (1, CBinaryOperation (Div, ImmInt 4, ImmInt 2), ACExpr (CImm (ImmId 1)))
;; *)