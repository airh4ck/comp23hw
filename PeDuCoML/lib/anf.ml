open Ast

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
   | `at_tuple   | tuple index  | -5   |
   -------------------------------------*)

let rec rewrite_match expr =
  let rt_head = eapplication (eidentifier "`head") in
  let rt_tail = eapplication (eidentifier "`tail") in
  let rt_length = eapplication (eidentifier "`length") in
  let rt_at_list lst idx =
    eapplication (eapplication (eidentifier "`at_list") lst) (eliteral @@ lint idx)
  in
  let rt_at_tuple tuple idx =
    eapplication (eapplication (eidentifier "`at_tuple") tuple) (eliteral @@ lint idx)
  in
  match expr with
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
      | PList pattern_list ->
        let pattern_list_length = List.length pattern_list in
        let partial_condition =
          eif
            (ebinary_operation
               NEq
               (rt_length matched_expression)
               (eliteral @@ lint pattern_list_length))
            (eliteral @@ lbool false)
        in
        let elems_condition =
          Base.List.fold_left
            ~init:(eliteral @@ lbool true, 0)
            pattern_list
            ~f:(fun (cond, ind) pattern ->
              let pattern_condition =
                get_match_condition (rt_at_list matched_expression ind) pattern
              in
              ebinary_operation AND cond pattern_condition, ind + 1)
        in
        partial_condition (fst elems_condition)
      | PTuple (first_pattern, second_pattern, pattern_list) ->
        let pattern_list = first_pattern :: second_pattern :: pattern_list in
        let elems_condition =
          Base.List.fold
            ~init:(eliteral @@ lbool true, 0)
            pattern_list
            ~f:(fun (cond, ind) pattern ->
              let pattern_condition =
                get_match_condition (rt_at_tuple matched_expression ind) pattern
              in
              ebinary_operation AND cond pattern_condition, ind + 1)
        in
        fst elems_condition
      | PConstructList (head_pattern, tail_pattern) ->
        let partial_condition =
          eif
            (ebinary_operation GT (rt_length matched_expression) (eliteral @@ lint 0))
            (eliteral @@ lbool false)
        in
        let head_condition =
          get_match_condition (rt_head matched_expression) head_pattern
        in
        let tail_condition =
          get_match_condition (rt_tail matched_expression) tail_pattern
        in
        partial_condition @@ ebinary_operation AND head_condition tail_condition
      | _ -> eliteral @@ lbool true
    in
    let rec rewrite_pattern matched pattern action =
      match pattern with
      | PIdentifier id -> eletin (ddeclaration id [] matched) [] action
      | PTuple (first_elem, second_elem, other_elems) ->
        let elem_list = first_elem :: second_elem :: other_elems in
        let rewritten, _ =
          Base.List.fold elem_list ~init:(action, 0) ~f:(fun (action, ind) elem ->
            rewrite_pattern (rt_at_tuple matched ind) elem action, ind + 1)
        in
        rewritten
      | PList pattern_list ->
        let rewritten, _ =
          Base.List.fold pattern_list ~init:(action, 0) ~f:(fun (action, ind) elem ->
            rewrite_pattern (rt_at_list matched ind) elem action, ind + 1)
        in
        rewritten
      | PConstructList (head, tail) ->
        rewrite_pattern (rt_head matched) head
        @@ rewrite_pattern (rt_tail matched) tail action
      | _ -> action
    in
    let rec gen_conditions k cases =
      let pattern, action = Base.List.hd_exn cases in
      match Base.List.tl_exn cases with
      | [] -> k @@ rewrite_pattern matched_expression pattern action
      | tail ->
        gen_conditions
          (fun else_branch ->
            k
            @@ eif
                 (get_match_condition matched_expression pattern)
                 (rewrite_pattern matched_expression pattern action)
                 else_branch)
          tail
    in
    gen_conditions (fun id -> id) cases
  | EFun _ ->
    failwith
      "AST to ANF convertion error: EFun constructor cannot be used in the program after \
       closure conversion and lambda lifting."
  | e -> e
;;

let rec anf (env : (string, unique_id, Base.String.comparator_witness) Base.Map.t) expr k
  : aexpr State.t
  =
  let expr = rewrite_match expr in
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
  | EMatchWith _ ->
    failwith
      "AST to ANF convertion error: EMatchWith constructor cannot be used in the program \
       after match expressions rewriting."
  | EFun _ ->
    failwith
      "AST to ANF convertion error: EFun constructor cannot be used in the program after \
       closure conversion and lambda lifting."
;;

let process_declaration env declaration =
  let rec helper current_expr current_arg_number args_list = function
    | head :: tail ->
      let arg_name = "``" ^ string_of_int current_arg_number in
      helper
        (ematchwith (eidentifier arg_name) (head, current_expr) [])
        (current_arg_number + 1)
        (arg_name :: args_list)
        tail
    | _ -> args_list, current_expr
  in
  let update_map env args_list =
    Base.List.fold_right args_list ~init:(return env) ~f:(fun id acc ->
      let* fresh_var = fresh in
      let* acc = acc in
      return @@ Base.Map.set acc ~key:id ~data:fresh_var)
  in
  let gen_imm_id env name = imm_id @@ Base.Map.find_exn env name in
  let gen_global_scope_function env name pattern_list expr =
    let args_list, expr = helper expr 1 [] pattern_list in
    let* env = update_map env args_list in
    let* anf_representation = anf env expr (fun imm -> return @@ acimm imm) in
    return (name, List.map (gen_imm_id env) args_list, anf_representation)
  in
  let* fresh_var = fresh in
  match declaration with
  | DDeclaration (name, pattern_list, expr) ->
    let* global_scope_f = gen_global_scope_function env name pattern_list expr in
    return (global_scope_f, fresh_var)
  | DRecursiveDeclaration (name, pattern_list, expr) ->
    let env = Base.Map.set env ~key:name ~data:fresh_var in
    let* global_scope_f = gen_global_scope_function env name pattern_list expr in
    return (global_scope_f, fresh_var)
;;

let anf_conversion (program : declaration list)
  : (string, global_scope_function, Base.String.comparator_witness) Base.Map.t State.t
  =
  let rec helper env current_map = function
    | head :: tail ->
      let* ((name, _, _) as global_scope_f), fresh_var = process_declaration env head in
      let env = Base.Map.set env ~key:name ~data:fresh_var in
      let current_map = Base.Map.set current_map ~key:name ~data:global_scope_f in
      helper env current_map tail
    | _ -> return @@ current_map
  in
  let env = Base.Map.empty (module Base.String) in
  let env = Base.Map.set env ~key:"`head" ~data:(-1) in
  let env = Base.Map.set env ~key:"`tail" ~data:(-2) in
  let env = Base.Map.set env ~key:"`length" ~data:(-3) in
  let env = Base.Map.set env ~key:"`at_list" ~data:(-4) in
  let env = Base.Map.set env ~key:"`at_tuple" ~data:(-5) in
  helper env (Base.Map.empty (module Base.String)) program
;;

let run_anf_conversion program
  : (string, global_scope_function, Base.String.comparator_witness) Base.Map.t
  =
  run @@ anf_conversion program
;;
