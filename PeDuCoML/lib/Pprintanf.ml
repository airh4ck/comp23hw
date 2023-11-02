open Anf
open Format

let pp_immexpr fmt = function
  | ImmInt num -> fprintf fmt "%d" num
  | ImmString str -> fprintf fmt "\"%s\"" str
  | ImmChar smb -> fprintf fmt "'%c'" smb
  | ImmBool boolean -> fprintf fmt (if boolean then "true" else "false")
  | ImmUnit -> fprintf fmt "()"
  (* | ImmWildcard -> fprintf fmt "_" *)
  | ImmId id -> fprintf fmt "i%d" id
;;

let rec pp_cexpr fmt =
  let pp_list pp fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp fmt value)
      fmt
  in
  function
  | CBinaryOperation (bop, left, right) ->
    fprintf
      fmt
      "%a %a %a"
      pp_immexpr
      left
      Pprintast.pp_binary_operator
      bop
      pp_immexpr
      right
  | CUnaryOperation (unop, imm) ->
    fprintf fmt "%a%a" Ast.pp_unary_operator unop pp_immexpr imm
  | CApplication (fun_imm, arg_imm) ->
    fprintf fmt "%a %a" pp_immexpr fun_imm pp_immexpr arg_imm
  | CList imm_list -> fprintf fmt "[%a]" (fun fmt -> pp_list pp_immexpr fmt "; ") imm_list
  | CTuple imm_list ->
    fprintf fmt "(%a)" (fun fmt -> pp_list pp_immexpr fmt ", ") imm_list
  | CIf (condition, true_branch, false_branch) ->
    fprintf
      fmt
      "if %a then %a else %a"
      pp_immexpr
      condition
      pp_immexpr
      true_branch
      pp_immexpr
      false_branch
  | CConstructList (head, tail) -> fprintf fmt "%a :: %a" pp_immexpr head pp_immexpr tail
  | CMatchWith (matched, case_list) ->
    fprintf fmt "match %a with" pp_immexpr matched;
    List.iter
      (fun (case, action) ->
        fprintf fmt " | %a -> %a" Pprintast.pp_pattern case pp_aexpr action)
      case_list
  | CImm immexpr -> pp_immexpr fmt immexpr

and pp_aexpr fmt = function
  | ALet (id, cexpr, aexpr) ->
    fprintf fmt "let i%d = %a in\n%a" id pp_cexpr cexpr pp_aexpr aexpr
  | ACExpr cexpr -> pp_cexpr fmt cexpr
;;

let pp_global_scope_function =
  let id = ref 0 in
  let rec helper fmt = function
    | FunctionNoArgs aexpr -> Format.fprintf fmt "%a\n" pp_aexpr aexpr
    | FunctionWithArgs func ->
      Format.fprintf fmt "%a\n" helper (func (imm_id !id));
      id := !id + 1
  in
  helper
;;

let%expect_test _ =
  printf "%a" pp_immexpr @@ ImmInt 2;
  [%expect {|
  2
  |}]
;;

let%expect_test _ =
  printf "%a" pp_immexpr @@ ImmString "abacaba";
  [%expect {|
  "abacaba"
  |}]
;;

(* TODO: need to display id's differently *)
let%expect_test _ =
  printf "%a" pp_immexpr @@ ImmId 1;
  [%expect {|
  i1
  |}]
;;

let%expect_test _ =
  printf "%a" pp_cexpr @@ CBinaryOperation (Add, ImmInt 2, ImmInt 3);
  [%expect {|
  2 + 3
  |}]
;;

let%expect_test _ =
  printf "%a" pp_cexpr
  @@ CBinaryOperation (GT, ImmString "greater_than", ImmString "less_than");
  [%expect {|
  "greater_than" > "less_than"
  |}]
;;

let%expect_test _ =
  printf "%a" pp_aexpr
  @@ ALet (1, CBinaryOperation (Mul, ImmInt 0, ImmInt 100), ACExpr (CImm (ImmId 1)));
  [%expect {|
  let i1 = 0 * 100 in 
  i1
  |}]
;;

let%expect_test _ =
  printf "%a" pp_aexpr
  @@ ALet
       ( 1
       , CBinaryOperation (Mul, ImmInt 0, ImmInt 100)
       , ALet
           ( 2
           , CBinaryOperation (Sub, ImmId 1, ImmInt 10)
           , ACExpr (CBinaryOperation (Eq, ImmId 2, ImmId 1)) ) );
  [%expect {|
  let i1 = 0 * 100 in 
  let i2 = i1 - 10 in
  i2 = i1
  |}]
;;