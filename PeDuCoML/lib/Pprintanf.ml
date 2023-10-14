open Anf
open Format

let pp_immexpr fmt = function
  | ImmNum num -> fprintf fmt "%d" num
  | ImmString str -> fprintf fmt "\"%s\"" str
  | ImmId id -> fprintf fmt "i%d" id
;;

let pp_cexpr fmt = function
  | CBop (bop, left, right) ->
    fprintf
      fmt
      "%a %a %a"
      pp_immexpr
      left
      Pprintast.pp_binary_operator
      bop
      pp_immexpr
      right
  | CImmExpr immexpr -> pp_immexpr fmt immexpr
;;

let rec pp_aexpr fmt = function
  | ALet (id, cexpr, aexpr) ->
    fprintf fmt "let i%d = %a in\n%a\n" id pp_cexpr cexpr pp_aexpr aexpr
  | ACExpr cexpr -> pp_cexpr fmt cexpr
;;

let%expect_test _ =
  printf "%a" pp_immexpr @@ ImmNum 2;
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
  printf "%a" pp_cexpr @@ CBop (Add, ImmNum 2, ImmNum 3);
  [%expect {|
  2 + 3
  |}]
;;

let%expect_test _ =
  printf "%a" pp_cexpr @@ CBop (GT, ImmString "greater_than", ImmString "less_than");
  [%expect {|
  "greater_than" > "less_than"
  |}]
;;

let%expect_test _ =
  printf "%a" pp_aexpr
  @@ ALet (1, CBop (Mul, ImmNum 0, ImmNum 100), ACExpr (CImmExpr (ImmId 1)));
  [%expect {|
  let i1 = 0 * 100 in 
  i1
  |}]
;;

let%expect_test _ =
  printf "%a" pp_aexpr
  @@ ALet
       ( 1
       , CBop (Mul, ImmNum 0, ImmNum 100)
       , ALet (2, CBop (Sub, ImmId 1, ImmNum 10), ACExpr (CBop (Eq, ImmId 2, ImmId 1))) );
  [%expect {|
  let i1 = 0 * 100 in 
  let i2 = i1 - 10 in
  i2 = i1
  |}]
;;