$ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli -load ../../runtime/peducoml_runtime.so llvm_test.ll
> let main = print_int 42
> EOF
declare i64 @compare_tuples_lte(i64, i64)
declare i64 @compare_tuples_lt(i64, i64)
declare i64 @compare_tuples_gte(i64, i64)
declare i64 @compare_tuples_gt(i64, i64)
declare i64 @compare_tuples_neq(i64, i64)
declare i64 @compare_tuples_eq(i64, i64)
declare i64 @compare_lists_lte(i64, i64)
declare i64 @compare_lists_lt(i64, i64)
declare i64 @compare_lists_gte(i64, i64)
declare i64 @compare_lists_gt(i64, i64)
declare i64 @compare_lists_neq(i64, i64)
declare i64 @compare_lists_eq(i64, i64)
declare i64 @print_new_line()
declare i64 @print_tuple(i64)
declare i64 @print_list(i64)
declare i64 @print_bool(i64)
declare i64 @print_char(i64)
declare i64 @print_int(i64)
declare i64 @peducoml_tuple_field(i64, i64)
declare i64 @peducoml_fill_tuple(i64, i64)
declare i64 @peducoml_alloc_tuple(i64)
declare i64 @peducoml_length(i64)
declare i64 @peducoml_tail(i64)
declare i64 @peducoml_list_field(i64, i64)
declare i64 @peducoml_add_to_list(i64, i64)
declare i64 @peducoml_alloc_list()
declare i64 @peducoml_apply0(i64)
declare i64 @peducoml_apply(i64, i64)
declare i64 @peducoml_alloc_closure(i64, i64)
define i64 @main() {
entry:
  %"0" = alloca i64, align 8
  %peducoml_alloc_closure_n = call i64 @peducoml_alloc_closure(i64 ptrtoint (ptr @print_int to i64), i64 1)
  %peducoml_apply_n = call i64 @peducoml_apply(i64 %peducoml_alloc_closure_n, i64 42)
  store i64 %peducoml_apply_n, ptr %"0", align 4
  %"01" = load i64, ptr %"0", align 4
  ret i64 %"01"
}
---
42
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_char 'c'
> EOF
c
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_int (((13 + 52 - 7) * 3 - 6) / 4)
> EOF
42
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main =
>   let x = 1 in
>   let y = 1 in
>   print_bool (x = y)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_list [1; 2; 3; 4; 5]
> EOF
[1; 2; 3; 4; 5]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_tuple (10, 5, false, true)
> EOF
(10, 5, 0, 1)
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = 
>   let x = 1 in
>   let y = 1 in
>   print_bool (x <> y)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = 
>   let x = 1 in
>   let y = 42 in
>   print_bool (x < y)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = 
>   let x = 42 in
>   let y = 1 in
>   print_bool (x <= y)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = 
>   let x = 1 in
>   let y = 42 in
>   print_bool (x > y)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = 
>   let x = 42 in
>   let y = 42 in
>   print_bool (x >= y)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = 
>   let x = true in
>   let y = false in
>   print_bool (x && y)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = 
>   let x = true in
>   let y = false in
>   print_bool (x || y)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let id x = x
> let main = print_int (id 42)
> EOF
42
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let add x y = x + y
> let main = print_int (add 12 32)
> EOF
44
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let add_and_mul x y z = (x + y) * z
> let main = print_int (add_and_mul 1 3 5)
> EOF
20
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let add x y = x + y
> let partial_add x = add x
> let main = print_int (partial_add 1 2)
> EOF
3
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
> let main = print_int (factorial 6)
> EOF
720
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let is_greater_than_5 x = if x > 5 then true else false
> let main = print_bool (is_greater_than_5 6)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let is_greater_than_5 x = if x > 5 then true else false
> let main = print_bool (is_greater_than_5 2)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec filter predicate list =
>   match list with
>     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
>     | _ -> []
> 
> let main = print_list (filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63])
> EOF
[12; 3; 0; 1; 2]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec add_1_to_list list = 
>   match list with
>     | head :: tail -> (head + 1) :: add_1_to_list tail
>     | _ -> []
> 
> let main = print_list (add_1_to_list [1; 2; 3; 4; 5])
> EOF
[2; 3; 4; 5; 6]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_list ((5 + 4) :: 3 :: [1; 6; 0])
> EOF
[9; 3; 1; 6; 0]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_list []
> EOF
[]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let list1 = [1; 2; 3]
> let list2 = [2]
> let main = print_bool (compare_lists_lt list1 list2)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_bool (compare_lists_eq [1; 2; 3] [1; 2; 3])
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let list1 = [1; 2]
> let list2 = [1; 2; 3]
> let main = print_bool (compare_lists_gt list1 list2)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_bool (compare_lists_lte [true; true] [true; true])
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let list1 = ['a'; 'a'; 'b']
> let list2 = ['a'; 'a'; 'a']
> let main = print_bool (compare_lists_gte list1 list2)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_bool (compare_lists_neq [] [])
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let tuple1 = (1, 2, true, 'a')
> let tuple2 = (1, 2, true, 'a')
> let main = print_bool (compare_tuples_eq tuple1 tuple2)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_bool (compare_tuples_lte (1, 2, true, 'a') (1, 2, true, 'a'))
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let tuple1 = (1, 1, true, 'a')
> let tuple2 = (1, 2, true, 'a')
> let main = print_bool (compare_tuples_gt tuple1 tuple2)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_bool (compare_tuples_gte ('a', 'b') ('b', 'b'))
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let tuple1 = (1, 2, true, 'a')
> let tuple2 = (1, 1, true, 'a')
> let main = print_bool (compare_tuples_lt tuple1 tuple2)
> EOF
false
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_bool (compare_tuples_neq (1, 8) (1, 7))
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_tuple (10, 20, 888, 60)
> EOF
(10, 20, 888, 60)
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let list_to_print = [0; 10; 100]
> let main = print_list list_to_print
> EOF
[0; 10; 100]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let number_to_print = 5
> let main = print_int number_to_print
> EOF
5
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let bool_to_print = true
> let main = print_bool bool_to_print
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let main = print_new_line
> EOF

$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec filter predicate list =
>   match list with
>     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
>     | _ -> []
> 
> let main = print_list (filter (fun v -> v > 10) [1;2;3])
> EOF
[]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let phi n = let rec helper last1 last2 n = if n > 0 then helper last2 (last1 + last2) (n - 1) else last2 in helper 1 1 (n - 2)
> 
> let main = print_int (phi 10)
> EOF
55
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let product list =
>   let rec helper list acc = match list with
>     | head :: tail -> helper tail (acc * head)
>     | _ -> acc
>   in
>   helper list 1
> 
> let main = print_int (product [1; 2; 7; 12; 10; 3; 21])
> EOF
105840
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let sum list =
>   let rec helper list acc = match list with
>     | head :: tail -> helper tail (acc + head)
>     | _ -> acc
>   in
>   helper list 0
> 
> let main = print_int (sum [1; 2; 7; 12; 10; 3; 21; 101; 78; 42; 38])
> EOF
315
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let length list =
>   let rec helper list acc = match list with
>     | _ :: tail -> helper tail (acc + 1)
>     | _ -> acc
>   in
>   helper list 0
> 
> let main = print_int (length [1; 23; 12; 657; 123; 346; 6; 234 ; 99; 34; 78; 28; 123; 0])
> EOF
14
$ ./llvm_test.exe <<- EOF 
> let filter predicate list =
>   match list with
>     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
>     | _ -> []
> 
> let main = print_list (filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63])
> EOF
No such variable: filter
$ ./llvm_test.exe <<- EOF
> let main = f 1 2
> EOF
No such variable: f
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let count_solutions_of_sq_equation a b c =
>   let sq x = x * x
>   in
>   let d = sq b - 4 * a * c
>   in
>   if d > 0 then 2 else (if d = 0 then 1 else 0)
> 
> let main = print_int (count_solutions_of_sq_equation 2 9 4)
> EOF
2
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let count_solutions_of_sq_equation a b c =
>   let sq x = x * x
>   in
>   let d = sq b - 4 * a * c
>   in
>   if d > 0 then 2 else (if d = 0 then 1 else 0)
> 
> let main = print_int (count_solutions_of_sq_equation 3 6 3)
> EOF
1
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let count_solutions_of_sq_equation a b c =
>   let sq x = x * x
>   in
>   let d = sq b - 4 * a * c
>   in
>   if d > 0 then 2 else (if d = 0 then 1 else 0)
> 
> let main = print_int (count_solutions_of_sq_equation 1 2 3)
> EOF
0
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec map f list = match list with
>   | head :: tail -> f head :: map f tail
>   | _ -> []
> 
> let sq = fun x -> x * x
> 
> let main = print_list (map sq [1;2;3;4;5;6;7;8;9;10])
> EOF
[1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let f x y z =
>   match x, y, z with
>     | true, true, true -> true
>     | false, false, false -> true
>     | _ -> false
> 
> let main = print_bool (f (10 * 5 > 49) (58 / 2 = 27) (10 <> 20))
> EOF
false
$ ./llvm_test.exe <<- EOF
> let main = "abc" + "def"
> EOF
Unification failed: type of the expression is string but expected type was int
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let pifagor_check = fun x y z -> x * x + y * y = z * z
> 
> let main = print_bool (pifagor_check 3 4 5)
> EOF
true
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let check_password password = 
>   match password with
>     | "qwerty123" -> "success"
>     | _ -> "FAIL"
> 
> let main = print_string (check_password "qwerty")
> EOF
FAIL
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let check_password password = 
>   match password with
>     | "qwerty123" -> "Success"
>     | _ -> "FAIL"
> 
> let main = print_string (check_password "qwerty123")
> EOF
Success
  $ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 2
  > 
  > let main = print_int ((if idx = 1 then fst else snd) (13, 225))
  > EOF
  225
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let fst pair =
>   match pair with (x, _) -> x
> 
> let snd pair =
>   match pair with (_, y) -> y
> 
> let idx = 1
> 
> let main = (if idx = 1 then fst else snd) (13, 225)
> EOF
13
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let fst pair =
>   match pair with (x, _) -> x
> 
> let snd pair =
>   match pair with (_, y) -> y
> 
> let idx = 1
> 
> let main = (if idx = 1 then fst else snd) (13, 45, 89)
> EOF
Unification failed: type of the expression is int * int * int but expected type was 'p * 'p
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec matrix_sum m1 m2 =
>   let rec lines_sum l1 l2 =
>     match l1, l2 with
>       | h1 :: t1, h2 :: t2 -> (h1 + h2) :: lines_sum t1 t2
>       | _, _ -> []
>   in
>   match m1, m2 with
>     | h1 :: t1, h2 :: t2 -> lines_sum h1 h2 :: matrix_sum t1 t2
>     | _, _ -> []
> 
> let matrix1 = [[1;  5;  7 ];
>                [13; 32; 56];
>                [45; 2;  17]]
> 
> let matrix2 = [[4;  29;  0];
>                [79; 12; 66];
>                [8;  88; 19]]
> 
> let main = matrix_sum matrix1 matrix2
> EOF
[[5; 34; 7]; [92; 44; 122]; [53; 90; 36]]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec matrix_mult_number matrix number =
>   let rec line_mult_number line =
>     match line with
>       | head :: tail -> (head * number) :: line_mult_number tail
>       | _ -> []
>   in
>   match matrix with
>     | head :: tail -> line_mult_number head :: matrix_mult_number tail number
>     | _ -> []
> 
> let matrix = [[1;  5;  7 ];
>               [13; 32; 56];
>               [45; 2;  17]]
> 
> let main = matrix_mult_number matrix 5
> EOF
[[5; 25; 35]; [65; 160; 280]; [225; 10; 85]]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let rec matrix_mult_number matrix number =
>   let rec line_mult_number line =
>     match line with
>       | head :: tail -> (head * number) :: line_mult_number tail
>       | _ -> []
>   in
>   match matrix with
>     | head :: tail -> line_mult_number head :: matrix_mult_number tail number
>     | _ -> []
> 
> let rec matrix_sum m1 m2 =
>   let rec lines_sum l1 l2 =
>     match l1, l2 with
>       | h1 :: t1, h2 :: t2 -> (h1 + h2) :: lines_sum t1 t2
>       | _, _ -> []
>   in
>   match m1, m2 with
>     | h1 :: t1, h2 :: t2 -> lines_sum h1 h2 :: matrix_sum t1 t2
>     | _, _ -> []
> 
> let matrix1 = [[1;  5;  7 ];
>                [13; 32; 56];
>                [45; 2;  17]]
> 
> let matrix2 = [[4;  29;  0];
>                [79; 12; 66];
>                [8;  88; 19]]
> 
> let main = matrix_sum (matrix_mult_number matrix1 2) (matrix_mult_number matrix2 7)
> EOF
[[30; 213; 14]; [579; 148; 574]; [146; 620; 167]]
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> let int_list = [1; 2; 3]
> 
> let main = "0" :: int_list
> EOF
Unification failed: type of the expression is string but expected type was int
$ ./llvm_test.exe <<- EOF | lli -load ../../runtime/peducoml_runtime.so
> effect Failure : string -> int effect
> 
> let binary_int_of_str n = match n with
>   | "0" -> 0
>   | "1" -> 1
>   | s -> perform (Failure s)
> 
> let rec sum_up list = match list with
>   | [] -> 0
>   | s :: ss -> binary_int_of_str s + sum_up ss
> 
> let test_list = ["0"; "hope"; "1"; "it"; "0"; "works"; "1"]
> 
> let main = match sum_up test_list with
>   | effect (Failure _) -> continue 0
>   | res -> res
> EOF
2
$ ./demo.exe <<-EOF
> effect E: int -> int effect
> 
> let helper x = match perform (E x) with
>    | effect (E s) -> continue (s*s)
>    | l -> l
> 
> let main = match perform (E 5) with
>    | effect (E s) -> continue (s*s)
>    | l -> helper l
> EOF
625
$ ./demo.exe <<-EOF
> effect EmptyListException : int effect
> 
> let list_hd list = match list with
>    | [] -> perform EmptyListException
>    | hd :: _ -> hd
> 
> let safe_list_hd l = match list_hd l with
>   | effect EmptyListException -> 0, false
>   | res -> res, true
> 
> let main = safe_list_hd [12; 65; 94]
> EOF
(12, true)
$ ./demo.exe <<-EOF
> effect EmptyListException : int effect
> 
> let list_hd list = match list with
>    | [] -> perform EmptyListException
>    | hd :: _ -> hd
> 
> let safe_list_hd l = match list_hd l with
>   | effect EmptyListException -> 0, false
>   | res -> res, true
> 
> let main = safe_list_hd []
> EOF
(0, false)
$ ./demo.exe <<-EOF
> effect EmptyListException : int effect
> 
> let list_hd list = match list with
>    | [] -> perform EmptyListException
>    | hd :: _ -> hd
> 
> let safe_list_hd l = match list_hd l with
>   | effect EmptyListException -> continue (0, false)
>   | res -> res, true
> 
> let main = safe_list_hd []
> EOF
((0, false), true)
$ ./demo.exe <<-EOF
> effect SmallDiscount : int -> int effect
> 
> effect BigDiscount : int -> int effect
> 
> let count_discount value = if value < 10000 then perform (SmallDiscount value) else perform (BigDiscount value)
> 
> let main = match count_discount 8500 with
>   | effect (SmallDiscount v) -> continue (v - v / 10)
>   | effect (BigDiscount v) -> continue (v - v / 5)
>   | v -> v
> EOF
7650
$ ./demo.exe <<-EOF
> effect SmallDiscount : int -> int effect
> 
> effect BigDiscount : int -> int effect
> 
> let count_discount value = if value < 10000 then perform (SmallDiscount value) else perform (BigDiscount value)
> 
> let main = match count_discount 25000 with
>   | effect (SmallDiscount v) -> continue (v - v / 10)
>   | effect (BigDiscount v) -> continue (v - v / 5)
>   | v -> v
> EOF
20000
$ ./demo.exe <<-EOF
> effect E: int -> int effect
> 
> let helper x = match perform (E x) with
>    | effect (E s) -> continue "hello"
>    | v -> v
> 
> let main = match perform (E 5) with
>    | effect (E s) -> continue (s*s)
>    | v -> helper v
> EOF
Unification failed: type of the expression is string but expected type was int
$ ./demo.exe <<-EOF
> let main = [1; 2; 3; "abc"]
> EOF
Unification failed: type of the expression is string but expected type was int
$ ./demo.exe <<-EOF
> let main = (1, 2, 3, "abc", fun x -> x * x)
> EOF
(1, 2, 3, "abc", <fun>)
$ ./demo.exe <<-EOF
> let f x = x * 100
> let main = ["0", "1", f "2"]
> EOF
Unification failed: type of the expression is string but expected type was int
$ ./demo.exe <<-EOF
> let rec remove_last list = match list with
> | [] -> []
> | [head] -> []
> | head :: tail -> head :: remove_last tail
> 
> let main = remove_last [1;2;3;4;5]
> EOF
[1; 2; 3; 4]
$ ./demo.exe <<-EOF
> let _ x = x
> EOF
: end_of_input
