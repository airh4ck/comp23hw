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
  
