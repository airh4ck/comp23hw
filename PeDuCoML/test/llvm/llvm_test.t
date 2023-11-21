  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = print_int 42
  > EOF
  declare i64 @print_new_line(i64)
  declare i64 @print_tuple(i64)
  declare i64 @print_list(i64)
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  declare i64 @peducoml_fill_tuple(i64, i64)
  declare i64 @peducoml_alloc_tuple(i64)
  declare i64 @peducoml_length(i64)
  declare i64 @peducoml_tail(i64)
  declare i64 @peducoml_field(i64, i64)
  declare i64 @peducoml_add_to_list(i64, i64)
  declare i64 @peducoml_alloc_list()
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
