  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let x = 52
  > EOF
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %tmp_call = call i64 @print_int(i64 42)
    ret i64 0
  }
  define i64 @x() {
  entry:
    ret i64 52
  }
  ---
  42
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let not_main = 'c'
  > EOF
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %tmp_call = call i64 @print_int(i64 42)
    ret i64 0
  }
  define i64 @not_main() {
  entry:
    ret i64 99
  }
  ---
  42
