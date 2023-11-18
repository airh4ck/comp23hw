  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = print_int 42
  > EOF
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    %tmp_call1 = call i64 @print_int(i64 42)
    store i64 %tmp_call1, i64* %"0", align 4
    %"01" = load i64, i64* %"0", align 4
    ret i64 %"01"
  }
  ---
  42
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = print_char 'c'
  > EOF
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    %tmp_call1 = call i64 @print_char(i64 99)
    store i64 %tmp_call1, i64* %"0", align 4
    %"01" = load i64, i64* %"0", align 4
    ret i64 %"01"
  }
  ---
  c
