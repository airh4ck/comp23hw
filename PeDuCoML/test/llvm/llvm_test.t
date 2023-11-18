  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = print_int 42
  > EOF
  declare i64 @print_bool(i64)
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
  declare i64 @print_bool(i64)
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
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = print_int (((13 + 52 - 7) * 3 - 6) / 4)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"5" = alloca i64, align 8
    store i64 65, i64* %"5", align 4
    %"4" = alloca i64, align 8
    %"51" = load i64, i64* %"5", align 4
    %boptmp = sub i64 %"51", 7
    store i64 %boptmp, i64* %"4", align 4
    %"3" = alloca i64, align 8
    %"42" = load i64, i64* %"4", align 4
    %boptmp3 = mul i64 %"42", 3
    store i64 %boptmp3, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"34" = load i64, i64* %"3", align 4
    %boptmp5 = sub i64 %"34", 6
    store i64 %boptmp5, i64* %"2", align 4
    %"1" = alloca i64, align 8
    %"26" = load i64, i64* %"2", align 4
    %boptmp7 = udiv i64 %"26", 4
    store i64 %boptmp7, i64* %"1", align 4
    %"0" = alloca i64, align 8
    %"18" = load i64, i64* %"1", align 4
    %tmp_call1 = call i64 @print_int(i64 %"18")
    store i64 %tmp_call1, i64* %"0", align 4
    %"09" = load i64, i64* %"0", align 4
    ret i64 %"09"
  }
  ---
  42
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x = y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 1, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 1, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = icmp eq i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  true
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x <> y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 1, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 1, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = icmp ne i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  false
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x < y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 1, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 42, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = icmp slt i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  true
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = 42 in
  >   let y = 1 in
  >   print_bool (x <= y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 42, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 1, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = icmp sle i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  false
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x > y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 1, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 42, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = icmp sgt i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  false
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = 42 in
  >   let y = 1 in
  >   print_bool (x >= y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 42, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 1, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = icmp sge i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  true
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x && y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 1, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 0, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = and i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  false
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x || y)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    store i64 1, i64* %"0", align 4
    %"1" = alloca i64, align 8
    store i64 0, i64* %"1", align 4
    %"3" = alloca i64, align 8
    %"01" = load i64, i64* %"0", align 4
    %"12" = load i64, i64* %"1", align 4
    %boptmp = or i64 %"01", %"12"
    %zext = zext i1 %boptmp to i64
    store i64 %zext, i64* %"3", align 4
    %"2" = alloca i64, align 8
    %"33" = load i64, i64* %"3", align 4
    %tmp_call1 = call i64 @print_bool(i64 %"33")
    store i64 %tmp_call1, i64* %"2", align 4
    %"24" = load i64, i64* %"2", align 4
    ret i64 %"24"
  }
  ---
  true
