  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = print_int 42
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
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
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let id x = x
  > let main = print_int (id 42)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
  define i64 @id(i64 %"0") {
  entry:
    %"01" = alloca i64, align 8
    store i64 %"0", i64* %"01", align 4
    %"02" = load i64, i64* %"01", align 4
    ret i64 %"02"
  }
  define i64 @main() {
  entry:
    %"2" = alloca i64, align 8
    %tmp_call1 = call i64 @id(i64 42)
    store i64 %tmp_call1, i64* %"2", align 4
    %"1" = alloca i64, align 8
    %"21" = load i64, i64* %"2", align 4
    %tmp_call12 = call i64 @print_int(i64 %"21")
    store i64 %tmp_call12, i64* %"1", align 4
    %"13" = load i64, i64* %"1", align 4
    ret i64 %"13"
  }
  ---
  42
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let add x y = x + y
  > let main = print_int (add 12 32)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
  define i64 @add(i64 %"0", i64 %"1") {
  entry:
    %"11" = alloca i64, align 8
    store i64 %"1", i64* %"11", align 4
    %"02" = alloca i64, align 8
    store i64 %"0", i64* %"02", align 4
    %"2" = alloca i64, align 8
    %"03" = load i64, i64* %"02", align 4
    %"14" = load i64, i64* %"11", align 4
    %boptmp = add i64 %"03", %"14"
    store i64 %boptmp, i64* %"2", align 4
    %"25" = load i64, i64* %"2", align 4
    ret i64 %"25"
  }
  define i64 @main() {
  entry:
    %"5" = alloca i64, align 8
    %peducoml_alloc_closure = call i64 @peducoml_alloc_closure(i64 ptrtoint (i64 (i64, i64)* @add to i64), i64 2)
    %peducoml_apply = call i64 @peducoml_apply(i64 %peducoml_alloc_closure, i64 12)
    store i64 %peducoml_apply, i64* %"5", align 4
    %"4" = alloca i64, align 8
    %"51" = load i64, i64* %"5", align 4
    %peducoml_alloc_closure2 = call i64 @peducoml_alloc_closure(i64 %"51", i64 12597)
    %peducoml_apply3 = call i64 @peducoml_apply(i64 %peducoml_alloc_closure2, i64 32)
    store i64 %peducoml_apply3, i64* %"4", align 4
    %"3" = alloca i64, align 8
    %"44" = load i64, i64* %"4", align 4
    %tmp_call1 = call i64 @print_int(i64 %"44")
    store i64 %tmp_call1, i64* %"3", align 4
    %"35" = load i64, i64* %"3", align 4
    ret i64 %"35"
  }
  ---
  44
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let add_and_mul x y z = (x + y) * z
  > let main = print_int (add_and_mul 1 3 5)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
  define i64 @add_and_mul(i64 %"0", i64 %"1", i64 %"2") {
  entry:
    %"21" = alloca i64, align 8
    store i64 %"2", i64* %"21", align 4
    %"12" = alloca i64, align 8
    store i64 %"1", i64* %"12", align 4
    %"03" = alloca i64, align 8
    store i64 %"0", i64* %"03", align 4
    %"4" = alloca i64, align 8
    %"04" = load i64, i64* %"03", align 4
    %"15" = load i64, i64* %"12", align 4
    %boptmp = add i64 %"04", %"15"
    store i64 %boptmp, i64* %"4", align 4
    %"3" = alloca i64, align 8
    %"46" = load i64, i64* %"4", align 4
    %"27" = load i64, i64* %"21", align 4
    %boptmp8 = mul i64 %"46", %"27"
    store i64 %boptmp8, i64* %"3", align 4
    %"39" = load i64, i64* %"3", align 4
    ret i64 %"39"
  }
  define i64 @main() {
  entry:
    %"8" = alloca i64, align 8
    %peducoml_alloc_closure = call i64 @peducoml_alloc_closure(i64 ptrtoint (i64 (i64, i64, i64)* @add_and_mul to i64), i64 3)
    %peducoml_apply = call i64 @peducoml_apply(i64 %peducoml_alloc_closure, i64 1)
    store i64 %peducoml_apply, i64* %"8", align 4
    %"7" = alloca i64, align 8
    %"81" = load i64, i64* %"8", align 4
    %peducoml_alloc_closure2 = call i64 @peducoml_alloc_closure(i64 %"81", i64 12600)
    %peducoml_apply3 = call i64 @peducoml_apply(i64 %peducoml_alloc_closure2, i64 3)
    store i64 %peducoml_apply3, i64* %"7", align 4
    %"6" = alloca i64, align 8
    %"74" = load i64, i64* %"7", align 4
    %peducoml_alloc_closure5 = call i64 @peducoml_alloc_closure(i64 %"74", i64 13367)
    %peducoml_apply6 = call i64 @peducoml_apply(i64 %peducoml_alloc_closure5, i64 5)
    store i64 %peducoml_apply6, i64* %"6", align 4
    %"5" = alloca i64, align 8
    %"67" = load i64, i64* %"6", align 4
    %tmp_call1 = call i64 @print_int(i64 %"67")
    store i64 %tmp_call1, i64* %"5", align 4
    %"58" = load i64, i64* %"5", align 4
    ret i64 %"58"
  }
  ---
  20
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-15 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let add x y = x + y
  > let partial_add x = add x
  > let main = print_int (partial_add 1 2)
  > EOF
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
  define i64 @add(i64 %"0", i64 %"1") {
  entry:
    %"11" = alloca i64, align 8
    store i64 %"1", i64* %"11", align 4
    %"02" = alloca i64, align 8
    store i64 %"0", i64* %"02", align 4
    %"2" = alloca i64, align 8
    %"03" = load i64, i64* %"02", align 4
    %"14" = load i64, i64* %"11", align 4
    %boptmp = add i64 %"03", %"14"
    store i64 %boptmp, i64* %"2", align 4
    %"25" = load i64, i64* %"2", align 4
    ret i64 %"25"
  }
  define i64 @partial_add(i64 %"3") {
  entry:
    %"31" = alloca i64, align 8
    store i64 %"3", i64* %"31", align 4
    %"4" = alloca i64, align 8
    %"32" = load i64, i64* %"31", align 4
    %peducoml_alloc_closure = call i64 @peducoml_alloc_closure(i64 ptrtoint (i64 (i64, i64)* @add to i64), i64 2)
    %peducoml_apply = call i64 @peducoml_apply(i64 %peducoml_alloc_closure, i64 %"32")
    store i64 %peducoml_apply, i64* %"4", align 4
    %"43" = load i64, i64* %"4", align 4
    ret i64 %"43"
  }
  define i64 @main() {
  entry:
    %"7" = alloca i64, align 8
    %tmp_call1 = call i64 @partial_add(i64 1)
    store i64 %tmp_call1, i64* %"7", align 4
    %"6" = alloca i64, align 8
    %"71" = load i64, i64* %"7", align 4
    %peducoml_alloc_closure = call i64 @peducoml_alloc_closure(i64 %"71", i64 12599)
    %peducoml_apply = call i64 @peducoml_apply(i64 %peducoml_alloc_closure, i64 2)
    store i64 %peducoml_apply, i64* %"6", align 4
    %"5" = alloca i64, align 8
    %"62" = load i64, i64* %"6", align 4
    %tmp_call13 = call i64 @print_int(i64 %"62")
    store i64 %tmp_call13, i64* %"5", align 4
    %"54" = load i64, i64* %"5", align 4
    ret i64 %"54"
  }
  ---
  3
