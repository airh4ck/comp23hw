  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; lli-15 llvm_test.ll; echo "---\n$?"
  > let main = 42
  > EOF
  define i64 @main() {
  entry:
    ret i64 42
  }
  ---
  42
$ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; lli-15 llvm_test.ll; echo "---\n$?"
> let main = "abacaba"
> EOF
define i64 @main() {
entry:
ret i64 1
}

Output:
IDK
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; lli-15 llvm_test.ll; echo "---\n$?"
  > let main = 'c'
  > EOF
  define i64 @main() {
  entry:
    ret i64 99
  }
  ---
  99
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; lli-15 llvm_test.ll; echo "---\n$?"
  > let main = [1; 2]
  > EOF
  define i64 @main() {
  entry:
    ret i64 1
  }
  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; lli-15 llvm_test.ll; echo "---\n$?"
  > let main = [1; 2]
  > EOF
  define i64 @main() {
  entry:
    ret i64 1
  }