  $ ./llvm_test.exe <<- EOF
  > let main = 1
  > EOF
  define i64 @main() {
  entry:
    ret i64 1
  }
  $ ./llvm_test.exe <<- EOF
  > let main = "abacaba"
  > EOF
  define i64 @main() {
  entry:
    ret i64 1
  }
