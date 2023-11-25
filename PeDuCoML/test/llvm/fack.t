  $ cat > input.ml <<- EOF
  > let rec fack n k = if n<=1 then k 1 else fack (n-1) (fun m -> k(m*n))
  > let fac n = fack n (fun id -> id)
  > let main = print_int (fac 6)
  > EOF
  $ ocaml input.ml
  720
  $ cat input.ml | ./llvm_test.exe | lli-16 -load ../../runtime/peducoml_runtime.so
  