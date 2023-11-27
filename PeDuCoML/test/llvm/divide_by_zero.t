Copyright 2023-2024, Kakadu and contributors
A test for miniML designed by Kakadu 

  $ cat > input.ml <<- EOF
  > let main = print_int (1/0)
  > EOF

  $ cat input.ml | ./llvm_test.exe | lli-16 -load ../../runtime/peducoml_runtime.so
  94272858410000
