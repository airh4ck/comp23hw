Copyright 2023-2024, Kakadu and contributors
A test for miniML designed by Kakadu 

truee and falsee should be valid identifiers!

  $ cat > input.ml <<- EOF
  > let main truee = truee  
  > EOF
  $ ocaml input.ml
  $ cat input.ml | ./llvm_test.exe   
  : end_of_input
