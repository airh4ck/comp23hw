Copyright 2023-2024, Kakadu and contributors
A test for miniML designed by Kakadu 

  $ cat > input.ml <<- EOF
  > let rec fib n =
  >   if n=0 then 0 else if n=1 then 1 else fib (n-2) + fib (n-1)
  > let main = let u = print_int (fib 8) in 0
  > EOF

  $ ocaml input.ml
  File "./input.ml", line 3, characters 15-16:
  3 | let main = let u = print_int (fib 8) in 0
                     ^
  Warning 26 [unused-var]: unused variable u.
  21
  $ cat input.ml | ./llvm_test.exe | lli-16 -load ../../runtime/peducoml_runtime.so
  21

  $ cat > input.ml <<- EOF
  > let rec fib n k =
  >   if n=0 then k 0 else if n=1 then k 1 else fib (n-2) (fun a -> fib (n-1) (fun b -> k(a+b)))
  > let main = let u = print_int (fib 8 (fun w -> w)) in 0
  > EOF
  $ ocaml input.ml
  File "./input.ml", line 3, characters 15-16:
  3 | let main = let u = print_int (fib 8 (fun w -> w)) in 0
                     ^
  Warning 26 [unused-var]: unused variable u.
  21
  $ cat input.ml | ./llvm_test.exe | lli-16 -load ../../runtime/peducoml_runtime.so
  lli-16: lli: <stdin>:1:1: error: expected top-level entity
  Occurs check failed.
  ^
  
  [1]
