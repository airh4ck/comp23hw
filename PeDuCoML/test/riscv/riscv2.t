Copyright 2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0


  $ ./riscv_test.exe <<- EOF
  > let sum x y = x+y
  > let main = print_int (if true then (sum 1) else (sum (-1)))
  > EOF
  Unification failed: type of the expression is int -> int but expected type was int
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let sum a b = a+b
  > let main = 
  >   let u = if false then sum 1 else print_int in 
  >   print_int (u 0)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  00
