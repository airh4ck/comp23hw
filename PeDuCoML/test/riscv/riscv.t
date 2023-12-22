  $ ./riscv_test.exe <<- EOF | tee riscv_test.S
  > let main = 2
  > EOF
      .globl main
      .type main, @function
  main:
      addi sp,sp,-16
      sd ra,8(sp)
      sd s0,0(sp)
      addi s0,sp,16
      li a0,2
      ld ra,8(sp)
      ld s0,0(sp)
      addi sp,sp,16
      ret
  $ riscv64-linux-gnu-gcc -static riscv_test.S -o riscv_test.out
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  
  exit code = 2 (0x2)
  $ ./riscv_test.exe <<- EOF | tee riscv_test.S
  > let main = print_int 2
  > EOF
      .globl main
      .type main, @function
  main:
      addi sp,sp,-24
      sd ra,16(sp)
      sd s0,8(sp)
      addi s0,sp,24
      li a0,2
      call print_int
      sd a0,-24(s0)
      ld t0,-24(s0)
      mv a0,t0
      ld ra,16(sp)
      ld s0,8(sp)
      addi sp,sp,24
      ret
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  2
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let ignore x = 42
  > let main = print_int (ignore 0)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  42
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_char 'c'
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  c
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (13 + 52)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  65
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (132 - 42)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  90
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (51 - 100)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  -49
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (13 * 11)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  143
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (13 * 11)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  143
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (36 / 18)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  2
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (((13 + 52 - 7) * 3 - 6) / 4)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  42
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main =
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x = y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  true
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main =
  >   let x = 1 in
  >   let y = 2 in
  >   print_bool (x = y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  false
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x <> y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  false
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x < y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  true
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 42 in
  >   let y = 1 in
  >   print_bool (x <= y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  false
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x > y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  false
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 42 in
  >   let y = 42 in
  >   print_bool (x >= y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  true
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x && y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  false
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x || y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  true
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_list [1; 2; 3; 4; 5]
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  [1; 2; 3; 4; 5]
  exit code = 0 (0x0)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_tuple (10, 5, false, true)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  (10, 5, 0, 1)
  exit code = 0 (0x0)
