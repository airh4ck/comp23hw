  $ ./riscv_test.exe <<- EOF | tee riscv_test.S
  > let main = 2
  > EOF
      .globl main
      .type main, @function
  main:
      addi sp,sp,-16
      sd ra,8(sp)
      sd s0,0(sp)
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
      addi sp,sp,-16
      sd ra,8(sp)
      sd s0,0(sp)
      li a0,2
      call print_int
      mv t0,a0
      mv a0,t0
      ld ra,8(sp)
      ld s0,0(sp)
      addi sp,sp,16
      ret
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ rvlinux riscv_test.out | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
  2
  exit code = 0 (0x0)
  