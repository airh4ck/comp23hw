  $ ./riscv_test.exe <<- EOF | tee riscv_test.S
  > let main = 2
  > EOF
  $ riscv64-unknown-elf-gcc -static -c riscv_test.S -o riscv_test.o
  $ riscv64-unknown-elf-ld -o riscv_test.elf riscv_test.o
  $ rvlinux riscv_test.elf | awk 'BEGIN{RS=""} {split($0, arr, "Instructions executed"); split(arr[1], rez, ">>> Program exited, "); print rez[1]; print rez[2]}'
