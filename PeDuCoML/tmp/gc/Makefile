RUNTIME_PATH=../../runtime/
linux:
	gcc $(RUNTIME_PATH)/gc.c main.c -o gc_demo

riscv: 
	riscv64-linux-gnu-gcc -static -o riscv_test.out $(RUNTIME_PATH)/gc.c main.c $(RUNTIME_PATH)/gc.s