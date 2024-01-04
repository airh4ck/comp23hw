    .text
    .globl gc_init
    .type gc_init, @function
gc_init:
    mv a0, sp
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    call peducoml_init
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
    .globl gc_get_stack_top
    .type gc_get_stack_top, @function
gc_get_stack_top:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    mv a0, sp
    call print_int
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
    .globl gc_stack_scan
    .type gc_stack_scan, @function
gc_stack_scan:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    mv t0, sp
    mv t1, a0
.Lstack_loop:
    bge t0, t1, .Lloop_break
    mv a0, t0
    mv a1, sp
    mv a2, t0
    call check_pointer
    mv t0, a2
    addi t0, t0, 8
    j .Lstack_loop
.Lloop_break:
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret 
