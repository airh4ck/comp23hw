        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0"
        .file   "smth"
        .globl  main                            # -- Begin function main
        .p2align        2
        .type   main,@function
main:                                   # @main
        .cfi_startproc
# %bb.0:                                # %entry
        addi    sp, sp, -16
        .cfi_def_cfa_offset 16
        sd      ra, 8(sp)                       # 8-byte Folded Spill
        .cfi_offset ra, -8
        lui     a0, %hi(print_int)
        addi    a0, a0, %lo(print_int)
        li      a1, 1
        call    peducoml_alloc_closure@plt
        li      a1, 42
        call    peducoml_apply@plt
        sd      a0, 0(sp)
        ld      ra, 8(sp)                       # 8-byte Folded Reload
        addi    sp, sp, 16
        ret
.Lfunc_end0:
        .size   main, .Lfunc_end0-main
        .cfi_endproc
                                        # -- End function
        .section        ".note.GNU-stack","",@progbits

