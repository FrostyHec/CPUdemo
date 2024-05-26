sys_interrupt:

addi sp, sp, -24
sw a1, 4(sp)
sw a2, 8(sp)
sw a3, 12(sp)
sw a4, 16(sp)
sw t0, 20(sp)

csrrw t0, mcause, zero
addi a1, zero, 11 # ecall
addi a2, zero, 3  # ebreak
beq t0, a1, sys_ecall
beq t0, a2, sys_ebreak
beq zero, zero, sys_other_cause

