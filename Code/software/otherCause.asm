sys_other_cause:
# 前面需要把用过的寄存器全部存进内存里面，然后后面统一恢复
# addi sp, sp, -24
# sw a1, 4(sp)
# sw a2, 8(sp)
# sw a3, 12(sp)
# sw a4, 16(sp)
# sw t0, 20(sp)

addi a1, zero, -256 # 0xffff_ff00 -> led * 24
addi a2, a1, 4      # 0xffff_ff04 -> btn * 5
addi a3, a1, 8      # 0xffff_ff08 -> swi * 24
addi a4, a1, 12     # 0xffff_ff0c -> 7seg

addi t0, zero, 0
lw t0, (a1)

csrrw t0, mtval, zero
sw t0, (a4)
sys_other_wait:
lw t0, (a2)
andi t0, t0, 12
beq t0, zero, sys_other_wait

addi t0, zero, 0
sw t0, (a4)
csrrs t0, mepc, zero
addi t0, t0, 4
csrrw zero, mepc, t0
lw a1, 4(sp)
lw a2, 8(sp)
lw a3, 12(sp)
lw a4, 16(sp)
lw t0, 20(sp)
addi sp, sp, 24
mret
