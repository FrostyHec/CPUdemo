addi sp, x0, 511
slli sp, sp, 8
addi sp, sp, 252 # 0x0001_fffc -> stack

sys_boot:
la x1, sys_other_cause
csrrw x0, mtvec, x1
addi x1, x0, 0
jal app


sys_other_cause:
# 前面�?要把用过的寄存器全部存进内存里面，然后后面统�?恢复
addi sp, sp, -24
sw a1, 4(sp)
sw a2, 8(sp)
sw a3, 12(sp)
sw a4, 16(sp)
sw t0, 20(sp)

addi a1, zero, -256 # 0xffff_ff00 -> led * 24
addi a2, a1, 4      # 0xffff_ff04 -> btn * 5
addi a3, a1, 8      # 0xffff_ff08 -> swi * 24
addi a4, a1, 12     # 0xffff_ff0c -> 7seg

addi t0, zero, 0
lw t0, (a1)

csrrw t0, mtval, zero
sw t0, (a4)
sw t0, (a1)
sys_other_wait:
lw t0, (a2)
andi t0, t0, 4
beq t0, zero, sys_other_wait

addi t0, zero, 0
sw t0, (a4)
csrrs t0, mepc, zero
addi t0, t0, 4
csrrw zero, mepc, t0
sys_other_wait1:
lw t0, (a2)
andi t0, t0, 4
bne t0, zero, sys_other_wait1
lw a1, 4(sp)
lw a2, 8(sp)
lw a3, 12(sp)
lw a4, 16(sp)
lw t0, 20(sp)
addi sp, sp, 24
mret


app:
addi x1, x0, 9
lw t0, (x0) # load wrong address -> output 9
# csrrs x0, mepc, x0 # wrong instruction?
sw t0, (x0) # store wrong address -> output 9
