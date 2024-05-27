#           ecall
# 
# |  a7  |  load  |    to    |
# |------|--------|----------|
# |   1  | switch |    a0    |
# |   2  |   a0   |   7seg   |
# |   3  |   a0   |   led    |
# |   4  |  exit  |   ---    |
#
sys_boot:
la x1, sys_ecall
csrrw x0, mtvec, x1
addi x1, x0, 0
jal app

sys_ecall:
# 前面需要把用过的寄存器全部存进内存里面，然后后面统一恢复
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
sw t0, (a4)
sw t0, (a1)

# determine the cases
addi t0, zero, 1
beq a7, t0, sys_ecall_case1
addi t0, t0, 1
beq a7, t0, sys_ecall_case2
addi t0, t0, 1
beq a7, t0, sys_ecall_case3
addi t0, t0, 1
beq a7, t0, sys_ecall_case4
# if no cases satisfy, then output error
addi t0, zero, 1
slli t0, t0, 22
sw t0, (a1)
jal sys_ecall_goBack


sys_ecall_case1:
    lw t0, (a2)
    andi t0, t0, 4
    beq t0, zero, sys_ecall_case1
    lw a0, (a3)
    jal sys_ecall_goBack


sys_ecall_case2:
    lw t0, (a2)
    andi t0, t0, 4
    beq t0, zero, sys_ecall_case2
    sw a0, (a4)
    jal sys_ecall_goBack


sys_ecall_case3:
    lw t0, (a2)
    andi t0, t0, 4
    beq t0, zero, sys_ecall_case3
    sw a0, (a1)
    jal sys_ecall_goBack

sys_ecall_case4:
    lw t0, (a2)
    andi t0, t0, 4
    beq t0, zero, sys_ecall_case4
    jal sys_boot


sys_ecall_goBack:
    lw t0, (a2)
    andi t0, t0, 4
    beq t0, zero, sys_ecall_goBack
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




app:
addi a7, zero, 1
ecall
addi a7, zero, 2
ecall
addi a7, zero, 3
ecall
addi a7, zero, 4
ecall