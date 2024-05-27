#           ebreak
# 
# |  button  |    function    |
# |----------|----------------|
# |    b3    |   confirm reg  |
# |    b4    |     go back    |
# 
#
sys_boot:
la x1, sys_ebreak
csrrw x0, mtvec, x1
addi x1, x0, 0
jal app


sys_ebreak:
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

addi t0, zero, 1
slli t0, t0, 22 # error on 7seg
lw t0, (a4)
addi t0, zero, 0
lw t0, (a1)

# determine the cases
sys_ebreak_cases:
lw t0, (a2)
andi t0, t0, 12
beq t0, zero, sys_ebreak_cases
andi t0, t0, 4
beq t0, zero, sys_ebreak_case2
beq zero, zero, sys_ebreak_case1

sys_ebreak_case1:
    lw t0, (a3)
    sw t0, (a1)
    addi t0, t0, -31
    beq t0, zero, sys_ebreak_x31
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x30
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x29
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x28
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x27
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x26
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x25
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x24
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x23
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x22
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x21
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x20
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x19
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x18
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x17
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x16
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x15
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x14
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x13
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x12
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x11
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x10
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x9
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x8
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x7
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x6
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x5
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x4
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x3
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x2
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x1
    addi t0, t0, 1
    beq t0, zero, sys_ebreak_x0
    beq zero, zero, sys_ebreak_cases

sys_ebreak_c:
    sys_ebreak_x0:
        lw t0, (x0)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x1:
        lw t0, (x1)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x2:
        lw t0, (x2)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x3:
        lw t0, (x3)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x4:
        lw t0, (x4)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x5:
        lw t0, (x5)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x6:
        lw t0, (x6)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x7:
        lw t0, (x7)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x8:
        lw t0, (x8)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x9:
        lw t0, (x9)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x10:
        lw t0, (x10)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x11:
        lw t0, (x11)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x12:
        lw t0, (x12)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x13:
        lw t0, (x13)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x14:
        lw t0, (x14)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x15:
        lw t0, (x15)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x16:
        lw t0, (x16)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x17:
        lw t0, (x17)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x18:
        lw t0, (x18)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x19:
        lw t0, (x19)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x20:
        lw t0, (x20)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x21:
        lw t0, (x21)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x22:
        lw t0, (x22)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x23:
        lw t0, (x23)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x24:
        lw t0, (x24)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x25:
        lw t0, (x25)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x26:
        lw t0, (x26)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x27:
        lw t0, (x27)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x28:
        lw t0, (x28)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x29:
        lw t0, (x29)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x30:
        lw t0, (x30)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x31:
        lw t0, (x31)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases

sys_ebreak_case2: # go back
    addi t0, zero, 0
    lw t0, (a4)
    lw t0, (a1)
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
addi t0,  zero, 1
addi t1,  zero, 2
addi t2,  zero, 3
addi t3,  zero, 4
addi t4,  zero, 5
ebreak