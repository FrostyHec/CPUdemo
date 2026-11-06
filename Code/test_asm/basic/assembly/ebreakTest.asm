#           ebreak
# 
# |  button  |    function    |
# |----------|----------------|
# |    b3    |   confirm reg  |
# |    b4    |     go back    |
# 
#

addi sp, x0, 511
slli sp, sp, 8
addi sp, sp, 252 # 0x0001_fffc -> stack

sys_boot:
la x1, sys_ebreak
csrrw x0, mtvec, x1 # mtvec -> x0, x1 -> mtvec
addi x1, x0, 0
jal app


sys_ebreak:
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

addi t0, zero, 1
slli t0, t0, 22 # error on 7seg
sw t0, (a4) # print error on 7seg
addi t0, zero, 0
sw t0, (a1) # clear led

# determine the cases
sys_ebreak_cases:
lw t0, (a2) # load button
andi t0, t0, 12 # button 3 / 4
beq t0, zero, sys_ebreak_cases
andi t0, t0, 4 # button 3
beq t0, zero, sys_ebreak_case2
beq zero, zero, sys_ebreak_case1

sys_ebreak_case1:
    lw t0, (a3) # load switches
    sw t0, (a1) # store led
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
        addi t0, x0, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x1:
        addi t0, x1, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x2:
        addi t0, x2, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x3:
        addi t0, x3, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x4:
        addi t0, x4, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x5: # t0 20(sp)
        lw t0, 20(sp)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x6:
        addi t0, x6, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x7:
        addi t0, x7, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x8:
        addi t0, x8, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x9:
        addi t0, x9, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x10:
        addi t0, x10, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x11: # a1 4(sp)
        lw t0, 4(sp)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x12: # a2 8(sp)
        lw t0, 8(sp)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x13: # a3 12(sp)
        lw t0, 12(sp)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x14: # a4 16(sp)
        lw t0, 16(sp)
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x15:
        addi t0, x15, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x16:
        addi t0, x16, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x17:
        addi t0, x17, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x18:
        addi t0, x18, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x19:
        addi t0, x19, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x20:
        addi t0, x20, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x21:
        addi t0, x21, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x22:
        addi t0, x22, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x23:
        addi t0, x23, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x24:
        addi t0, x24, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x25:
        addi t0, x25, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x26:
        addi t0, x26, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x27:
        addi t0, x27, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x28:
        addi t0, x28, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x29:
        addi t0, x29, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x30:
        addi t0, x30, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases
    sys_ebreak_x31:
    	addi x31, x0, 10
        addi t0, x31, 0
        sw t0, (a4)
        beq zero, zero, sys_ebreak_cases

sys_ebreak_case2: # go back
    addi t0, zero, 0
    sw t0, (a4)
    sw t0, (a1)
    csrrs t0, mepc, zero # mepc -> t0, 0 -> mepc
    addi t0, t0, 4 # t0 should be 868
    csrrw zero, mepc, t0
    lw a1, 4(sp)
    lw a2, 8(sp)
    lw a3, 12(sp)
    lw a4, 16(sp)
    lw t0, 20(sp)
    addi sp, sp, 24
    mret



app:
addi a4, zero, 20
addi t0,  zero, 1
addi t1,  zero, 2
addi t2,  zero, 3
addi t3,  zero, 4
addi t4,  zero, 5
ebreak
