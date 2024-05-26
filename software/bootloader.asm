la x1, sys_interrupt
csrrw x0, mtvec, x1


addi a0, zero, -236 # ffff_ff14 -> uart
addi a1, zero, -232 # ffff_ff18 -> uart_valid
addi a2, zero, -228 # ffff_ff1c -> uart_ready
addi ra, zero, 1000 # start of the program
addi a3, ra, 0      # pointer

sys_getIns_ready:
    addi t1, zero, 4

sys_uart_wait:
    lw t0, (a1)
    andi t0, t0, 1
    # reset uart_ready to 0
    addi t5, zero, 0
    sw t5, (a2)
    beq t0, zero, sys_uart_wait
    beq zero, zero, sys_uart_read

sys_uart_read:
    lw t0, (a1)
    andi t0, t0, 1
    beq t0, zero, sys_uart_prewait
    lw t2, (a0)
    slli t2, t2, 24
    or t3, t3, t2
    # set uart_ready to 1
    addi t5, zero, 1
    sw t5, (a2)
    beq zero, zero, sys_uart_read

sys_uart_prewait:
    addi t1, t1, -1
    beq t1, zero, sys_uart_write
    srli t3, t3, 8
    beq t0, zero, sys_uart_wait

sys_uart_write:
    not t5, t3
    beq t5, zero, sys_run
    sw t3, (a3)
    addi a3, a3, 4
    beq zero, zero, sys_getIns_ready

sys_run:
    addi a0, zero, 0
    addi a1, zero, 0
    addi a2, zero, 0
    addi a3, zero, 0
    addi ra, zero, 0
    addi t0, zero, 0
    addi t1, zero, 0
    addi t2, zero, 0
    addi t3, zero, 0
    addi t4, zero, 0
    addi t5, zero, 0
    jalr ra, 0

