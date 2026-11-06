addi a0, zero, -256 # 0xffff_ff00 -> led * 24
addi a1, a0, 4      # 0xffff_ff04 -> btn * 5
addi a2, a0, 8      # 0xffff_ff08 -> swi * 24
addi a3, a0, 12     # 0xffff_ff0c -> 7seg
addi a6, zero, 1
slli a6, a6, 16
srli t0, a0, 16
add a6, a6, t0		
addi a6, a6, -3	# 0x0001_fffc -> stack

case1:
lw t0, 0(a2)
andi t0, t0, 1
beq t0, zero, case1

addi x1, x0, 1
addi x3, x0, 3
addi x5, x0, 5
sub x2, x1, x3
and x12, x2, x5
sw x12, 0(a3)