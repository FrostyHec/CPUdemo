# initialize the data address
addi a0, zero, -256 # 0xffff_ff00 -> led * 24
addi a1, a0, 4      # 0xffff_ff04 -> btn * 5
addi a2, a0, 8      # 0xffff_ff08 -> swi * 24
addi a3, a0, 12     # 0xffff_ff0c -> 7seg
addi a6, zero, 1
slli a6, a6, 16
srli t0, a0, 16
add a6, a6, t0		# 0x0001_ffff -> stack
addi a4, zero, 7
slli a4, a4, 20     # mask of the cases
addi t0, zero, -1
sw t0, (a0)

# determine the cases
ini:
lw t0, (a1)
andi t0, t0, 4
bne t0, zero, ini
situation:
addi a5, zero, 0
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, situation
lw t1, (a2)
and t1, t1, a4
srli t1, t1, 20
check0:
lw t0, (a1)
andi t0, t0, 4
bne t0, zero, check0

# clear the outputs
addi t0, zero, 0
sw t0, (a3)
sw t0, (a0)

beq a5, t1, case0
addi a5, a5, 1
beq a5, t1, case1
addi a5, a5, 1
beq a5, t1, case2
addi a5, a5, 1
beq a5, t1, case3
addi a5, a5, 1
beq a5, t1, case4
addi a5, a5, 1
beq a5, t1, case5
addi a5, a5, 1
beq a5, t1, case6
addi a5, a5, 1
beq a5, t1, case7
beq zero, zero, ini

case0:
# 7seg -> a
addi t5, zero, 10
sw t5, (a3)
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case0
addi a5, zero, 255
lw t3, (a2)
and t3, t3, a5
slli t3, t3, 8
sw t3, (a0)
check0c1:
lw t0, (a1)
andi t0, t0, 4
bne t0, zero, check0c1
	case01:
	# 7seg -> b
	addi t5, zero, 11
	sw t5, (a3)
	lw t0, (a1)
	andi t0, t0, 4
	beq t0, zero, case01
	addi a5, zero, 255
	lw t4, (a2)
	and t4, t4, a5
	or t3, t3, t4
	sw t3, (a0)
	beq zero, zero, ini
	

case1:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case1
lb t3, (a2)
sw t3, (a3) # 7-seg
sw t3, (a6)
beq zero, zero, ini

case2:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case2
lbu t4, (a2)
sw t4, (a3) # 7-seg
sw t4, -4(a6)
beq zero, zero, ini

case3:
lw t3, (a6)
lw t4, -4(a6)
beq t3, t4, label
beq zero, zero, out

case4:
lw t3, (a6)
lw t4, -4(a6)
blt t3, t4, label
beq zero, zero, out

case5:
lw t3, (a6)
lw t4, -4(a6)
bge t3, t4, label
beq zero, zero, out

case6:
lw t3, (a6)
lw t4, -4(a6)
bltu t3, t4, label
beq zero, zero, out

case7:
lw t3, (a6)
lw t4, -4(a6)
bgeu t3, t4, label
beq zero, zero, out

out:
addi t6, zero, 0
sw t6, (a0)
beq zero, zero, ini

label:
addi t6, zero, 1
sw t6, (a0)
beq zero, zero, ini
