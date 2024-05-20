# initialize the data address
addi a0, zero, -256 # 0xffff_ff00 -> led * 24
addi a1, a0, 4      # 0xffff_ff04 -> btn * 5
addi a2, a0, 8      # 0xffff_ff08 -> swi * 24
addi a3, a0, 12     # 0xffff_ff0c -> 7seg
addi a4, zero, 14
slli a4, a4, 20     # mask of the cases

# determine the cases
situation:
addi a4, zero, 0
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, situation
lw t1, (a2)
and t1, t1, a4
srli t1, t1, 21
beq a4, t1, case0

addi a4, a4, 1
beq a4, t1, case1
addi a4, a4, 1
beq a4, t1, case2
addi a4, a4, 1
beq a4, t1, case3
addi a4, a4, 1
beq a4, t1, case4
addi a4, a4, 1
beq a4, t1, case5
addi a4, a4, 1
beq a4, t1, case6
addi a4, a4, 1
beq a4, t1, case7
beq zero, zero, situation 

case0:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case0
addi a5, zero, 255
slli a5, a5, 8
lw t3, (a2)
and t3, t3, a5
lw t3, (a0)
	case01:
	lw t0, (a1)
	andi t0, t0, 4
	beq t0, zero, case01
	addi a5, zero, 255
	lw t4, (a2)
	and t4, t4, a5
	or t3, t3, t4
	sw t3, (a0)
	beq zero, zero, situation
	

case1:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case1
lb t3, 2(a2) # �����п��ܻ������� -> ��Ӧ�ľ�����ֽڵĵ�ַ
sw t3, (a3) # 7-seg
sw t3, 16(a0)
beq zero, zero, situation

case2:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case2
lbu t4, 1(a2) # �����п��ܻ������� -> ��Ӧ�ľ�����ֽڵĵ�ַ
sw t4, (a3) # 7-seg
sw t4, 20(a0)
beq zero, zero, situation

case3:
beq t3, t4, label
beq zero, zero, situation

case4:
blt t3, t4, label
beq zero, zero, situation

case5:
bge t3, t4, label
beq zero, zero, situation

case6:
bltu t3, t4, label
beq zero, zero, situation

case7:
bgeu t3, t4, label
beq zero, zero, situation

label:
addi t6, zero, 1
sw t6, (a0)
beq zero, zero, situation
