# initialize the data address
addi a0, zero, -256 # 0xffff_ff00 -> led * 24
addi a1, a0, 4      # 0xffff_ff04 -> btn * 5
addi a2, a0, 8      # 0xffff_ff08 -> swi * 24
addi a3, a0, 12     # 0xffff_ff0c -> 7seg
addi a6, zero, 1
slli a6, a6, 16
srli t0, a0, 16
add a6, a6, t0		# 0x0001_ffff -> stack
addi s0, zero, 1
slli s0, s0, 16
addi s0, s0, 1024  # 0x0001_0400 -> push
addi s1, s0, 1024  # 0x0001_0800 -> pop
addi a4, zero, 7
slli a4, a4, 20     # mask of the cases
addi a7, zero, 1
slli a7, a7, 16 	# get switch 16

# 40000 interval (waiting for test)
addi s7, x0, 61
slli s7, s7, 16
addi s7, s7, 1792

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



# Calculate the number of zeros
case0:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case0
lw t0, (a2)
andi t0, t0, 255
addi t2, zero, 8 # counter
c0_loop:
	beq t0, zero, c0_out
	srli t0, t0, 1
	addi t2, t2, -1
	beq zero, zero, c0_loop
c0_out:
sw t2, (a0)
sw t2, (a3)
beq zero, zero, ini



# Round up
case1:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case1
lw t0, (a2) # the original float-16 unmber
andi t1, t0, 1023 # fraction
srli t2, t0, 10
andi t2, t2, 31
addi t2, t2, -15 # exponent
srli t3, t0, 15
andi t3, t3, 1 # sign
blt t2, zero, case1_end1 # exp < 0
# shift
ori t1, t1, 1024
addi t2, t2, -10
blt t2, zero, case1_small # exp < 10
sll t4, t1, t2
beq zero, zero, case1_end2

case1_small: # exp < 10
not t2, t2
addi t2, t2, 1
srl t4, t1, t2 # round down
not t5, t4
addi t5, t5, 1
sll t5, t5, t2
add t5, t1, t5 # origin number - t4
beq t3, zero, case1_pass
not t4, t4
addi t4, t4, 1
case1_pass:
beq zero, t5, case1_end2 # fraction part = 0
addi t4, t4, 1

case1_end2: # exp >= 0
sw t4, (a0)
sw t4, (a3)
beq zero, zero, ini

case1_end1: # exp < 0
xori t3, t3, 1
sw t3, (a0)
sw t3, (a3)
beq zero, zero, ini



# Round down
case2:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case2
lw t0, (a2) # the original float-16 unmber
andi t1, t0, 1023 # fraction
srli t2, t0, 10
andi t2, t2, 31
addi t2, t2, -15 # exponent
srli t3, t0, 15
andi t3, t3, 1 # sign
blt t2, zero, case2_end1 # exp < 0
# shift
ori t1, t1, 1024
addi t2, t2, -10
blt t2, zero, case2_small # exp < 10
sll t4, t1, t2
beq zero, zero, case2_end2

case2_small: # exp < 10
not t2, t2
addi t2, t2, 1
srl t4, t1, t2 # round down
beq t3, zero, case2_end2
not t4, t4
addi t4, t4, 1

case2_end2: # exp >= 0
sw t4, (a0)
sw t4, (a3)
beq zero, zero, ini

case2_end1: # exp < 0
xori t3, t3, 1
addi t3, t3, -1
sw t3, (a0)
sw t3, (a3)
beq zero, zero, ini




# Round to the nearest whole number
case3:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case3
lw t0, (a2) # the original float-16 unmber
andi t1, t0, 1023 # fraction
srli t2, t0, 10
andi t2, t2, 31
addi t2, t2, -15 # exponent
srli t3, t0, 15
andi t3, t3, 1 # sign
blt t2, zero, case3_end1 # exp < 0
# shift
ori t1, t1, 1024
addi t2, t2, -10
blt t2, zero, case3_small # exp < 10
sll t4, t1, t2
beq zero, zero, case3_end2

case3_small: # exp < 10
not t2, t2
addi t2, t2, 1
srl t4, t1, t2 # round down
not t5, t4
addi t5, t5, 1
sll t5, t5, t2
add t5, t1, t5 # origin number - t4
slli t5, t5, 1
srl t5, t5, t2 # t5 = fraction part >= 0.5 ? 1 : 0
beq t3, zero, case3_pass
not t4, t4
addi t4, t4, 1
case3_pass:
beq zero, t5, case3_end2 # fraction part < 0.5
addi t4, t4, 1

case3_end2: # exp >= 0
sw t4, (a0)
sw t4, (a3)
beq zero, zero, ini

case3_end1: # exp < 0 here there will be something wrong
not t2, t2
bne t2, zero, case3_end1_other
xori t3, t3, 1
sw t3, (a0)
sw t3, (a3)
beq zero, zero, ini
case3_end1_other:
addi t3, zero, 0
sw t3, (a0)
sw t3, (a3)





# Add two numbers
case4:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case4
lw t1, (a2)
andi t1, t1, 255 # a
check0c4:
lw t0, (a1)
andi t0, t0, 4
bne t0, zero, check0c4
	case41:
	lw t0, (a1)
	andi t0, t0, 4
	beq t0, zero, case41
	lw t2, (a2)
	andi t2, t2, 255 # b
add t3, t1, t2
srli t4, t3, 8
beq t4, zero, c4_out
addi t3, t3, 1
not t3, t3
c4_out:
sw t3, (a0)
sw t3, (a3)
beq zero, zero, ini




# little-endian -> big endian
case5:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case4
lw t0, (a2)
addi t1, zero, 255 # mask
and t2, t0, t1
slli t1, t1, 8
and t3, t0, t1
slli t2, t2, 4
srli t3, t3, 12
or t0, t2, t3
sw t0, (a0)
sw t0, (a3)
beq zero, zero, ini



# Judge if is the power of 2, notice here 0 is not the power of 2, but 1 is
case6:
lw t0, (a1)
andi t0, t0, 4
beq t0, zero, case6
lw t1, (a2)
andi t1, t1, 255 # a
beq t1, zero, c6_no # 0 is not the power of 2
addi t1, t1, -1
beq t1, zero, c6_yes # 1 is the power of 2
andi t1, t1, 1
addi t1, t1, -1
beq t1, zero, c6_yes
bne t1, zero, c6_no
c6_yes:
addi t3, zero, -1
sw t3, (a0)
beq zero, zero, ini
c6_no:
addi t3, zero, 0
sw t3, (a0)
beq zero, zero, ini

# Fib
case7:
lw t0, (a1) # load button
andi t0, t0, 4
beq t0, zero, case7
lw t1, (a2) # load switches

andi t1, t1, 255 # t1: input
addi t2, x0, 0 # t2: counter
addi t3, a6, 0 # t3: stack
addi t4, x0, 1 # t4 = 1
addi t5, x0, 2 # t5 = 2 
add s2, x0, s0 # s2: push
add s3, x0, s1 # s3: pop
loop:
addi t2, t2, 1
jal fib # go to fib
blt t6, t1, loop
beq x0, x0, out

fib: # output: t6
addi t3, t3, -12
sw ra, 0(t3)
sw t2, 4(t3) # push
sw t2, 0(s2) # store push
addi s2, s2, 4

bgt t2, t5, call_add
addi t6, x0, 1

lw t2, 4(t3)
sw t2, 0(s3) # store pop
addi s3, s3, 4

addi t3, t3, 12
jr ra

call_add:
addi t2, t2, -1
jal fib
sw t6, 8(t3)
addi t2, t2, -1
jal fib
lw t2, 8(t3)
add t6, t6, t2

lw ra, 0(t3)
lw t2, 4(t3)
sw t2, 0(s3) # store pop
addi s3, s3, 4
addi t3, t3, 12
jr ra

out:
addi t2, t2, -1
sw t2, 0(a3) # seg7

waitconfirm: # print push & pop list
lw t0, (a1)
andi t1, t0, 1 # button 0
bne t1, zero, ini
andi t0, t0, 2 # button 1
beq t0, zero, waitconfirm
lw t1, (a2)
and t1, t1, a7 # get switch 16
beq t1, zero, print_push # print push

print_pop:
add s4, x0, s1 # s4: pop begin
pop_loop:
beq s4, s3, clearseg7
lw s5, 0(s4)
sw s5, 0(a3) # to seg7
addi s4, s4, 4

# 40000 interval
addi s6, x0, 0
while_pop:
addi s6, s6, 1
bne s6, s7, while_pop

beq x0, x0, pop_loop

print_push:
add s4, x0, s0 # s4: push begin
push_loop:
beq s4, s2, clearseg7
lw s5, 0(s4)
sw s5, 0(a3) # to seg7
addi s4, s4, 4

# 40000 interval
addi s6, x0, 0
while_push:
addi s6, s6, 1
bne s6, s7, while_push

beq x0, x0, push_loop

clearseg7:
addi t0, x0, 0
sw t0, 0(a3)
beq x0, x0, waitconfirm
