add t3, x0, sp
addi s0, zero, 1
slli s0, s0, 12
addi s0, s0, 4
slli s0, s0, 16
addi s0, s0, 1024  # 0x1004_0400 -> push
addi s1, s0, 1024  # 0x1004_0800 -> pop

# 40000 interval
addi s7, x0, 0
slli s7, s7, 16
addi s7, s7, 1792

li a7, 1
mv a0, s7
ecall

# Fib
case7:
li a7, 5 
ecall
mv t0, a0
beq t0, zero, case7

li a7, 5
ecall
mv t1, a0

addi t2, x0, 0 # t2: counter
addi t3, a6, 0 # t3: stack
addi t4, x0, 1 # t4 = 1
addi t5, x0, 2 # t5 = 2 
add s2, x0, s0 # s2: push  0x0001_0400 -> push
add s3, x0, s1 # s3: pop   0x0001_0800 -> pop
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
mv a0, t2
li a7, 1
ecall

waitconfirm: # 等待输出入栈出栈数据
li a7, 5
ecall
mv t0, a0
beq t0, zero, waitconfirm
li a7, 5
ecall
mv t1, a0
beq t1, zero, print_push # print push

print_pop:
add s4, x0, s1
pop_loop:
beq s4, s3, waitconfirm
lw s5, 0(s4)
li a7, 1
mv a0, s5
ecall
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
li a7, 34
mv a0, s4
# ecall
li a7, 34
mv a0, s2
# ecall
beq s4, s2, waitconfirm
lw s5, 0(s4)
li a7, 1
mv a0, s5
ecall
addi s4, s4, 4

# 40000 interval
addi s6, x0, 0
while_push:
li a7, 1
mv a0, s6
# ecall
addi s6, s6, 1
bne s6, s7, while_push

beq x0, x0, push_loop