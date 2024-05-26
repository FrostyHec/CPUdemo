sys_ebreak:
# 前面需要把用过的寄存器全部存进内存里面，然后后面统一恢复
# addi sp, sp, -24
# sw a1, 4(sp)
# sw a2, 8(sp)
# sw a3, 12(sp)
# sw a4, 16(sp)
# sw t0, 20(sp)