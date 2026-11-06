	.file	"main.c"
	.option nopic
	.attribute arch, "rv32i2p1"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.globl	address
	.section	.sdata,"aw"
	.align	2
	.type	address, @object
	.size	address, 4
address:
	.word	-244
	.text
	.align	2
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-2032
	sw	s0,2028(sp)
	addi	s0,sp,2032
	addi	sp,sp,-2000
	sw	zero,-20(s0)
	j	.L2
.L3:
	lw	a5,-20(s0)
	addi	a4,a5,1
	li	a5,-4096
	addi	a5,a5,-16
	add	a3,a5,s0
	lw	a5,-20(s0)
	slli	a5,a5,2
	add	a5,a3,a5
	sw	a4,84(a5)
	lw	a5,-20(s0)
	addi	a5,a5,1
	sw	a5,-20(s0)
.L2:
	lw	a4,-20(s0)
	li	a5,999
	ble	a4,a5,.L3
	sw	zero,-24(s0)
	sw	zero,-28(s0)
	j	.L4
.L5:
	li	a5,-4096
	addi	a5,a5,-16
	add	a4,a5,s0
	lw	a5,-28(s0)
	slli	a5,a5,2
	add	a5,a4,a5
	lw	a5,84(a5)
	lw	a4,-24(s0)
	add	a5,a4,a5
	sw	a5,-24(s0)
	lw	a5,-28(s0)
	addi	a5,a5,1
	sw	a5,-28(s0)
.L4:
	lw	a4,-28(s0)
	li	a5,999
	ble	a4,a5,.L5
	lui	a5,%hi(address)
	lw	a5,%lo(address)(a5)
	lw	a4,-24(s0)
	sw	a4,0(a5)
	li	a5,0
	mv	a0,a5
	addi	sp,sp,2000
	lw	s0,2028(sp)
	addi	sp,sp,2032
	jr	ra
	.size	main, .-main
	.ident	"GCC: (xPack GNU RISC-V Embedded GCC x86_64) 13.2.0"
