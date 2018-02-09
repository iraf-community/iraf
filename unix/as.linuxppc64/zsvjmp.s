
# Copyright 2018 Gustavo Romero, Rogerio Cardoso, Breno Leitao, IBM Corporation.

	.file "zsvjmp.s"
	.abiversion 2
	.section ".text"
	.align 2
	.globl zsvjmp_
	.type zsvjmp_, @function


# Use VRSAVE (unused in Linux) and stack reserved area (SP+12) to save the
# return address (LR) which is lost after bl __sigsetjmp. External functions
# should be called using a 'bl/nop' pair, which is turned into a plt_call stub
# by the linker. PPC64 does not support 'b' function@plt anymore.

zsvjmp_:
	# r3 = buf, r4 = &status

	xor   %r11, %r11, %r11  # zero r11
	std   %r11, 0(%r4)      # *status = 0
	std   %r4,  0(%r3)      # *(buf + 0) = status
	addi  %r3, %r3, 8       # r3 = buf + 8
	mr    %r4, %r11         # r4 = 0

	mflr  %r11              # save LR
	mtvrsave %r11           # LR LSB to VRSAVE
	srdi %r11, %r11, 32     # LR MSB to SP+12 (reserved)
	stw  %r11, 12(%r1)

	bl    __sigsetjmp
	nop

	lwz %r11, 12(%r1)       # restore LR
	sldi %r11, %r11, 32
	mfvrsave %r10
	add %r11, %r11, %r10
	mtlr %r11

	# Some libc can use VRSAVE as a boolean to simplify handling VMX
	# regset save/restore so it's necessary to restore it back to -1
	# (VRSAVE value is always 0xffffffff).
	li %r11, -1             # restore VRSAVE
	mtvrsave %r11

	blr
	.size	zsvjmp_,.-zsvjmp_
	.section	.note.GNU-stack,"",@progbits
