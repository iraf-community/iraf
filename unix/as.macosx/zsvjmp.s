# ZSVJMP.S -- MacOS X version, September 2001, March 2002.

.file	 "zsvjmp.s"

	# ZSVJMP -- SPP callable SETJMP.
.text
	.align	2
	.globl	_zsvjmp_
_zsvjmp_:
	# R3 = buf, R4 = &status
	li	r11,0			; r11 = 0
	stw	r11,0(r4)		; set *status to zero
	stw	r4,0(r3)		; store &status in buf[0]
	addi	r3,r3,4			; reference buf[1] for setjmp
	b	L_setjmp$stub
L2:
	lwz	r1,0(r1)
	lwz	r0,8(r1)
	mtlr	r0
	lmw	r30,-8(r1)
	blr

	# The setjmp code is only available in a dynamic library on 10.1.
.picsymbol_stub
L_setjmp$stub:
        .indirect_symbol _setjmp
        mflr	r0
        bcl	20,31,L1$pb
L1$pb:
        mflr	r11
        addis	r11,r11,ha16(L1$lz-L1$pb)
        mtlr	r0
        lwz	r12,lo16(L1$lz-L1$pb)(r11)
        mtctr	r12
        addi	r11,r11,lo16(L1$lz-L1$pb)
        bctr
.lazy_symbol_pointer
L1$lz:
        .indirect_symbol _setjmp
        .long dyld_stub_binding_helper
.text
.Lfe1:

	# Set the address of the MEM common to zero.
	.globl   _mem_
	_mem_ = 0


	# GFPSCR -- Return the contents of the PowerPC FPSCR register.
.text
	.align 2
.globl _gfpscr_
_gfpscr_:
	stmw	r30,-8(r1)
	stwu	r1,-48(r1)
	mr	r30,r1
	mflr	r0
	bcl	20,31,L2$pb
L2$pb:
	mflr	r31
	mtlr	r0

	mffs	f0
	stfd	f0, 16(r30)
	lwz	r0, 20(r30)
	mr	r3, r0

	b	L3
L3:
	lwz	r1,0(r1)
	lmw	r30,-8(r1)
	blr


	# SFPSCR -- Set the contents of the PowerPC FPSCR register.
.text
	.align 2
.globl _sfpscr_
_sfpscr_:
	stmw	r30,-8(r1)
	stwu	r1,-48(r1)
	mr	r30,r1
	mflr	r0
	bcl	20,31,L4$pb
L4$pb:
	mflr	r31
	mtlr	r0

	lis	r0, 0xfff8
	stw	r0, 16(r30)
	lwz	r0, 0(r3)
	stw	r0, 20(r30)
	lfd	f0, 16(r30)
	mtfsf	255, f0

	b	L5
L5:
	lwz	r1,0(r1)
	lmw	r30,-8(r1)
	blr


	# GXER -- Return the contents of the PowerPC XER register.
.text
	.align 2
.globl _gxer_
_gxer_:
	stmw	r30,-8(r1)
	stwu	r1,-48(r1)
	mr	r30,r1
	mflr	r0
	bcl	20,31,L3$pb
L3$pb:
	mflr	r31
	mtlr	r0

	mfspr	r3,1

	b	L4
L4:
	lwz	r1,0(r1)
	lmw	r30,-8(r1)
	blr
