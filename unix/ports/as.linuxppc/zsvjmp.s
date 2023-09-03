# ZSVJMP.S -- LinuxPPC version, August 2000.

	.file	 "zsvjmp.s"
	.ident	"NOAO/IRAF 21Aug2000 DCT"
	.section ".text"
	.align	 2

	# ZSVJMP -- SPP callable SETJMP.

	.globl	zsvjmp_
	.type	zsvjmp_,@function
zsvjmp_:
	# R3 = buf, R4 = &status
	li	11,0			# r11 = 0
	stw	11,0(4)			# set *status to zero
	stw	4,0(3)			# store &status in buf[0]
	addi	3,3,4			# reference buf[1] for sigsetjmp
	li	4,0			# zero signal mask for sigsetjmp
	b	__sigsetjmp
.Lfe1:
	.size	 zsvjmp_,.Lfe1-zsvjmp_

	# Set the address of the MEM common to zero.
	.globl   mem_
	mem_ = 0


	# GFPUCW -- Get the FPU control register.
	.align	2
	.globl	gfpucw_
	.type	gfpucw_,@function
gfpucw_:
	stwu	1, -32(1)
	stw	31, 28(1)
	mr	31, 1
	stw	3, 8(31)
	mffs	0
	stfd	0, 16(31)
	lwz	0, 20(31)
	mr	9, 0
	lwz	9, 8(31)
	stw	0, 0(9)
.L3:
	lwz	11, 0(1)
	lwz	31, -4(11)
	mr	1, 11
	blr
.Lfe2:
	.size	 gfpucw_, .Lfe2-gfpucw_


	# SFPUCW -- Set the FPU control register.

	.align	2
	.globl	sfpucw_
	.type	 sfpucw_,@function
sfpucw_:
	stwu	1, -32(1)
	stw	31, 28(1)
	mr	31, 1
	stw	3, 8(31)
	lis	0, 0xfff8
	stw	0, 16(31)
	lwz	9, 8(31)
	lwz	0, 0(9)
	stw	0, 20(31)
	lfd	0, 16(31)
	mtfsf	255, 0
.L4:
	lwz	11, 0(1)
	lwz	31, -4(11)
	mr	1, 11
	blr
.Lfe3:
	.size	 sfpucw_, .Lfe3-sfpucw_


	# CFPUCW -- Clear the exception flags in the FPU control register.
	# So far I have not been able to find a way to make this work, at
	# least with the current version of LinuxPPC.  All of the instructions
	# below fail, raising another SIGFPE if an exception condition is
	# already present.  ANY instruction involving the FPU will raise
	# SIGFPE once the exception condition exists.  Also, LinuxPPC
	# sigaction does not block SIGFPE in the called exception handler,
	# contrary to the manpage.  It appears that the exception handling
	# in the kernel needs to clear the exception condition but is not
	# doing so.  Supervisor level instructions appear to be required to
	# clear the exception condition, so this has to be done in the kernel
	# before the user level signal handler is called.

	.align	2
	.globl	cfpucw_
	.type	 cfpucw_,@function
cfpucw_:
	stwu	1, -32(1)
	stw	31, 28(1)
	mr	31, 1
	#mcrfs	0, 0
	#mtfsfi	0, 0
	#mtfsfi	3, 0
	#mtfsfi	3, 0
	#mtfsfi	5, 0
	#mtfsfb0	3
	#mtfsfb0	5
	#mtfsfb0	7
.L5:
	lwz	11, 0(1)
	lwz	31, -4(11)
	mr	1, 11
	blr
.Lfe4:
	.size	 sfpucw_, .Lfe4-cfpucw_
