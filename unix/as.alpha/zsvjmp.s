#include <regdef.h>

 # ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
 # registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
 # the registers, effecting a call in the context of the procedure which
 # originally called ZSVJMP, but with the new status code.  These are Fortran
 # callable procedures.
 #
 # (DEC ALPHA/OSF)
  
	.text
	.align 4
	.file	2 "zsvjmp.s"
	.globl	zsvjmp_

	# The following has nothing to do with ZSVJMP, and is included here
	# only because this assembler module is loaded with every process.
	# This code sets the value of the symbol MEM (the Mem common) to zero,
	# setting the origin for IRAF pointers to zero rather than some
	# arbitrary value, and ensuring that the MEM common is aligned for
	# all datatypes as well as page aligned.  A further advantage is that
	# references to NULL pointers will cause a memory violation.

	# This is in zshlib.c and shlib/slib.s in OSF/1 IRAF.
	# .globl	mem_
	# mem_	=	0

	.ent	zsvjmp_ 2
zsvjmp_:					# a0=jmpbuf, a1=status
	mov	gp, t8				# save caller's gp
	ldgp	gp, 4($27)			# needed for setjmp reference

	stq	a1, 0(a0)			# jmpbuf[0] = status
	stl	zero, 0(a1)			# *status = 0
	addq	a0, 8, a0			# setjmp ignores jmpbuf[0]

	lda	$27, setjmp			# get address of setjmp
	mov	t8, gp				# restore caller's gp
	jmp	($27)				# branch to setjmp
	.end	zsvjmp_
