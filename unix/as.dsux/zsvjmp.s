 # ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
 # registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
 # the registers, effecting a call in the context of the procedure which
 # originally called ZSVJMP, but with the new status code.  These are Fortran
 # callable procedures.
 #
 # (DECstation/MIPS version)
 
	# The following has nothing to do with ZSVJMP, and is included here
	# only because this assembler module is loaded with every process.
	# This code sets the value of the symbol MEM (the Mem common) to zero,
	# setting the origin for IRAF pointers to zero rather than some
	# arbitrary value, and ensuring that the MEM common is aligned for
	# all datatypes as well as page aligned.  A further advantage is that
	# references to NULL pointers will cause a memory violation.

	# .globl	mem_
	# mem_	=	0

	.text	
	.align	2
	.globl	zsvjmp_
	.ent	zsvjmp_ 2
	.set	noreorder

	# Upon entry, r4 = &jmpbuf, r5 = &status.
zsvjmp_:
	sw	$5, 0($4)		# save &status in jmpbuf[0]
	sw	$0, 0($5)		# zero the output status
	j	setjmp
	addu	$4, $4, 4
	.end	zsvjmp_
