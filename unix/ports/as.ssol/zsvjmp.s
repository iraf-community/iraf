!# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
!# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
!# the registers, effecting a call in the context of the procedure which
!# originally called ZSVJMP, but with the new status code.  These are Fortran
!# callable procedures.
!#
!# (SUN/UNIX sparc version)
 
        .seg	"text"
        .global	zsvjmp_

	!# The following has nothing to do with ZSVJMP, and is included here
	!# only because this assembler module is loaded with every process.
	!# This code sets the value of the symbol MEM (the Mem common) to zero,
	!# setting the origin for IRAF pointers to zero rather than some
	!# arbitrary value, and ensuring that the MEM common is aligned for
	!# all datatypes as well as page aligned.  A further advantage is that
	!# references to NULL pointers will cause a memory violation.

	.global	_mem_
	_mem_	=	0

	.proc	0
zsvjmp_:
	st	%o1, [%o0]		! save &status in jmpbuf[0]
	clr	%o2
	st	%o2, [%o1]		! zero the value of status
	add	%o0, 0x4, %o0
	set	setjmp, %o1
	jmp	%o1
	nop
	.seg	"data"
