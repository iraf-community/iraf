;# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
;# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
;# the registers, effecting a call in the context of the procedure which
;# originally called ZSVJMP, but with the new status code.  These are Fortran
;# callable procedures.

	.globl	_zsvjmp_

	;# The following has nothing to do with ZSVJMP, and is included here
	;# only because this assembler module is loaded with every process.
	;# This code sets the value of the symbol MEM (the Mem common) to zero,
	;# setting the origin for IRAF pointers to zero rather than some
	;# arbitrary value, and ensuring that the MEM common is aligned for
	;# all datatypes as well as page aligned.  A further advantage is that
	;# references to NULL pointers will cause a memory violation.

	.bss
	;.globl	__mem_
	;.align	4096
	.comm	__mem_,8
	__mem_	= 0x80000000

	.text
	.align	1
_zsvjmp_:
	ld.w	0(ap),a1		;# A1 = addr of jmp_buf
	ld.w	4(ap),s0		;# S0 = addr of status variable
	st.w	s0,0(a1)		;# JB[0] = addr of status variable
	add.w	#4,a1			;# skip first word of jmp_buf
	st.w	a1,0(ap)		;# edit arg pointer
	mov	s0,a1
	ld.w	#0,s0
	st.w	s0,0(a1)		;# status = zero
	jmp	_setjmp			;# let setjmp do the rest
