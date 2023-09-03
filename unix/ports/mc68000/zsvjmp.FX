|# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
|# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
|# the registers, effecting a call in the context of the procedure which
|# originally called ZSVJMP, but with the new status code.  These are Fortran
|# callable procedures.
|#
|# (Alliant/UNIX MC68xxx version)
 
        .text
        .globl  _zsvjmp_
        .globl  _zdojmp_

	|# The following has nothing to do with ZSVJMP, and is included here
	|# only because this assembler module is loaded with every process.
	|# This code sets the value of the symbol MEM (the Mem common) to zero,
	|# setting the origin for IRAF pointers to zero rather than some
	|# arbitrary value, and ensuring that the MEM common is aligned for
	|# all datatypes as well as page aligned.  A further advantage is that
	|# references to NULL pointers will cause a memory violation.

	.globl	_mem_
	_mem_	=	0

_zsvjmp_:				|# call zsvjmp (jmpbuf, status)
	movl	a0@(4), sp@-		|# save pointer to status variable
	movl	a0@, sp@-		|# save pointer to jmpbuf

	movl	a0@, sp@-		|# call alliant setcontext proc
	movl	sp, a0
	pea	1
	jsr	_setcontext
	addql	#8, sp

	movl	sp@+, a0		|# A0 = &jmpbuf
	movl	sp@+, a1		|# A1 = &status

	movl	a1, a0@(2240)		|# save &status in jmpbuf
	clrl	a1@			|# set status to zero
	lea	sp@(4), a1
	movl	a1, a0@(68)
	movl	sp@, a0@(74)
	rts

_zdojmp_:
	movl	a0@, a1			|# A1 = &jmpbuf
	movl	a1@(2240), a2		|# A2 = &status
	movl	a0@(4), a3		|# pointer to status value
	movl	a3@, a2@		|# set status value
	jsr	_resetcontext
