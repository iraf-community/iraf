|# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
|# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
|# the registers, effecting a call in the context of the procedure which
|# originally called ZSVJMP, but with the new status code.  These are Fortran
|# callable procedures.
|#
|# (SUN/UNIX MC68xxx version)
 
        .text
        .globl  _zsvjmp_

	|# The following has nothing to do with ZSVJMP, and is included here
	|# only because this assembler module is loaded with every process.
	|# This code sets the value of the symbol MEM (the Mem common) to zero,
	|# setting the origin for IRAF pointers to zero rather than some
	|# arbitrary value, and ensuring that the MEM common is aligned for
	|# all datatypes as well as page aligned.  A further advantage is that
	|# references to NULL pointers will cause a memory violation.

	.globl	_mem_
	_mem_	=	0

        JMPBUF	=       4
        STATUS  =       8

	|# The strategy here is to build on the services provided by the C
	|# setjmp/longjmp.  Note that we cannot do this by writing a C function
	|# which calls setjmp, because the procedure which calls setjmp cannot
	|# return before the longjmp is executed.

_zsvjmp_:	|# CALL ZSVJMP (JMPBUF, STATUS)
        movl    sp@(JMPBUF),a0          |# set A0 to point to jmp_buf 
        movl    sp@(STATUS),a1          |# A1 = status variable 
        movl    a1,a0@			|# JB[0] = addr of status variable
	clrl	a1@			|# return zero status
	addql	#4,sp@(JMPBUF)		|# skip first cell of jmp_buf
        jmp     _setjmp			|# let setjmp do the rest.
