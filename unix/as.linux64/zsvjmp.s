# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
#
# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
# the registers, effecting a call in the context of the procedure which
# originally called ZSVJMP, but with the new status code.  These are Fortran
# callable procedures.
#
#		zsvjmp (jmp_buf, status)	# (returns status)
#		zdojmp (jmp_buf, status)	# (passes status to zsvjmp)
#
# These routines are directly comparable to the UNIX setjmp/longjmp, except
# that they are Fortran callable kernel routines, i.e., trailing underscore,
# call by reference, and no function returns.  ZSVJMP requires an assembler
# jacket routine to avoid modifying the call stack, but relies upon setjmp
# to do the real work.  ZDOJMP is implemented as a portable C routine in OS,
# calling longjmp to do the restore.  In these routines, JMP_BUF consists
# of one longword containing the address of the STATUS variable, followed
# by the "jmp_buf" used by setjmp/longjmp.

	.file	"zsvjmp.s"

        .globl	zsvjmp_

zsvjmp_:
	movq	%rdi, -8(%rbp)          # %rdi contains &jmpbuf
	movq	%rsi, -16(%rbp)         # %rsi contains &status
	movl    $0, (%rsi)              # zero the value of status
	movq    %rsi, (%rdi)            # store address of status in jmpbuf[0]
	addq    $8, %rdi                # change &jmpbuf[0] to &jmpbuf[1]
	movl    $0, -16(%rbp)           # zero the second argument of jmpbuf

	jmp     __sigsetjmp             # use sigsetjmp to do the rest

