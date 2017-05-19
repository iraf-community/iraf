	.file	"zsvjmp.s"

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
#
 
        .globl	zsvjmp_
	.type   zsvjmp_, @function

	# The following has nothing to do with ZSVJMP, and is included here
	# only because this assembler module is loaded with every process.
	# This code sets the value of the symbol MEM (the VOS or Fortran Mem
	# common) to zero, setting the origin for IRAF pointers to zero
	# rather than some arbitrary value, and ensuring that the MEM common
	# is aligned for all datatypes as well as page aligned.  A further
	# advantage is that references to NULL pointers are likely to cause a
	# memory violation.

	.globl  _mem_
	_mem_   =       0

zsvjmp_:
	# %rsi ... &status  %rdi ... &jumpbuf
	movq    %rsi, (%rdi)    # store &status in jmpbuf[0]
	movl    $0, (%rsi)      # zero the value of status
	addq    $8, %rdi        # change point to &jmpbuf[1]
	movl    $0, %esi        # change arg2 to zero
	jmp     __sigsetjmp@PLT # let sigsetjmp do the rest

	.section        .note.GNU-stack,"",@progbits
