# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
# the registers, effecting a call in the context of the procedure which
# originally called ZSVJMP, but with the new status code.  These are Fortran
# callable procedures.

        .globl  _zsvjmp_

        # The following has nothing to do with ZSVJMP, and is included here
        # only because this assembler module is loaded with every process.
        # This code sets the value of the symbol MEM (the Mem common) to zero,
        # setting the origin for IRAF pointers to zero rather than some
        # arbitrary value, and ensuring that the MEM common is aligned for
        # all datatypes as well as page aligned.  A further advantage is that
        # references to NULL pointers will cause a memory violation.

        .globl  _mem_
        .set    _mem_,  0

        .set    JMPBUF, 4
        .set    STATUS, 8

        # The strategy here is to build on the services provided by the C
        # setjmp/longjmp.  Note that we cannot do this by writing a C function
        # which calls setjmp, because the procedure which calls setjmp cannot
        # return before the longjmp is executed (we want to return to the caller        # of the routine containing the setjmp call, not the routine itself).

        .align  1
_zsvjmp_:                               # CALL ZSVJMP (JMPBUF, STATUS)
        .word   0x0
        movl    STATUS(ap),*JMPBUF(ap)  # jmp_buf[0] = addr of status variable
        clrl    *STATUS(ap)             # return zero status
        addl2   $4, JMPBUF(ap)          # skip first cell of jmp_buf
        movl    $1, (ap)                # SETJMP (JMP_BUF)
        jmp     _setjmp+2               # let setjmp do the rest.
