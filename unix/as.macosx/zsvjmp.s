        .file "zsvjmp.S"

# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
# the registers, effecting a call in the context of the procedure which
# originally called ZSVJMP, but with the new status code. These are Fortran
# callable procedures.
#
#               zsvjmp (jmp_buf, status)        # (returns status)
#               zdojmp (jmp_buf, status)        # (passes status to zsvjmp)
#
# These routines are directly comparable to the UNIX setjmp/longjmp, except
# that they are Fortran callable kernel routines, i.e., trailing underscore,
# call by reference, and no function returns.  ZSVJMP requires an assembler
# jacket routine to avoid modifying the call stack, but relies upon setjmp
# to do the real work.  ZDOJMP is implemented as a portable C routine in OS,
# calling longjmp to do the restore.  In these routines, JMP_BUF consists
# of one longword containing the address of the STATUS variable, followed
# by the "jmp_buf" used by setjmp/longjmp.

 .arch armv8-a
 .align 2
        .globl _zsvjmp_

        .text
_zsvjmp_:
        str xzr, [x1]           // zero the value of status
        str x1, [x0], 8         // change stack to point to &jmpbuf[1]
        mov w1, 0               // change arg2 to zero

        b _sigsetjmp            // let sigsetjmp do the rest
