	.file	"zsvjmp.s"

/ ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
/ registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
/ the registers, effecting a call in the context of the procedure which
/ originally called ZSVJMP, but with the new status code.  These are Fortran
/ callable procedures.
/ 
/ 		zsvjmp (jmp_buf, status)	(returns status)
/ 		zdojmp (jmp_buf, status)	(passes status to zsvjmp)
/ 
/ These routines are directly comparable to the UNIX setjmp/longjmp, except
/ that they are Fortran callable kernel routines, i.e., trailing underscore,
/ call by reference, and no function returns.  ZSVJMP requires an assembler
/ jacket routine to avoid modifying the call stack, but relies upon setjmp
/ to do the real work.  ZDOJMP is implemented as a portable C routine in OS,
/ calling longjmp to do the restore.  In these routines, JMP_BUF consists
/ of one longword containing the address of the STATUS variable, followed
/ by the "jmp_buf" used by setjmp/longjmp.
/
/ This file contains the Solaris x86 version of ZSVJMP.
 
        .globl	zsvjmp_

	/ The following has nothing to do with ZSVJMP, and is included here
	/ only because this assembler module is loaded with every process.
	/ This code sets the value of the symbol MEM (the VOS or Fortran Mem
	/ common) to zero, setting the origin for IRAF pointers to zero rather
	/ than some arbitrary value, and ensuring that the MEM common is
	/ aligned for all datatypes as well as page aligned.  A further
	/ advantage is that references to NULL pointers are likely to cause a
	/ memory violation.

	.data
	.globl	mem_
	.type   mem_, @object
	.size   mem_, 0x8
	mem_	=	0

	.text
zsvjmp_:
	movl	4(%esp), %ecx		/ &jmpbuf to ECX
	movl	8(%esp), %eax		/ &status to EAX
	movl	%eax, (%ecx)		/ store &status in jmpbuf[0]
	movl 	$0, (%eax)		/ zero the value of status
	addl	$4, %ecx		/ change stack to point to &jmpbuf[1]
	movl	%ecx, 4(%esp)		/ 	...
	jmp	setjmp			/ let setjmp do the rest
