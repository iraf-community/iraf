/*
# ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
# registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
# the registers, effecting a call in the context of the procedure which
# originally called ZSVJMP, but with the new status code.  These are Fortran
# callable procedusres.
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
*/

#ifdef I386
asm(".globl	zsvjmp_");
asm("zsvjmp_:");
asm("	movl	4(%esp), %ecx	# &jmpbuf to ECX");
asm("	movl	8(%esp), %eax	# &status to EAX");
asm("	movl	%eax, (%ecx)	# store &status in jmpbuf[0]");
asm("	movl 	$0, (%eax)	# zero the value of status");
asm("	addl	$4, %ecx	# change stack to point to &jmpbuf[1]");
asm("	movl	%ecx, 4(%esp)	# 	...");
asm("	movl	$0, 8(%esp)	# change arg2 to zero");
asm("	jmp	__sigsetjmp	# let sigsetjmp do the rest");
#endif	/* I386 */

#ifdef X86_64

#ifdef SPP_LP64
/* LP64-SPP */
asm(".globl	zsvjmp_");
asm("zsvjmp_:");
asm("	# %rsi ... &status  %rdi ... &jumpbuf");
asm("	movq	%rsi, (%rdi)	# store &status in jmpbuf[0]");
asm("	movl	$0, (%rsi)	# zero the value of status");
asm("	addq	$8, %rdi	# change point to &jmpbuf[1]");
asm("	movl	$0, %esi	# change arg2 to zero");
asm("	jmp	__sigsetjmp	# let sigsetjmp do the rest");
#else
/* ILP64-SPP */
asm(".globl	zsvjmp_");
asm("zsvjmp_:");
asm("	# %rsi ... &status  %rdi ... &jumpbuf");
asm("	movq	%rsi, (%rdi)	# store &status in jmpbuf[0]");
asm("	movq	$0, (%rsi)	# zero the value of status");
asm("	addq	$8, %rdi	# change point to &jmpbuf[1]");
asm("	movl	$0, %esi	# change arg2 to zero");
asm("	jmp	__sigsetjmp	# let sigsetjmp do the rest");
#endif

#endif	/* X86_64 */
