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

/*
asm("# Set the address of the MEM common to zero.");
asm(".globl	mem_");
asm("	mem_	=	0");
*/

/*
#if defined(MACOSX)
asm(".globl	_mem_");
asm("	_mem_	=	0");
#endif
*/

#if defined(LINUX)
asm(".text");
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
#endif	/* LINUX */

#if (defined(CYGWIN) || defined(MACOSX))
asm(".text");
asm(".globl	_zsvjmp_");
asm("_zsvjmp_:");
asm("	movl	4(%esp), %ecx	# &jmpbuf to ECX");
asm("	movl	8(%esp), %eax	# &status to EAX");
asm("	movl	%eax, (%ecx)	# store &status in jmpbuf[0]");
asm("	movl 	$0, (%eax)	# zero the value of status");
asm("	addl	$4, %ecx	# change stack to point to &jmpbuf[1]");
asm("	movl	%ecx, 4(%esp)	# 	...");
asm("	movl	$0, 8(%esp)	# change arg2 to zero");
asm("	jmp	_setjmp		# let setjmp do the rest");
#endif	/* CYGWIN || MACOSX */

#endif	/* I386 */


#ifdef X86_64

/*
asm("# Set the address of the MEM common to zero.");
asm(".globl	mem_");
asm("	mem_	=	0");
*/

#ifdef SPP_LP64
/* LP64-SPP */
asm(".text");
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
asm(".text");
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


#ifdef POWERPC

#if defined(LINUX)
/*
asm("	# Set the address of the MEM common to zero.");
asm("	.globl   mem_");
asm("	mem_ = 0");
*/
asm(".text");
asm("	# ZSVJMP -- SPP callable SETJMP.");
asm("	.align	 2");
asm("	.globl	zsvjmp_");
asm("	.type	zsvjmp_,@function");
asm("zsvjmp_:");
asm("	# R3 = buf, R4 = &status");
asm("	li	r11,0		# r11 = 0");
asm("	stw	r11,0(r4)	# set *status to zero");
asm("	stw	r4,0(r3)	# store &status in buf[0]");
asm("	addi	r3,r3,4		# reference buf[1] for sigsetjmp");
asm("	li	r4,0		# zero signal mask for sigsetjmp");
asm("	b	__sigsetjmp");
#endif	/* LINUX */

#if defined(MACOSX)
/*
asm("	# Set the address of the MEM common to zero.");
asm("	.globl   _mem_");
asm("	_mem_ = 0");
*/
asm(".text");
asm("	# ZSVJMP -- SPP callable SETJMP.");
asm("	.align	2");
asm("	.globl	_zsvjmp_");
asm("_zsvjmp_:");
asm("	# R3 = buf, R4 = &status");
asm("	li	r11,0		# r11 = 0");
asm("	stw	r11,0(r4)	# set *status to zero");
asm("	stw	r4,0(r3)	# store &status in buf[0]");
asm("	addi	r3,r3,4		# reference buf[1] for setjmp");
asm("	b	L_setjmp$stub");
asm("L2:");
asm("	lwz	r1,0(r1)");
asm("	lwz	r0,8(r1)");
asm("	mtlr	r0");
asm("	lmw	r30,-8(r1)");
asm("	blr");
asm("");
asm("	# The setjmp code is only available in a dynamic library on 10.1.");
asm(".picsymbol_stub");
asm("L_setjmp$stub:");
asm("        .indirect_symbol _setjmp");
asm("        mflr	r0");
asm("        bcl	20,31,L1$pb");
asm("L1$pb:");
asm("        mflr	r11");
asm("        addis	r11,r11,ha16(L1$lz-L1$pb)");
asm("        mtlr	r0");
asm("        lwz	r12,lo16(L1$lz-L1$pb)(r11)");
asm("        mtctr	r12");
asm("        addi	r11,r11,lo16(L1$lz-L1$pb)");
asm("        bctr");
asm(".lazy_symbol_pointer");
asm("L1$lz:");
asm("        .indirect_symbol _setjmp");
asm("        .long dyld_stub_binding_helper");
#endif	/* MACOSX */

#endif	/* POWERPC */
