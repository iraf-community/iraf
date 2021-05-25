/* Copyright(c) 1986 Association of Universities for Research in Astronomy
 * Inc. (comment block, alpha, i386)
 * Copyright(c) 2008 Chisato Yamauchi (C-SODA/ISAS/JAXA) (amd64),
 * Copyright(c) 2014 David Kuehling <dvdkhlng AT posteo TOD de> (mips)
 * Copyright(c) 2014 John Long <codeblue@inbox.lv> (s390x)
 * Copyright(c) 2018 Ole Streicher (arm, x32)
 * Copyright(c) 2018 Peter Green (arm64)
 * Copyright(c) 2018 James Cowgill <jcowgill AT debian TOD org> (mips64)
 * Copyright(c) 2018 Gustavo Romero, Rogerio Cardoso, Breno Leitao,
 *                   IBM Corporation (ppc64)
 * Copyright(c) 2018 Manuel A. Fernandez Montecelo (riscv64)
 */

/* ZSVJMP, ZDOJMP -- Set up a jump (non-local goto) by saving the processor
 * registers in the buffer jmpbuf.  A subsequent call to ZDOJMP restores
 * the registers, effecting a call in the context of the procedure which
 * originally called ZSVJMP, but with the new status code.  These are Fortran
 * callable procedures.
 *
 *		zsvjmp (jmp_buf, status)	# (returns status)
 *		zdojmp (jmp_buf, status)	# (passes status to zsvjmp)
 *
 * These routines are directly comparable to the UNIX setjmp/longjmp, except
 * that they are Fortran callable kernel routines, i.e., trailing underscore,
 * call by reference, and no function returns.  ZSVJMP requires an assembler
 * jacket routine to avoid modifying the call stack, but relies upon setjmp
 * to do the real work.  ZDOJMP is implemented as a portable C routine in OS,
 * calling longjmp to do the restore.  In these routines, JMP_BUF consists
 * of one longword containing the address of the STATUS variable, followed
 * by the "jmp_buf" used by setjmp/longjmp.
 */
	.file	"zsvjmp.S"

#if defined(__aarch64__)
	.arch armv8-a
#elif defined(__ppc64__) || defined(__PPC64__) || defined(__powerpc64__)
	.abiversion 2
#elif defined (__arm__)
	.arch	armv6
#elif defined(__mips__) && (_MIPS_SIM == _ABIO32)
	.set mips1
	.abicalls
#endif
	.align 2

#if defined(__APPLE__)
        .globl	_zsvjmp_
	.text
_zsvjmp_:

#else
        .globl	zsvjmp_
	.text
	.type   zsvjmp_, STT_FUNC
zsvjmp_:

#endif

/* ***************************************************************
 * 64-bit architectures (LP64)
 * *************************************************************** */

#if defined(__x86_64__) && defined(__LP64__)

	// %rsi ... &status  %rdi ... &jumpbuf
	movq	%rsi, (%rdi)		// store &status in jmpbuf[0]
	movl	$0, (%rsi)		// zero the value of status
	addq	$8, %rdi		// change point to &jmpbuf[1]
	movl	$0, %esi		// change arg2 to zero
#if defined(__APPLE__)
	jmp     _sigsetjmp		// let sigsetjmp do the rest
#elif defined(__freebsd__)
	jmp     sigsetjmp		// let sigsetjmp do the rest
#else
	jmp	__sigsetjmp@PLT		// let sigsetjmp do the rest
#endif

#elif defined(__aarch64__) && defined(__LP64__)

	str	xzr, [x1]		// *status = 0;
	str	x1, [x0], 8		// ((long **)buf)[0] = status
	// also post-increment x0 by 8: 1st arg for sigsetjmp
	mov	w1, 0			// 0 --> 2nd arg for sigsetjmp
#if defined(__APPLE__)
	b      _sigsetjmp		// call sigsetjmp
#else
	b      __sigsetjmp		// call sigsetjmp
#endif

#elif defined(__mips__) && (_MIPS_SIM == _ABI64)

	.ent	zsvjmp_, 0
	.frame	$sp, 0, $ra
	.cpsetup $t9, $v0, zsvjmp_

	sd $a1, 0($a0)			// buf[0] = status
	sd $zero, 0($a1)		// *status = 0
	daddiu $a0, $a0, 8		// a0 = &buf[1], 1st arg for sigsetjmp
	move $a1, $zero			// a1 = 0, 2nd arg

	dla $t9, __sigsetjmp		// t9 = address of sigsetjmp
	.cpreturn			// Restore gp
	jr $t9				// Tail call sigsetjmp
	.end	zsvjmp_

#elif defined(__ppc64__) || defined(__PPC64__) || defined(__powerpc64__)

// Use VRSAVE (unused in Linux) and stack reserved area (SP+12) to
// save the return address (LR) which is lost after bl __sigsetjmp.
// External functions should be called using a 'bl/nop' pair, which is
// turned into a plt_call stub by the linker. PPC64 does not support
// 'b' function@plt anymore.

	// r3 = buf, r4 = &status
	xor   %r11, %r11, %r11		// zero r11
	std   %r11, 0(%r4)		// *status = 0
	std   %r4,  0(%r3)		// *(buf + 0) = status
	addi  %r3, %r3, 8		// r3 = buf + 8
	mr    %r4, %r11			// r4 = 0

	mflr  %r11			// save LR
	mtvrsave %r11			// LR LSB to VRSAVE
	srdi %r11, %r11, 32		// LR MSB to SP+12 (reserved)
	stw  %r11, 12(%r1)

	bl    __sigsetjmp
	nop

	lwz %r11, 12(%r1)		// restore LR
	sldi %r11, %r11, 32
	mfvrsave %r10
	add %r11, %r11, %r10
	mtlr %r11

	// Some libc can use VRSAVE as a boolean to simplify handling VMX
	// regset save/restore so it's necessary to restore it back to -1
	// (VRSAVE value is always 0xffffffff).
	li %r11, -1		// restore VRSAVE
	mtvrsave %r11

	blr

#elif defined (__riscv)

        sd      zero,0(a1)
        sd      a1,0(a0)
        li      a1,0
        addi    a0,a0,8
        tail    __sigsetjmp@plt

#elif defined(__alpha__)

	// $16=jmpbuf, $17=status
	mov	$29, $8			// save caller's global pointer
	ldgp	$29, 4($27)		// needed for setjmp reference

	stq	$17, 0($16)		// jmpbuf[0] = status
	stl	$31, 0($17)		// *status = 0
	addq	$16, 8, $16		// setjmp ignores jmpbuf[0]

	lda	$27, __sigsetjmp	// get address of setjmp
	mov	$8, $29			// restore caller's global pointer
	jmp	($27)			// branch to setjmp

#elif defined (__s390x__)

        stg     %r3,0(0,%r2)         // save contents of r3 where r2 is pointing
	xc      0(8,%r3),0(%r3)      // clear doubleword where r3 is pointing
	xgr     %r3,%r3              // clear r3
	aghi    %r2,8                // r2 <- r2 + 8
	jg      __sigsetjmp@PLT      // load vcon resolved by linker

/* ***************************************************************
 * 31-bit architectures (ILP32)
 * *************************************************************** */

#elif defined (__i386__)

	movl	4(%esp), %ecx		// &jmpbuf to ECX
	movl	8(%esp), %eax		// &status to EAX
	movl	%eax, (%ecx)		// store &status in jmpbuf[0]
	movl 	$0, (%eax)		// zero the value of status
	addl	$4, %ecx		// change stack to point to &jmpbuf[1]
	movl	%ecx, 4(%esp)		// 	...
	movl	$0, 8(%esp)		// change arg2 to zero
#if defined(__APPLE__)
	jmp	_sigsetjmp		// let setjmp do the rest
#else
	jmp	__sigsetjmp@PLT		// let sigsetjmp do the rest
#endif

#elif defined(__x86_64__) && defined(__ILP32__) // linux x32 ABI

	// %esi ... &status  edi ... &jumpbuf
	movl	%esi, (%edi)		// store &status in jmpbuf[0]
	movl	$0, (%esi)		// zero the value of status
	addl	$4, %edi		// change point to &jmpbuf[1]
	movl	$0, %esi		// change arg2 to zero
	jmp	__sigsetjmp@PLT		// let sigsetjmp do the rest

#elif defined (__arm__)

	mov	r2, #0			//
	str	r2, [r1, #0]		// *status = 0
	str	r1, [r0, #0]		// buf[0] = status
	add	r0, r0, #4		// &buf[1] --> 1st arg for sigsetjmp
	mov	r1, #0			// 0	   --> 2nd arg for sigsetjmp
	b	__sigsetjmp		// call sigsetjmp

#elif defined(__mips__) && (_MIPS_SIM == _ABIO32)

	.set noreorder
	.cpload $t9
	.set reorder
	sw  $a1, 0($a0)			// buf[0]=status
	sw  $zero, 0($a1)		// *status=0
	addiu  $a0, $a0, 4		// &buf[1] --> 1st arg for sigsetjmp
	move    $a1, $zero		// 2nd arg is zero

	// this call sequence is required when used inside shared library
	la $t9, __sigsetjmp
	j $t9
	//  note: no delay slot, filled by GAS

#else
#error "Unsupported CPU type"
#endif
