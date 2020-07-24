/* Copyright(c) 1986 Association of Universities for Research in Astronomy
 * Inc. (i386),
 * Copyright(c) 2018 Ole Streicher (arm, x32)
 * Copyright(c) 2014 David Kuehling <dvdkhlng AT posteo TOD de> (mips)
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
	
#if defined (__arm__)
	.arch	armv6
#elif defined(__mips__)
	.set mips1
	.abicalls
#endif

        .globl	zsvjmp_
	.text
	.type   zsvjmp_, STT_FUNC

zsvjmp_:

#if defined (__i386__)

	movl	4(%esp), %ecx		// &jmpbuf to ECX
	movl	8(%esp), %eax		// &status to EAX
	movl	%eax, (%ecx)		// store &status in jmpbuf[0]
	movl 	$0, (%eax)		// zero the value of status
	addl	$4, %ecx		// change stack to point to &jmpbuf[1]
	movl	%ecx, 4(%esp)		// 	...
	movl	$0, 8(%esp)		// change arg2 to zero
	jmp	__sigsetjmp@PLT		// let sigsetjmp do the rest

#elif defined(__x86_64__)		// linux x32 ABI

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

#elif defined(__mips__)

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

	.size	zsvjmp_,.-zsvjmp_
