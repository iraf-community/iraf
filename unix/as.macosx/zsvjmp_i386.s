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
# This file contains the OS X Intel (x86) version of ZSVJMP.
# Modified to remove leading underscore for ELF (Jan99).
 
        .globl	_zsvjmp_
        .globl	_sfpucw_
        .globl	_gfpucw_
        .globl	_gfpusw_

	# The following has nothing to do with ZSVJMP, and is included here
	# only because this assembler module is loaded with every process.
	# This code sets the value of the symbol MEM (the VOS or Fortran Mem
	# common) to zero, setting the origin for IRAF pointers to zero
	# rather than some arbitrary value, and ensuring that the MEM common
	# is aligned for all datatypes as well as page aligned.  A further
	# advantage is that references to NULL pointers are likely to cause a
	# memory violation.

	.globl	mem_
	mem_	=	0
	.globl	_mem_
	_mem_	=	0

	.text
_zsvjmp_:
	movl	4(%esp), %edx	# &jmpbuf to EDX
	movl	8(%esp), %eax	# &status to EAX
	movl	%eax, (%edx)	# store value-of &status in &jmpbuf[0]
	movl	$0, (%eax)	# zero the value of status
	addl	$4, %edx	# change stack to point to &jmpbuf[1]
	movl	%edx, 4(%esp)
	jmp	L_setjmp$stub
	leave
	ret
_gfpucw_:				# Get fpucw:  gfpucw_ (&cur_fpucw)
	pushl   %ebp
	movl    %esp,%ebp
	subl    $0x4,%esp
	movl    0x8(%ebp), %eax
	fnstcw  0xfffffffe(%ebp)
	movw    0xfffffffe(%ebp), %dx
	movl    %edx,(%eax)
	movl    %ebp, %esp
	popl    %ebp
	ret

_sfpucw_:				# Set fpucw:  sfpucw_ (&new_fpucw)
	pushl   %ebp
	movl    %esp,%ebp
	subl    $0x4,%esp
	movl    0x8(%ebp), %eax
	movl    (%eax), %eax
	andl    $0xf3f, %eax
	fclex
	movw    %ax, 0xfffffffe(%ebp)
	fldcw   0xfffffffe(%ebp)
	leave  
	ret    

_gfpusw_:				# Get fpusw:  gfpusw_ (&cur_fpusw)
	pushl   %ebp
	movl    %esp,%ebp
	subl    $0x4,%esp
	movl    0x8(%ebp), %eax
	fstsw   0xfffffffe(%ebp)
	movw    0xfffffffe(%ebp), %dx
	movl    %edx,(%eax)
	movl    %ebp, %esp
	popl    %ebp
	ret

	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_setjmp$stub:
	.indirect_symbol _setjmp
	hlt ; hlt ; hlt ; hlt ; hlt
	.subsections_via_symbols
