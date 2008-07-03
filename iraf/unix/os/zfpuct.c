/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/* 
   Architecture OS      GFPUCW/SFPUCW GFPUSW gfpscr/sfpscr/gxer MXMASK/MXUMSK
   I386/X86_64  LINUX         O         -            -                -
   I386         CYGWIN        O         -            -                O
   I386         MACOSX        O         O            -                O
   POWERPC      LINUX         O         -            -                -
   POWERPC      MACOSX        -         -            O                O
*/

#ifdef LINUX
# include <fpu_control.h>
#endif
#ifdef CYGWIN
# include <math.h>
# include <mingw/fenv.h>
#endif
#ifdef MACOSX
# include <math.h>
# include <fenv.h>
#endif

#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

#include "zos.h"


#ifdef LINUX

int GFPUCW ( XINT *xcw )
{
	fpu_control_t cw;
	_FPU_GETCW(cw);
	*xcw = cw;
	return cw;
}

int SFPUCW ( XINT *xcw )
{
	fpu_control_t cw = *xcw;
	_FPU_SETCW(cw);
	return cw;
}

#endif	/* LINUX */


#ifdef I386

#if (defined(CYGWIN) || defined(MACOSX))
asm(".text");
asm(".globl	_gfpucw_");
asm("_gfpucw_:				# Get fpucw:  gfpucw_ (&cur_fpucw)");
asm("	pushl	%ebp");
asm("	movl	%esp,%ebp");
asm("	subl    $0x4,%esp");
asm("	movl    0x8(%ebp), %eax");
asm("	fnstcw  0xfffffffe(%ebp)");
asm("	movw    0xfffffffe(%ebp), %dx");
asm("	movl	%edx,(%eax)");
asm("	movl	%ebp, %esp");
asm("	popl	%ebp");
asm("	ret");
asm("");
asm(".globl	_sfpucw_");
asm("_sfpucw_:				# Set fpucw:  sfpucw_ (&new_fpucw)");
asm("	pushl   %ebp");
asm("	movl    %esp,%ebp");
asm("	subl    $0x4,%esp");
asm("	movl    0x8(%ebp), %eax");
asm("	movl	(%eax), %eax");
asm("	andl    $0xf3f, %eax");
asm("	fclex");
asm("	movw    %ax, 0xfffffffe(%ebp)");
asm("	fldcw   0xfffffffe(%ebp)");
asm("	leave  ");
asm("	ret    ");
#endif	/* CYGWIN || MACOSX */

#if defined(MACOSX)
asm(".globl	_gfpusw_");
asm("_gfpusw_:				# Get fpusw:  gfpusw_ (&cur_fpusw)");
asm("	pushl   %ebp");
asm("	movl    %esp,%ebp");
asm("	subl    $0x4,%esp");
asm("	movl    0x8(%ebp), %eax");
asm("	fstsw   0xfffffffe(%ebp)");
asm("	movw    0xfffffffe(%ebp), %dx");
asm("	movl    %edx,(%eax)");
asm("	movl    %ebp, %esp");
asm("	popl    %ebp");
asm("	ret");
#endif	/* MACOSX */

#endif	/* I386 */


#ifdef POWERPC

#if defined(MACOSX)
asm(".text");
asm("	# GFPSCR -- Return the contents of the PowerPC FPSCR register.");
asm("	.align 2");
asm(".globl _gfpscr_");
asm("_gfpscr_:");
asm("	stmw	r30,-8(r1)");
asm("	stwu	r1,-48(r1)");
asm("	mr	r30,r1");
asm("	mflr	r0");
asm("	bcl	20,31,L2$pb");
asm("L2$pb:");
asm("	mflr	r31");
asm("	mtlr	r0");
asm("");
asm("	mffs	f0");
asm("	stfd	f0, 16(r30)");
asm("	lwz	r0, 20(r30)");
asm("	mr	r3, r0");
asm("");
asm("	b	L3");
asm("L3:");
asm("	lwz	r1,0(r1)");
asm("	lmw	r30,-8(r1)");
asm("	blr");
asm("");
asm("	# SFPSCR -- Set the contents of the PowerPC FPSCR register.");
asm("	.align 2");
asm(".globl _sfpscr_");
asm("_sfpscr_:");
asm("	stmw	r30,-8(r1)");
asm("	stwu	r1,-48(r1)");
asm("	mr	r30,r1");
asm("	mflr	r0");
asm("	bcl	20,31,L4$pb");
asm("L4$pb:");
asm("	mflr	r31");
asm("	mtlr	r0");
asm("");
asm("	lis	r0, 0xfff8");
asm("	stw	r0, 16(r30)");
asm("	lwz	r0, 0(r3)");
asm("	stw	r0, 20(r30)");
asm("	lfd	f0, 16(r30)");
asm("	mtfsf	255, f0");
asm("");
asm("	b	L5");
asm("L5:");
asm("	lwz	r1,0(r1)");
asm("	lmw	r30,-8(r1)");
asm("	blr");
asm("");
asm("	# GXER -- Return the contents of the PowerPC XER register.");
asm("	.align 2");
asm(".globl _gxer_");
asm("_gxer_:");
asm("	stmw	r30,-8(r1)");
asm("	stwu	r1,-48(r1)");
asm("	mr	r30,r1");
asm("	mflr	r0");
asm("	bcl	20,31,L3$pb");
asm("L3$pb:");
asm("	mflr	r31");
asm("	mtlr	r0");
asm("");
asm("	mfspr	r3,1");
asm("");
asm("	b	L4");
asm("L4:");
asm("	lwz	r1,0(r1)");
asm("	lmw	r30,-8(r1)");
asm("	blr");
#endif	/* MACOSX */

#endif	/* POWERPC */


#if (defined(MACOSX) || defined(CYGWIN))

int macosx_sigmask = (FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);

/* Mask or unmask the invalid operand exception.  Invalid must be
 * masked to be able to operate upon invalid operands, e.g., to filter
 * out NaN/Inf in IEEE i/o code (see as$ieee.c).
 */
int MXMASK ( void )
{
	macosx_sigmask &= ~FE_INVALID;
	return 0;
}

int MXUMSK ( void )
{	
	fexcept_t flagp;

	fegetexceptflag (&flagp, macosx_sigmask);
	macosx_sigmask |=  FE_INVALID;
	flagp &= ~FE_INVALID;
	
	fesetexceptflag (&flagp, macosx_sigmask);
	return 0;
}

#endif	/* MACOSX || CYGWIN */
