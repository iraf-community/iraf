#include <stdio.h>
#include <signal.h>

#define import_spp
#define import_knames
#include <iraf.h>

/*
 * ZZEPRO.C -- Code which is executed at the end of every procedure.
 */

#ifdef MACOSX

#define FE_DIVBYZERO      0x04000000     /* divide-by-zero */
#define FE_OVERFLOW       0x10000000     /* overflow */
#define FE_INVALID        0x20000000     /* invalid */

int macosx_sigmask = (FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);


/* ZZEPRO.C -- On MacOSX (which under 10.1.x can't raise a hardware
 * exception) we check at the end of every procedure to see if a floating
 * exception occurred.
 */
ZZEPRO()
{
	register unsigned int v = gfpscr_();
	struct sigcontext scp;

	if (v & macosx_sigmask) {
	    scp.sc_psw = (v & macosx_sigmask);
	    ex_handler (SIGFPE, NULL, &scp);
	}
}

/* Mask or unmask the invalid operand exception.  Invalid must be
 * masked to be able to operate upon invalid operands, e.g., to filter
 * out NaN/Inf in IEEE i/o code (see as$ieee.gx).
 */
mxmask_()
{
	macosx_sigmask &= ~FE_INVALID;
}

mxumsk_()
{	
	register unsigned int v = gfpscr_();

	macosx_sigmask |=  FE_INVALID;
	v &= ~FE_INVALID;
	sfpscr_ (&v);
}


#else
ZZEPRO() {}
#endif
