#include <stdio.h>
#include <signal.h>
#ifdef MACOSX
#include <math.h>
#include <fenv.h>
#endif
#ifdef CYGWIN
#include <math.h>
#include <mingw/fenv.h>
#endif

#define import_spp
#define import_knames
#include <iraf.h>

/*
 * ZZEPRO.C -- Code which is executed at the end of every procedure.
 */

/* NOTE: Following is also picked up by Mac/Intel. */
#if defined(MACOSX) || defined(CYGWIN)

int macosx_sigmask = (FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);


/* ZZEPRO.C -- On MacOSX (which under 10.1.x can't raise a hardware
 * exception) we check at the end of every procedure to see if a floating
 * exception occurred.
 */
ZZEPRO()
{
	fexcept_t  flagp;

	fegetexceptflag (&flagp, macosx_sigmask);
	if (flagp & macosx_sigmask) {
	    siginfo_t info;
	    info.si_code = (flagp & macosx_sigmask);
	    ex_handler (SIGFPE, &info, NULL);
	}

	/* Clear the exception. */
    	flagp = (fexcept_t) NULL;
    	feclearexcept (FE_ALL_EXCEPT);
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
	fexcept_t  flagp;

	fegetexceptflag (&flagp, macosx_sigmask);
	macosx_sigmask |=  FE_INVALID;
	flagp &= ~FE_INVALID;
	
	fesetexceptflag (&flagp, macosx_sigmask);
}


#else
ZZEPRO() {}
#endif
