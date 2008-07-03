/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <signal.h>

#ifdef CYGWIN
# include <math.h>
# include <mingw/fenv.h>
#endif
#ifdef MACOSX
# include <math.h>
# include <fenv.h>
#endif

#define import_spp
#define import_knames
#include <iraf.h>

#include "zos.h"

/*
 * ZZEPRO.C -- Code which is executed at the end of every procedure.
 */

/* NOTE: Following is also picked up by Mac/Intel. */
#if (defined(MACOSX) || defined(CYGWIN))

/* zfpuct.c */
extern int macosx_sigmask;

/* ZZEPRO.C -- On MacOSX (which under 10.1.x can't raise a hardware
 * exception) we check at the end of every procedure to see if a floating
 * exception occurred.
 */
int ZZEPRO( void )
{
	fexcept_t  flagp;

	fegetexceptflag (&flagp, macosx_sigmask);
	if (flagp & macosx_sigmask) {
#if (defined(MACOSX) && defined(OLD_MACOSX))
	    struct sigcontext scp;
	    scp.sc_psw = (flagp & macosx_sigmask);
	    ex_handler (SIGFPE, 0, &scp);
#else
	    siginfo_t info;
	    info.si_code = (flagp & macosx_sigmask);
	    ex_handler (SIGFPE, &info, NULL);
#endif	/* OLD_MACOSX */
	}

	/* Clear the exception. */
	flagp = (fexcept_t) NULL;
	feclearexcept (FE_ALL_EXCEPT);

	return XOK;
}

#else
int ZZEPRO( void )
{
	return XOK;
}
#endif	/* MACOSX || CYGWIN */
