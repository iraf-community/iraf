/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#ifdef LINUX
# include <fpu_control.h>
#endif

#ifdef MACOSX
#include <math.h>
#include <fenv.h>
#ifndef MACINTEL
#define MACUNIX
#endif
#endif

#define	import_spp
#define	import_kernel
#define	import_knames
#define	import_xnames
#define import_prtype
#include <iraf.h>

/*
 * ZZSTRT,ZZSTOP -- Routines to perform initialization and cleanup functions
 * during process startup and shutdown, when the IRAF kernel routines are being
 * called from a program which does not have a ZMAIN.
 */

/* #define DEBUG */

static	int prtype, ipc_isatty=NO;
static	int ipc_in = 0, ipc_out = 0;
static	char os_process_name[SZ_FNAME];
static	char osfn_bkgfile[SZ_PATHNAME];
extern	int errno;

#define	align(a)	((a)&(~pmask))

void 	ready_ (void);

extern int ZAWSET(), ZOPNTY(), ZZSETK();



/* ZZSTRT -- Initialize the IRAF kernel at process startup time.
 */
int
ZZSTRT (void)
{
	XINT	wsetsize=0L, junk;
#ifndef LINUX64
	extern  void sfpucw_();
#endif
	extern  int  spp_debug();


	spp_debug ();

	/* Initialize globals.
	 */
	sprintf (os_process_name, "%d", getpid());
	strcpy (osfn_bkgfile, "");
	prtype = PR_HOST;

	/* Initialize the kernel file descriptor. */
	zfd[0].fp = stdin;	zfd[0].flags = KF_NOSEEK;
	zfd[1].fp = stdout;	zfd[1].flags = KF_NOSEEK;
	zfd[2].fp = stderr;	zfd[2].flags = KF_NOSEEK;

	/* Dummy routine called to indicate that mapping is complete. */
	ready_();

#if defined(MACOSX)
        /*  Clears the exception-occurred bits in the FP status register.
         */
        feclearexcept (FE_ALL_EXCEPT);
#else

#if defined(LINUX)
	/* Enable the common IEEE exceptions.  Newer Linux systems disable
	 * these by default, the usual SYSV behavior.
	 */

	/* Old code; replaced by SFPUCW in as$zsvjmp.s 
	    asm ("fclex");
	    setfpucw (0x1372);
	 */
	{   
	    /* 0x332: round to nearest, 64 bit precision, mask P-U-D. */
#ifdef MACUNIX
	    int fpucw = _FPU_IEEE;
#else
	    int fpucw = 0x332;
#endif
#ifdef LINUX64
            /*
            XINT fpucw = 0x332;
            SFPUCW (&fpucw);
            */
            fpu_control_t cw = 
                (_FPU_EXTENDED | _FPU_MASK_PM | _FPU_MASK_UM | _FPU_MASK_DM);
            _FPU_SETCW(cw);
#else
	    sfpucw_ (&fpucw);
#endif
	}
#endif
#endif

#ifdef SYSV
	/* Initialize the time zone data structures. */
	tzset();
#endif

	/* Place a query call to ZAWSET to set the process working set limit
	 * to the IRAF default value, in case we did not inherit a working set
	 * limit value from the parent process.
	 */
	ZAWSET (&wsetsize, &junk, &junk, &junk);

	/* Initialize the stdio streams. */
	{   XINT ro = READ_ONLY, wo = WRITE_ONLY, chan;

	    ZOPNTY ((PKCHAR *)U_STDIN, &ro, &chan);
	    ZOPNTY ((PKCHAR *)U_STDOUT, &wo, &chan);
	    ZOPNTY ((PKCHAR *)U_STDERR, &wo, &chan);
	}

	/* Pass the values of the kernel parameters into the kernel. */
	ZZSETK (os_process_name, osfn_bkgfile, prtype, ipc_isatty,
	    &ipc_in, &ipc_out);

	return (XOK);
}


/* ZZSTOP -- Clean up prior to process shutdown.
 */
int ZZSTOP (void) { return (XOK); }


/* ready -- This is a dummy routine used when debugging to allow a breakpoint
 * to be set at a convenient point after the shared image has been mapped in.
 */
void ready_ (void) {}

