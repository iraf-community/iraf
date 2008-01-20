/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#ifdef LINUX
# include <fpu_control.h>
#endif
#ifdef BSD
# include <floatingpoint.h>
#endif
#ifdef CYGWIN
# include <mingw/fenv.h>
#endif
#ifdef MACOSX
# include <math.h>
# include <fenv.h>
#endif

#ifdef SOLARIS
# include <sys/siginfo.h>
# include <sys/ucontext.h>
# include <ieeefp.h>
#endif

#define import_spp
#define import_kernel
#define import_knames
#define import_xwhen
#include <iraf.h>

#include "zos.h"


#ifdef MACOSX

/* The following are needed for OS X 10.1 for backward compatability.  The
 * signal sa_flags are set to use them to get signal handling working on
 * 10.2 and later systems.
 */
#ifdef OLD_MACOSX
# ifndef SA_NODEFER
#  define SA_NODEFER	0x0010	/* don't mask the signal we're delivering */
# endif
# ifndef SA_NOCLDWAIT
#  define SA_NOCLDWAIT	0x0020	/* don't keep zombies around */
# endif
# ifndef SA_SIGINFO
#  define SA_SIGINFO	0x0040	/* signal handler with SA_SIGINFO args */
# endif
#endif	/* OLD_MACOSX */

#else

#ifdef OLD_MACOSX
# undef OLD_MACOSX
#endif

#endif	/* MACOSX */


/* ZXWHEN.C -- IRAF exception handling interface.  This version has been 
 * customized for PC-IRAF, i.e., LINUX and FreeBSD.
 *
 * Rewritten Aug200 to use sigaction by default on all systems (done in 
 * connection with the LinuxPPC port).  This got rid of a lot of old kludgy
 * platform-dependent code used to workaround Linux signal handling problems.
 */

/* Set the following nonzero to cause process termination with a core dump
 * when the first signal occurs.
 */
int debug_sig = 0;

#ifdef LINUX
# define	fcancel(fp)
#endif
#ifdef BSD
# define	fcancel(fp)	((fp)->_r = (fp)->_w = 0)
#endif
#ifdef MACOSX
# define	fcancel(fp)	((fp)->_r = (fp)->_w = 0)
#endif
#ifdef SOLARIS
# define	fcancel(fp)     ((fp)->_cnt=BUFSIZ,(fp)->_ptr=(fp)->_base)
#endif

static int ignore_sigint = 0;

/* */

#ifdef OLD_MACOSX
typedef sa_handler_t handler_type;
#else
typedef union {
    sa_handler_t sa__handler;
    sa_sigaction_t sa__sigaction;
} handler_type;
#endif

static int setsig ( int, handler_type );


#ifdef OLD_MACOSX
void ex_handler ( int, int, struct sigcontext * );
#else
void ex_handler ( int, siginfo_t *, void * );
#endif


/* Exception handling:  ZXWHEN (exception, handler, old_handler)
 *
 *	exception:	X_INT, X_ARITH, X_ACV, or X_IPC
 *
 *	handler:	Either X_IGNORE or the entry point address
 *			of a user supplied exception handler which
 *			will gain control in the event of an exception.
 *
 *	old_handler:	On output, contains the value of the previous
 *			handler (either X_IGNORE or an EPA).  Used to
 *			restore an old handler, or to chain handlers.
 *
 * An exception can be entirely disabled by calling ZXWHEN with the
 *   handler X_IGNORE.  Otherwise, the user supplied exception handler
 *   gains control when the exception occurs.  An exception handler is
 *   called with one argument, an integer code identifying the exception.
 *   The handler should return as its function value either X_IGNORE,
 *   causing normal processing to resume, or the EPA of the next handler
 *   to be called (normally the value of the parameter "old_handler").
 *   The user handler should call FATAL if error restart is desired.
 * 
 * If the SIGINT exeception has already been set to SIG_IGN, i.e., by the
 *   parent process which spawned us, then it will continue to be ignored.
 *   It is standard procedure in UNIX to spawn a background task with SIGINT
 *   disabled, so that interrupts sent to the parent process are ignored by
 *   the child.  If this is the case then SIGTERM may still be sent to the
 *   child to raise the X_INT exception in the high level code.
 */

#define EOMAP		(-1)	/* end of map array sentinel		*/
#define mask(s)		(1 << ((s) - 1))

static int last_os_exception;	/* save OS code of last exception	*/
static int last_os_hwcode;	/* hardware exception code		*/

static XSIGFUNC handler_epa[] = {	/* table of handler EPAs	*/
	0,			/* X_ACV				*/
	0,			/* X_ARITH				*/
	0,			/* X_INT				*/
	0,			/* X_IPC				*/
};

struct osexc {
	int x_vex;		/* UNIX signal code			*/
	const char *x_name;	/* UNIX signal name string		*/
};

static struct osexc unix_exception[] = {
	{ 0,		"" },
	{ 0,		"hangup" },
	{ X_INT,	"interrupt" },
	{ 0,		"quit" },
	{ X_ACV,	"illegal instruction" },
	{ 0,		"trace trap" },
	{ X_ACV,	"abort" },
	{ X_ACV,	"EMT exception" },
	{ X_ARITH,	"arithmetic exception" },
	{ 0,		"kill" },
	{ X_ACV,	"bus error" },
	{ X_ACV,	"segmentation violation" },
	{ X_ACV,	"bad arg to system call" },
	{ X_IPC,	"write to pipe with no reader" },
	{ 0,		"alarm clock" },
	{ X_INT,	"software terminate (interrupt)" },
	{ X_ARITH,	"STKFLT" },
	{ EOMAP,	"" }
};


/* Hardware exceptions [MACHDEP].  To customize for a new machine, replace
 * the symbol MYMACHINE by the machine name, #define the name in <iraf.h>
 * (i.e., hinclude$iraf.h), and edit the hardware exception list below.
 */
struct	_hwx {
	int v_code;			/* Hardware exception code	*/
	const char *v_msg;		/* Descriptive error message	*/
};

#ifdef MACOSX
#define FPE_INTDIV	(-2)		/* N/A */
#define FPE_INTOVF	(-2)		/* N/A */
#define FPE_FLTRES	FE_INEXACT	/* inexact */
#define FPE_FLTDIV	FE_DIVBYZERO	/* divide-by-zero */
#define FPE_FLTUND	FE_UNDERFLOW	/* underflow */
#define FPE_FLTOVF	FE_OVERFLOW	/* overflow */
#define FPE_FLTINV	FE_INVALID	/* invalid */
#define FPE_FLTSUB	(-2)		/* N/A */
#endif

static struct _hwx hwx_exception[] = {
	{ FPE_INTDIV,             "integer divide by zero" },
	{ FPE_INTOVF,             "integer overflow" },
	{ FPE_FLTDIV,             "floating point divide by zero" },
	{ FPE_FLTOVF,             "floating point overflow" },
	{ FPE_FLTUND,             "floating point underflow" },
	{ FPE_FLTRES,             "floating point inexact result" },
	{ FPE_FLTINV,             "floating point invalid operation" },
	{ FPE_FLTSUB,             "subscript out of range" },
	{ EOMAP,			"" }
};


/* ZXWHEN -- Post an exception handler or turn off interrupts.  Return
 * value of old handler, so that it may be restored by the user code if
 * desired.  The function EPA's are the type of value returned by ZLOCPR.
 */
/* ??? Related files are sys/etc/xwhen.x and sys/libc/cxwhen.c. ??? */
/* epa     : EPA of new exception handler	*/
/* old_epa : receives EPA of old handler	*/
int ZXWHEN ( XINT *sig_code, XPOINTER *epa, XPOINTER *old_epa )
{
	static int first_call = 1;
	int vex, uex;
	handler_type vvector;

	/* Convert code for virtual exception into an index into the table
	 * of exception handler EPA's.
	 */
	switch (*sig_code) {
	case X_ACV:
	case X_ARITH:
	case X_INT:
	case X_IPC:
	    vex = *sig_code - X_FIRST_EXCEPTION;
	    break;
	default:
	    vex = 0;
	    kernel_panic ("zxwhen: bad exception code");
	}
	    
	*old_epa = (XPOINTER)(handler_epa[vex]);
	handler_epa[vex] = (XSIGFUNC)(*epa);
#ifdef OLD_MACOSX
	vvector = &ex_handler;
#else
	vvector.sa__sigaction = &ex_handler;
#endif
	/* Check for attempt to post same handler twice.  Do not return EPA
	 * of handler as old_epa as this could lead to recursion.
	 */
	if ( (XSIGFUNC)(*epa) == X_IGNORE ) {
#ifdef OLD_MACOSX
	    vvector = SIG_IGN;
#else
	    vvector.sa__handler = SIG_IGN;
#endif
	} else if (*epa == *old_epa)
	    *old_epa = (XPOINTER)X_IGNORE;

	/* Set all hardware vectors in the indicated exception class.
	 * If interrupt (SIGINT) was disabled when we were spawned (i.e.,
	 * when we were first called to set SIGINT) leave it that way, else
	 * we will get interrupted when the user interrupts the parent.
	 */
	for (uex=1;  unix_exception[uex].x_vex != EOMAP;  uex++) {
	    if (unix_exception[uex].x_vex == *sig_code) {
		if (uex == SIGINT) {
		    if (first_call) {
			if (setsig (uex, vvector) != 0) {
			    setsig (uex, (handler_type)SIG_IGN);
			    ignore_sigint++;
			}
			first_call = 0;
		    } else if (!ignore_sigint) {
			if (debug_sig) {
			    setsig (uex, (handler_type)SIG_DFL);
			}
			else {
			    setsig (uex, vvector);
			}
		    }
		} else {
		    if (debug_sig) {
			setsig (uex, (handler_type)SIG_DFL);
		    }
		    else {
			setsig (uex, vvector);
		    }
		}
	    }
	}

	return XOK;
}


/* SETSIG -- Post an exception handler for the given exception.
 */
static int setsig ( int code, handler_type handler )
{
	struct sigaction sig;
	int status;

	sigemptyset (&sig.sa_mask);
#ifdef OLD_MACOSX
	sig.sa_handler = handler;
	sig.sa_flags = SA_NODEFER;
#else
	if ( handler.sa__handler == SIG_IGN ||
	     handler.sa__handler == SIG_DFL ) {
	    sig.sa_handler = handler.sa__handler;
	    sig.sa_flags = SA_NODEFER;
	}
	else {
	    sig.sa_sigaction = handler.sa__sigaction;
	    sig.sa_flags = (SA_NODEFER|SA_SIGINFO);
	}
#endif
	status = sigaction (code, &sig, NULL);

	return (status);
}


/* EX_HANDLER -- Called to handle an exception.  Map OS exception into
 * xwhen signal, call user exception handler.  A default exception handler
 * posted by the IRAF Main is called if the user has not posted another
 * handler.  If we get the software termination signal from the CL, 
 * stop process execution immediately (used to kill detached processes).
 */
#ifdef OLD_MACOSX
void ex_handler ( int unix_signal, int code, struct sigcontext *scp )
#else
void ex_handler ( int unix_signal, siginfo_t *info, void *uap )
#endif
{
	XSIGFUNC next_epa, epa;
	XINT x_vex, vex;

	last_os_exception = unix_signal;
#ifdef OLD_MACOSX
	last_os_hwcode = scp->sc_psw;
#else
	last_os_hwcode = info ? info->si_code : 0;
#endif
	x_vex = unix_exception[unix_signal].x_vex;
	vex = x_vex - X_FIRST_EXCEPTION;	
	epa = handler_epa[vex];

	/* Reenable/initialize the exception handler.
	 */

#if defined(MACOSX) || defined(CYGWIN)
        /* Clear the exception bits (ppc and x86). */
        feclearexcept (FE_ALL_EXCEPT);
#endif

#ifdef LINUX
	/* setfpucw (0x1372); */
	{
#ifdef POWERPC
	/* This is for Linux on a Mac, e.g., LinuxPPC (not MacOSX). */
	    XINT fpucw = _FPU_IEEE;

	    /*
	    if (unix_signal == SIGFPE)
		kernel_panic ("unrecoverable floating exception");
	    else
		SFPUCW (&fpucw);
	    if (unix_signal == SIGPIPE && !ignore_sigint)
		sigset (SIGINT, (SIGFUNC) ex_handler);
	     */

	    SFPUCW (&fpucw);
#else
	    /*
	    XINT fpucw = 0x336;
	    SFPUCW (&fpucw);
	    */
	    fpu_control_t cw = 
		(_FPU_EXTENDED | _FPU_MASK_PM | _FPU_MASK_UM | _FPU_MASK_ZM | _FPU_MASK_DM);
	    _FPU_SETCW(cw);
#endif	/* POWERPC */
	}
#endif	/* LINUX */


#ifdef SOLARIS
        fpsetsticky (0x0);
	fpsetmask (FP_X_INV | FP_X_OFL | FP_X_DZ);
#endif

	/* If signal was SIGINT, cancel any buffered standard output.  */
	if (unix_signal == SIGINT) {
	    fcancel (stdout);
	}

	/* Call user exception handler(s).  Each handler returns with the
	 * "value" (epa) of the next handler, or X_IGNORE if exception handling
	 * is completed and processing is to continue normally.  If the handler
	 * wishes to restart the process, i.e., initiate error recovery, then
	 * the handler procedure will not return.
	 */
	for ( next_epa=epa ; 
	      next_epa != X_IGNORE ;
	      (*epa)(&x_vex,&next_epa) ) {
	    epa = next_epa;
	}
}


/* ZXGMES -- Get the machine dependent integer code and error message for the
 * most recent exception.  The integer code XOK is returned if no exception
 * has occurred, or if we are called more than once.
 */
int ZXGMES ( XINT *os_exception, PKCHAR *errmsg, XINT *maxch )
{
	int v;
	const char *os_errmsg;
	size_t bufsize = *maxch + 1;

	*os_exception = last_os_exception;

	if (last_os_exception == XOK)
	    os_errmsg = "";
	else {
	    os_errmsg = unix_exception[last_os_exception].x_name;
	    if (last_os_exception == SIGFPE) {
		for (v=0;  hwx_exception[v].v_code != EOMAP;  v++)
		    if (hwx_exception[v].v_code == last_os_hwcode) {
			os_errmsg = hwx_exception[v].v_msg;
			break;
		    }
	    }
	}

	safe_strcpy ((char *)errmsg, bufsize, os_errmsg);

	last_os_exception = XOK;

	return XOK;
}
