/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <signal.h>

#include <math.h>
#include <fenv.h>

#define import_spp
#define	import_kernel
#define	import_knames
#define import_xwhen
#include <iraf.h>

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


/* If the OS allows, cancel any buffered output. If the OS doesn't, 
 * do nothing.
 */
#ifdef __APPLE__
#define	fcancel(fp)	((fp)->_r = (fp)->_w = 0)
#else
#define	fcancel(fp)
#endif

void ex_handler ( int, siginfo_t *, void * );

static long setsig();
static int ignore_sigint = 0;


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

#define	EOMAP		(-1)	/* end of map array sentinel		*/
#define	mask(s)		(1 << ((s) - 1))

int	last_os_exception;	/* save OS code of last exception	*/
int	last_os_hwcode;		/* hardware exception code		*/

XINT handler_epa[] = {		/* table of handler EPAs		*/
	0,			/* X_ACV    				*/
	0,			/* X_ARITH  				*/
	0,			/* X_INT    				*/
	0,			/* X_IPC    				*/
};

struct osexc {
	int	x_vex;		/* UNIX signal code			*/
	char	*x_name;	/* UNIX signal name string		*/
};

struct osexc unix_exception[] = {
	{ 0,		""					},
	{ 0,		"hangup"				},
	{ X_INT,	"interrupt"				},
	{ 0,		"quit"					},
	{ X_ACV,	"illegal instruction"			},
	{ 0,		"trace trap"				},
	{ X_ACV,	"abort"					},
	{ X_ACV,	"EMT exception"				},
	{ X_ARITH,	"arithmetic exception"			},
	{ 0,		"kill"					},
	{ X_ACV,	"bus error"				},
	{ X_ACV,	"segmentation violation"		},
	{ X_ACV,	"bad arg to system call"		},
	{ X_IPC,	"write to pipe with no reader"		},
	{ 0,		"alarm clock"				},
	{ X_INT,	"software terminate (interrupt)" 	},
	{ X_ARITH,	"STKFLT"				},
	{ EOMAP,	""					}
};


/* Hardware exceptions [MACHDEP].  To customize for a new machine, replace
 * the symbol MYMACHINE by the machine name, #define the name in <iraf.h>
 * (i.e., hlib$libc/iraf.h), and edit the hardware exception list below.
 */
struct	_hwx {
	int	v_code;			/* Hardware exception code	*/
	char	*v_msg;			/* Descriptive error message	*/
};

#ifdef __APPLE__
#ifdef  FPE_INTDIV
#undef  FPE_INTDIV
#endif
#define	FPE_INTDIV	(-2)		/* N/A */
#ifdef  FPE_INTOVF
#undef  FPE_INTOVF
#endif
#define	FPE_INTOVF	(-2)		/* N/A */
#ifdef  FPE_FLTRES
#undef  FPE_FLTRES
#endif
#define FPE_FLTRES	FE_INEXACT	/* inexact */
#ifdef  FPE_FLTDIV
#undef  FPE_FLTDIV
#endif
#define FPE_FLTDIV	FE_DIVBYZERO	/* divide-by-zero */
#ifdef  FPE_FLTUND
#undef  FPE_FLTUND
#endif
#define FPE_FLTUND	FE_UNDERFLOW	/* underflow */
#ifdef  FPE_FLTOVF
#undef  FPE_FLTOVF
#endif
#define FPE_FLTOVF	FE_OVERFLOW	/* overflow */
#ifdef  FPE_FLTINV
#undef  FPE_FLTINV
#endif
#define FPE_FLTINV	FE_INVALID	/* invalid */
#ifdef  FPE_FLTSUB
#undef  FPE_FLTSUB
#endif
#define	FPE_FLTSUB	(-2)		/* N/A */
#endif

struct	_hwx hwx_exception[] = {
	{ FPE_INTDIV,             "integer divide by zero"		},
	{ FPE_INTOVF,             "integer overflow"			},
	{ FPE_FLTDIV,             "floating point divide by zero"	},
	{ FPE_FLTOVF,             "floating point overflow"		},
	{ FPE_FLTUND,             "floating point underflow"		},
	{ FPE_FLTRES,             "floating point inexact result"	},
	{ FPE_FLTINV,             "floating point invalid operation"	},
	{ FPE_FLTSUB,             "subscript out of range"		},
	{ EOMAP,			""				}
};


/* ZXWHEN -- Post an exception handler or turn off interrupts.  Return
 * value of old handler, so that it may be restored by the user code if
 * desired.  The function EPA's are the type of value returned by ZLOCPR.
 */
int
ZXWHEN (
  XINT	*sig_code,
  XINT	*epa,			/* EPA of new exception handler	*/
  XINT	*old_epa 		/* receives EPA of old handler	*/
)
{
	static	int first_call = 1;
	int     vex, uex;
	SIGFUNC	vvector;

	extern  int  kernel_panic ();


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
	    vex = (int) 0;
	    kernel_panic ("zxwhen: bad exception code");
	}
	    
	*old_epa = handler_epa[vex];
	handler_epa[vex] = *epa;
	vvector = (SIGFUNC) ex_handler;

	/* Check for attempt to post same handler twice.  Do not return EPA
	 * of handler as old_epa as this could lead to recursion.
	 */
	if (*epa == (XINT) X_IGNORE)
	    vvector = (SIGFUNC) SIG_IGN;
	else if (*epa == *old_epa)
	    *old_epa = (XINT) X_IGNORE;

	/* Set all hardware vectors in the indicated exception class.
	 * If interrupt (SIGINT) was disabled when we were spawned (i.e.,
	 * when we were first called to set SIGINT) leave it that way, else
	 * we will get interrupted when the user interrupts the parent.
	 */
	for (uex=1;  unix_exception[uex].x_vex != EOMAP;  uex++) {
	    if (unix_exception[uex].x_vex == *sig_code) {
		if (uex == SIGINT) {
		    if (first_call) {
			if (setsig (uex, vvector) == (long) SIG_IGN) {
			    setsig (uex, SIG_IGN);
			    ignore_sigint++;
			}
			first_call = 0;
		    } else if (!ignore_sigint) {
			if (debug_sig)
			    setsig (uex, SIG_DFL);
			else
			    setsig (uex, vvector);
		    }
		} else {
		    if (debug_sig)
			setsig (uex, SIG_DFL);
		    else
			setsig (uex, vvector);
		}
	    }
	}

	return (XOK);
}


/* SETSIG -- Post an exception handler for the given exception.
 */
static long
setsig (code, handler)
int code;
SIGFUNC	handler;
{
	struct sigaction sig;
	long status;

	sigemptyset (&sig.sa_mask);
#ifdef __APPLE__
	sig.sa_handler = (SIGFUNC) handler;
#else
	sig.sa_sigaction = (SIGFUNC) handler;
#endif
	sig.sa_flags = (SA_NODEFER|SA_SIGINFO);
	status = (long) sigaction (code, &sig, NULL);

	return (status);
}


/* EX_HANDLER -- Called to handle an exception.  Map OS exception into
 * xwhen signal, call user exception handler.  A default exception handler
 * posted by the IRAF Main is called if the user has not posted another
 * handler.  If we get the software termination signal from the CL, 
 * stop process execution immediately (used to kill detached processes).
 */
void
ex_handler (
  int  	    unix_signal,
  siginfo_t *info,
  void      *ucp
)
{
	XINT  next_epa, epa, x_vex;
	int   vex;

	last_os_exception = unix_signal;
        last_os_hwcode = info ? info->si_code : 0;

	x_vex = unix_exception[unix_signal].x_vex;
	vex = x_vex - X_FIRST_EXCEPTION;	
	epa = handler_epa[vex];

	/* Reenable/initialize the exception handler.
	 */

        /*  Clears the exception-occurred bits in the FP status register.
         */
        feclearexcept (FE_ALL_EXCEPT);

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
	for (next_epa=epa;  next_epa != (XINT) X_IGNORE;
	    ((SIGFUNC)epa)(&x_vex,&next_epa))
	        epa = next_epa;
}


/* ZXGMES -- Get the machine dependent integer code and error message for the
 * most recent exception.  The integer code XOK is returned if no exception
 * has occurred, or if we are called more than once.
 */
int
ZXGMES (
  XINT	  *os_exception,
  PKCHAR  *errmsg,
  XINT	  *maxch
)
{
	register int	v;
	char	*os_errmsg;

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

	strncpy ((char *)errmsg, os_errmsg, (int)*maxch);
	((char *)errmsg)[*maxch] = EOS;

	last_os_exception = XOK;

	return (XOK);
}
