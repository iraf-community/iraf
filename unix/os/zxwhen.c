/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <signal.h>

#define import_spp
#define	import_kernel
#define	import_knames
#define import_xwhen
#include <iraf.h>

/* Set the following nonzero to cause process termination with a core dump
 * when the first signal occurs.
 */
int	debug_sig = 0;

/* The following definition has intimate knowledge of the STDIO structures. */
#define	fcancel(fp)	((fp)->_cnt=(fp)->_bufsiz,(fp)->_ptr=(fp)->_base)


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

int handler_epa[] = {		/* table of handler EPAs		*/
	NULL,			/* X_ACV    				*/
	NULL,			/* X_ARITH  				*/
	NULL,			/* X_INT    				*/
	NULL,			/* X_IPC    				*/
};

struct osexc {
	int	x_vex;		/* UNIX signal code			*/
	char	*x_name;	/* UNIX signal name string		*/
};

struct osexc unix_exception[] = {
	NULL,		"",
	NULL,		"hangup",
	X_INT,		"interrupt",
	NULL,		"quit",
	X_ACV,		"illegal instruction",
	NULL,		"trace trap",
	X_ACV,		"IOT",
	X_ACV,		"EMT",
	X_ARITH,	"FPE",
	NULL,		"kill",
	X_ACV,		"bus error",
	X_ACV,		"segmentation violation",
	X_ACV,		"bad arg to system call",
	X_IPC,		"write to pipe with no reader",
	NULL,		"alarm clock",
	X_INT,		"software terminate (interrupt)",
	EOMAP,		""
};


/* Hardware exceptions [MACHDEP].  To customize for a new machine, replace
 * the symbol MYMACHINE by the machine name, #define the name in <iraf.h>
 * (i.e., hlib$libc/iraf.h), and edit the hardware exception list below.
 */
struct	_hwx {
	int	v_code;			/* Hardware exception code	*/
	char	*v_msg;			/* Descriptive error message	*/
};

struct	_hwx hwx_exception[] = {
#ifdef vax
	ILL_RESAD_FAULT,	"reserved addressing fault",
	ILL_PRIVIN_FAULT,	"privileged instruction fault",
	ILL_RESOP_FAULT,	"reserved operand fault",
#endif vax
#ifdef mc68000
	ILL_ILLINSTR_FAULT,	"illegal instruction fault",
	ILL_PRIVVIO_FAULT,	"privilege violation fault",
	ILL_COPROCERR_FAULT,	"[coprocessor protocol error fault]",
	ILL_TRAP1_FAULT,	"trap #1 fault",
	ILL_TRAP2_FAULT,	"trap #2 fault",
	ILL_TRAP3_FAULT,	"trap #3 fault",
	ILL_TRAP4_FAULT,	"trap #4 fault",
	ILL_TRAP5_FAULT,	"trap #5 fault",
	ILL_TRAP6_FAULT,	"trap #6 fault",
	ILL_TRAP7_FAULT,	"trap #7 fault",
	ILL_TRAP8_FAULT,	"trap #8 fault",
	ILL_TRAP9_FAULT,	"trap #9 fault",
	ILL_TRAP10_FAULT,	"trap #10 fault",
	ILL_TRAP11_FAULT,	"trap #11 fault",
	ILL_TRAP12_FAULT,	"trap #12 fault",
	ILL_TRAP13_FAULT,	"trap #13 fault",
	ILL_TRAP14_FAULT,	"trap #14 fault",
#endif mc68000
#ifdef sparc
	ILL_STACK,		"bad stack",
	ILL_ILLINSTR_FAULT,	"illegal instruction fault",
	ILL_PRIVINSTR_FAULT,	"privileged instruction fault",
#endif sparc
#ifdef mc68000
	EMT_EMU1010,		"line 1010 emulator trap",
	EMT_EMU1111,		"line 1111 emulator trap",
#endif mc68000
#ifdef sparc
	EMT_TAG,		"tag overflow",
#endif sparc
#ifdef vax
	FPE_INTOVF_TRAP,	"integer overflow",
	FPE_INTDIV_TRAP,	"integer divide by zero",
	FPE_FLTOVF_TRAP,	"floating overflow",
	FPE_FLTDIV_TRAP,	"floating/decimal divide by zero",
	FPE_FLTUND_TRAP,	"floating underflow",
	FPE_DECOVF_TRAP,	"decimal overflow",
	FPE_SUBRNG_TRAP,	"subscript out of range",
	FPE_FLTOVF_FAULT,	"floating overflow fault",
	FPE_FLTDIV_FAULT,	"divide by zero floating fault",
	FPE_FLTUND_FAULT,	"floating underflow fault",
#endif vax
#ifdef mc68000
	FPE_INTDIV_TRAP,	"integer divide by zero",
	FPE_CHKINST_TRAP,	"CHK [CHK2] instruction",
	FPE_TRAPV_TRAP,		"TRAPV [cpTRAPcc TRAPcc] instr",
	FPE_FLTBSUN_TRAP,	"[branch or set on unordered cond]",
	FPE_FLTINEX_TRAP,	"[floating inexact result]",
	FPE_FLTDIV_TRAP,	"[floating divide by zero]",
	FPE_FLTUND_TRAP,	"[floating underflow]",
	FPE_FLTOPERR_TRAP,	"[floating operand error]",
	FPE_FLTOVF_TRAP,	"[floating overflow]",
	FPE_FLTNAN_TRAP,	"[floating Not-A-Number]",
#ifdef sun
	FPE_FPA_ENABLE,		"[FPA not enabled]",
	FPE_FPA_ERROR,		"[FPA arithmetic exception]",
#endif sun
#endif mc68000
#ifdef sparc
	FPE_INTOVF_TRAP,	"integer overflow",
	FPE_INTDIV_TRAP,	"integer divide by zero",
	FPE_FLTINEX_TRAP,	"[floating inexact result]",
	FPE_FLTDIV_TRAP,	"[floating divide by zero]",
	FPE_FLTUND_TRAP,	"[floating underflow]",
	FPE_FLTOPERR_TRAP,	"[floating operand error]",
	FPE_FLTOVF_TRAP,	"[floating overflow]",
#endif sparc
#ifdef i386
	FPE_INTDIV_TRAP,	"integer divide by zero",
	FPE_INTOVF_TRAP,	"integer overflow",
	FPE_FLTOPERR_TRAP,	"[floating operand error]",
	FPE_FLTDEN_TRAP,	"[floating denormalized operand]",
	FPE_FLTDIV_TRAP,	"[floating divide by zero]",
	FPE_FLTOVF_TRAP,	"[floating overflow]",
	FPE_FLTUND_TRAP,	"[floating underflow]",
	FPE_FLTINEX_TRAP,	"[floating inexact result]",
	FPE_UUOP_TRAP,		"[floating undefined opcode]",
	FPE_DATACH_TRAP,	"[floating data chain exception]",
	FPE_FLTSTK_TRAP,	"[floating stack fault]",
	FPE_FPA_ENABLE,		"[FPA not enabled]",
	FPE_FPA_ERROR,		"[FPA arithmetic exception]",
#endif i386
#ifdef SUNOS4
	BUS_HWERR,		"misc hardware error (e.g. timeout)",
	BUS_ALIGN,		"hardware alignment error",
	SEGV_NOMAP,		"no mapping at the fault address",
	SEGV_PROT,		"access exceeded protections",
	SEGV_OBJERR,		"object returned errno value",
#endif
	EOMAP,			""
};


/* ZXWHEN -- Post an exception handler or turn off interrupts.  Return
 * value of old handler, so that it may be restored by the user code if
 * desired.  The function EPA's are the type of value returned by ZLOCPR.
 */
ZXWHEN (sig_code, epa, old_epa)
XINT	*sig_code;
XINT	*epa;			/* EPA of new exception handler	*/
XINT	*old_epa;		/* receives EPA of old handler	*/
{
	static	int first_call = 1;
	static	int ignore_sigint;
	int     vex, uex;
	PFI     vvector;
	extern  ex_handler();

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
	    kernel_panic ("zxwhen: bad exception code");
	}
	    
	*old_epa = handler_epa[vex];
	handler_epa[vex] = *epa;
	vvector = (PFI) ex_handler;

	/* Check for attempt to post same handler twice.  Do not return EPA
	 * of handler as old_epa as this could lead to recursion.
	 */
	if (*epa == X_IGNORE)
	    vvector = (PFI) SIG_IGN;
	else if (*epa == *old_epa)
	    *old_epa = X_IGNORE;

	/* Set all hardware vectors in the indicated exception class.
	 * If interrupt (SIGINT) was disabled when we were spawned (i.e.,
	 * when we were first called to set SIGINT) leave it that way, else
	 * we will get interrupted when the user interrupts the parent.
	 */
	for (uex=1;  unix_exception[uex].x_vex != EOMAP;  uex++) {
	    if (unix_exception[uex].x_vex == *sig_code)
		if (uex == SIGINT) {
		    if (first_call) {
			if (signal (uex, vvector) == SIG_IGN) {
			    signal (uex, SIG_IGN);
			    ignore_sigint++;
			}
			first_call = 0;
		    } else if (!ignore_sigint) {
			if (debug_sig)
			    signal (uex, SIG_DFL);
			else
			    signal (uex, vvector);
		    }
		} else {
		    if (debug_sig)
			signal (uex, SIG_DFL);
		    else
			signal (uex, vvector);
		}
	}
}


/* EX_HANDLER -- Called to handle an exception.  Map OS exception into
 * xwhen signal, call user exception handler.  A default exception handler
 * posted by the IRAF Main is called if the user has not posted another
 * handler.  If we get the software termination signal from the CL, 
 * stop process execution immediately (used to kill detached processes).
 */
ex_handler (unix_signal, hwcode, scp, addr)
int	unix_signal;			/* SIGINT, SIGFPE, etc.		*/
int	hwcode;				/* VAX hardware trap/fault codes */
struct	sigcontext *scp;
char	*addr;				/* added for SunOS 4.0 */
{
	int	vex;
	XINT	next_epa, epa, x_vex;

#ifdef mc68000
	/* The Sun floating point accelerator board routinely uses the
	 * following signal to force recomputation of floating point results
	 * by the mc68881 when the results would violate the IEEE std.
	 * If this code is missing the program will enter an infinite loop.
	 */
	if (hwcode == FPE_FPA_ERROR)
	    if (fpa_handler (unix_signal, hwcode, scp, addr))
		return;
#endif

	last_os_exception = unix_signal;
	last_os_hwcode = hwcode;
	x_vex = unix_exception[unix_signal].x_vex;
	vex = x_vex - X_FIRST_EXCEPTION;	
	epa = handler_epa[vex];

	/* If signal was SIGINT, cancel any buffered standard output.
	 */
	if (unix_signal == SIGINT)
	    fcancel (stdout);

	/* Unblock the signal, in the event that the handler never returns.
	 * sigsetmask (sigblock(0) & ~mask(unix_signal));
	 */
	sigsetmask (0);

	/* Call user exception handler(s).  Each handler returns with the
	 * "value" (epa) of the next handler, or X_IGNORE if exception handling
	 * is completed and processing is to continue normally.  If the handler
	 * wishes to restart the process, i.e., initiate error recovery, then
	 * the handler procedure will not return.
	 */
	for (next_epa=epa;  next_epa != X_IGNORE;  ((PFI)epa)(&x_vex,&next_epa))
	    epa = next_epa;
}


/* ZXGMES -- Get the machine dependent integer code and error message for the
 * most recent exception.  The integer code XOK is returned if no exception
 * has occurred, or if we are called more than once.
 */
ZXGMES (os_exception, errmsg, maxch)
XINT	*os_exception;
PKCHAR	*errmsg;
XINT	*maxch;
{
	register int	v;
	char	*os_errmsg;

	*os_exception = last_os_exception;

	if (last_os_exception != XOK) {
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
}
