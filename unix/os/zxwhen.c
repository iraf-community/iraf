/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <signal.h>

#ifdef LINUX
# include <fpu_control.h>
# define USE_SIGACTION
#else
# ifdef BSD
# include <floatingpoint.h>
# endif
#endif

#ifdef SOLARIS
# define USE_SIGACTION
# include <sys/siginfo.h>
# include <sys/ucontext.h>
# include <ieeefp.h>
#endif

#define import_spp
#define	import_kernel
#define	import_knames
#define import_xwhen
#include <iraf.h>

/* ZXWHEN.C -- IRAF exception handling interface.  This version has been 
 * customized for PC-IRAF, i.e., LINUX and FreeBSD.
 */

/* Set the following nonzero to cause process termination with a core dump
 * when the first signal occurs.
 */
int debug_sig = 0;

#ifdef LINUX

#define FPSTAT          /* system-dependent FPU debugging */

/* The following definition has intimate knowledge of the STDIO structures.
 *    OBSOLETE - problem this fixed has gone away
#define	fcancel(fp)	( \
    (fp)->_IO_read_ptr = (fp)->_IO_read_end = (fp)->_IO_read_base,\
    (fp)->_IO_write_ptr = (fp)->_IO_write_end = (fp)->_IO_write_base)
*/
#define	fcancel(fp)

/* x86 Traps.
 */
#define HWX_BASE		10

#define	X86_DIVIDE_ERROR	10
#define	X86_DEBUG		11
#define	X86_NMI_INT		12
#define	X86_BREAKPOINT		13
#define	X86_INTO		14
#define	X86_BOUND		15
#define	X86_INVALID_OPCODE	16
#define	X86_DEVICE_NOTAVAIL	17
#define	X86_DOUBLE_FAULT	18
#define	X86_SEGMENT_OVERRUN	19
#define	X86_INVALID_TSS		20
#define	X86_SEGMENT_NOTPRESENT	21
#define	X86_STACK_FAULT		22
#define	X86_GPE			23
#define	X86_PAGE_FAULT		24
#define	X86_RESERVED_EX15	25
#define	X86_FPE			26
#define	X86_ALIGNMENT_CHECK	27
#define	X86_MACHINE_CHECK	28

/* x86 FPU hardware exceptions.
 */
#define	FPE_FPA_ERROR		30
#define	FPE_INTDIV_TRAP		31
#define	FPE_INTOVF_TRAP		32
#define	FPE_FLTOPERR_TRAP	33
#define	FPE_FLTDEN_TRAP		34
#define	FPE_FLTDIV_TRAP		35
#define	FPE_FLTOVF_TRAP		36
#define	FPE_FLTUND_TRAP		37
#define	FPE_FLTINEX_TRAP	38
#define	FPE_FLTSTK_TRAP		39

#else
# ifdef BSD
/* The following definition has intimate knowledge of the STDIO structures. */
# define	fcancel(fp)	((fp)->_r = (fp)->_w = 0)

#else
# ifdef SOLARIS
# define	fcancel(fp)     ((fp)->_cnt=BUFSIZ,(fp)->_ptr=(fp)->_base)

#endif
#endif
#endif

static long setsig();
static int ex_handler();

#ifdef FPSTAT
static int fpstatus();
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
	X_ACV,		"abort",
	X_ACV,		"EMT exception",
	X_ARITH,	"arithmetic exception",
	NULL,		"kill",
	X_ACV,		"bus error",
	X_ACV,		"segmentation violation",
	X_ACV,		"bad arg to system call",
	X_IPC,		"write to pipe with no reader",
	NULL,		"alarm clock",
	X_INT,		"software terminate (interrupt)",
	X_ARITH,	"STKFLT",
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

#ifdef LINUX
struct	_hwx hwx_exception[] = {
	/* x86 hardware (cpu) exceptions. */
	X86_DIVIDE_ERROR,	"integer divide by zero",
	X86_DEBUG,		"debug exception",
	X86_NMI_INT,		"NMI interrupt",
	X86_BREAKPOINT,		"breakpoint",
	X86_INTO,		"integer overflow",
	X86_BOUND,		"bound range exceeded",
	X86_INVALID_OPCODE,	"invalid opcode",
	X86_DEVICE_NOTAVAIL,	"device not available",
	X86_DOUBLE_FAULT,	"double fault",
	X86_SEGMENT_OVERRUN,	"coprocessor segment overrun",
	X86_INVALID_TSS,	"invalid task state segment",
	X86_SEGMENT_NOTPRESENT,	"segment not present",
	X86_STACK_FAULT,	"stack fault",
	X86_GPE,		"general protection error",
	X86_PAGE_FAULT,		"page fault",
	X86_RESERVED_EX15,	"reserved int15",
	X86_FPE,		"floating point exception",
	X86_ALIGNMENT_CHECK,	"alignment check",
	X86_MACHINE_CHECK,	"machine check",

	/* x86 floating point exceptions. */
	FPE_FPA_ERROR,		"FPA arithmetic exception",
	FPE_INTDIV_TRAP,	"integer divide by zero",
	FPE_INTOVF_TRAP,	"integer overflow",
	FPE_FLTOPERR_TRAP,	"floating operand error",
	FPE_FLTDEN_TRAP,	"floating denormalized operand",
	FPE_FLTDIV_TRAP,	"floating divide by zero",
	FPE_FLTOVF_TRAP,	"floating overflow",
	FPE_FLTUND_TRAP,	"floating underflow",
	FPE_FLTINEX_TRAP,	"floating inexact result",
	FPE_FLTSTK_TRAP,	"floating stack fault",
	EOMAP,			""
};

#else
#ifdef BSD
struct	_hwx hwx_exception[] = {
	FPE_INTOVF_TRAP,	"integer overflow",
	FPE_INTDIV_TRAP,	"integer divide by zero",
	FPE_FLTDIV_TRAP,	"floating divide by zero",
	FPE_FLTOVF_TRAP,	"floating overflow",
	FPE_FLTUND_TRAP,	"floating underflow",
	FPE_FPU_NP_TRAP,	"floating point unit not present",
	FPE_SUBRNG_TRAP,	"subrange out of bounds",
	EOMAP,			""
};

#else
#ifdef SOLARIS
struct	_hwx hwx_exception[] = {
	FPE_INTDIV,             "integer divide by zero",
	FPE_INTOVF,             "integer overflow",
	FPE_FLTDIV,             "floating point divide by zero",
	FPE_FLTOVF,             "floating point overflow",
	FPE_FLTUND,             "floating point underflow",
	FPE_FLTRES,             "floating point inexact result",
	FPE_FLTINV,             "invalid floating point operation",
	FPE_FLTSUB,             "subscript out of range",
	EOMAP,			""
};

#endif
#endif
#endif


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
	SIGFUNC	vvector;

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
	    vex = NULL;
	    kernel_panic ("zxwhen: bad exception code");
	}
	    
	*old_epa = handler_epa[vex];
	handler_epa[vex] = *epa;
	vvector = (SIGFUNC) ex_handler;

	/* Check for attempt to post same handler twice.  Do not return EPA
	 * of handler as old_epa as this could lead to recursion.
	 */
	if (*epa == X_IGNORE)
	    vvector = (SIGFUNC) SIG_IGN;
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


/* SETSIG -- Post an exception handler for the given exception.
 */
static long
setsig (code, handler)
int code;
SIGFUNC	handler;
{
	long status;

#ifdef USE_SIGACTION
	struct sigaction sig;
	sig.sa_handler = (SIGFUNC) handler;
	sigemptyset (&sig.sa_mask);
	sig.sa_flags = SA_NODEFER;
	status = (long) sigaction (code, &sig, NULL);
#else
	status = (long) signal (code, handler);
#endif

	return (status);
}


/* EX_HANDLER -- Called to handle an exception.  Map OS exception into
 * xwhen signal, call user exception handler.  A default exception handler
 * posted by the IRAF Main is called if the user has not posted another
 * handler.  If we get the software termination signal from the CL, 
 * stop process execution immediately (used to kill detached processes).
 */
static int
#ifdef SOLARIS
    ex_handler (unix_signal, info, ucp)
    int unix_signal;
    siginfo_t *info;
    ucontext_t *ucp;
#else
#ifdef USE_SIGACTION
    ex_handler (unix_signal, hwcode, scp)
    int unix_signal, hwcode;
    struct sigcontext *scp;
#else
    ex_handler (unix_signal)
    int unix_signal;
#endif
#endif
{
	XINT next_epa, epa, x_vex;
	int *frame, vex;

	last_os_exception = unix_signal;
#ifdef SOLARIS
        last_os_hwcode = info ? info->si_code : 0;
#else
	last_os_hwcode = 0;
#endif
	x_vex = unix_exception[unix_signal].x_vex;
	vex = x_vex - X_FIRST_EXCEPTION;	
	epa = handler_epa[vex];

#ifdef LINUX
#ifdef FPSTAT
	/* [LINUX] The following is horribly system dependent but is the only
	 * way to find out more about what type of system trap has occurred.
	 * The EBP register points to the beginning of the current stack frame.
	 * This is preceded by the saved frame giving the context before the
	 * exception handler was called.  This frame contains the system level
	 * trap number indicating which x86 cpu trap occurred.
	 */
	asm ("movl %%ebp,%0" : : "g" (frame));  frame++;
	if (frame[1] == unix_signal)
	    last_os_hwcode = frame[14] + HWX_BASE;

	/* If the floating point coprocessor is involved try to learn more
	 * about what type of floating point exception occurred.
	 */
	if (unix_signal == SIGFPE)
	    last_os_hwcode = fpstatus (last_os_hwcode);
#endif

	/* Reenable the exception (LINUX).  */
#ifndef USE_SIGACTION
	sigset (unix_signal, (SIGFUNC) ex_handler);
#endif
	/* setfpucw (0x1372); */
	{   int fpucw = 0x332;
	    sfpucw_ (&fpucw);
	}

#else
#ifdef BSD

#else
#ifdef SOLARIS
        fpsetsticky (0x0);
	fpsetmask (FP_X_INV | FP_X_OFL | FP_X_DZ);

#endif
#endif
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
	for (next_epa=epa;  next_epa != X_IGNORE;
		((SIGFUNC)epa)(&x_vex,&next_epa))
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
}


#ifdef LINUX
/*
 * x86 Floating status register.
 *
 *      15-8     7    6    5    4    3    2    1    0
 * | reserved | ES | SF | PE | UE | OE | ZE | DE | IE
 *
 * IE: Invalid operation
 * DE: Denormalized operand
 * ZE: Divide by zero
 * OE: Overflow
 * UE: Underflow
 * PE: Inexact result
 */

#define _FPU_COND_IE	0x01  
#define _FPU_COND_DE	0x02
#define _FPU_COND_ZE	0x04
#define _FPU_COND_OE	0x08
#define _FPU_COND_UE	0x10
#define _FPU_COND_PE	0x20
#define _FPU_COND_SF	0x40
#define _FPU_COND_ES	0x80
#define _FPU_COND_MASK	0xff

/* FPSTATUS -- Return the hardware code of the last floating point exception.
 */
static int
fpstatus (old_hwcode)
int old_hwcode;
{
	unsigned short status = 0;
	int hwcode = 0;

	/* Make sure we have a floating point exception. */
	if (old_hwcode != X86_FPE)
	    return (old_hwcode);

	/* Get floating point status register. */
	asm ("fstsw %0" : "=ax" (status));

	/* Return the old hardware code if the status register has been
	 * cleared and doesn't tell us anything.
	 */
	if (!(status & 0xff))
	    return (old_hwcode);

	switch (status & _FPU_COND_MASK) {
	case _FPU_COND_IE:
	    hwcode = FPE_FLTOPERR_TRAP;
	    break;
	case _FPU_COND_DE:
	    hwcode = FPE_FLTDEN_TRAP;
	    break;
	case _FPU_COND_ZE:
	    hwcode = FPE_FLTDIV_TRAP;
	    break;
	case _FPU_COND_OE:
	    hwcode = FPE_FLTOVF_TRAP;
	    break;
	case _FPU_COND_UE:
	    hwcode = FPE_FLTUND_TRAP;
	    break;
	case _FPU_COND_PE:
	    hwcode = FPE_FLTINEX_TRAP;
	    break;
	case _FPU_COND_SF:
	    hwcode = FPE_FLTSTK_TRAP;
	    break;
	}

	/* Clear floating point status register. */
	asm ("fclex");

	return (hwcode);
}
#endif
