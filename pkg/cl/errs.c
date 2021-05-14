/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_libc
#define	import_fset
#define	import_stdio
#define import_setjmp
#define import_knames
#define	import_xnames
#define	import_stdarg
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "operand.h"
#include "param.h"
#include "task.h"
#include "mem.h"
#include "errs.h"
#include "grammar.h"
#include "construct.h"
#include "proto.h"


/*
 * ERRS -- When a runtime operation detects an error, it calls error with an
 *   error type, a diagnostic string and some additional arguments. the type
 *   determines the severity and prefix for the diagnostic. the diagnositic
 *   and its args are written as an error message with doprnt.
 * After the error message has been printed to our t_stderr, tasks are killed
 *   until an interactive cl is found. the longjmp forces the last
 *   setjmp (errenv) in main() to return and start the parser again.
 *   thus, a call to error() never returns but forces a reset back to an
 *   interactive state.
 *
 * Some frequently used diagnostic strings are defined here to avoid
 *   repetition. The list may be expanded or ignored as desired when new
 *   errors are added.
 */
extern	int errlev;		/* for detecting error recursion	*/
extern	int bkgno;		/* bkg task number, if batch job	*/
extern	int validerrenv;	/* set in main once get past login()	*/
extern	int loggingout;		/* set while reading from logout file	*/
extern	int gologout;		/* set when getting ready to " " "	*/
extern	jmp_buf errenv;		/* setjmp() is in main().		*/

char	*e_appopen 	= "can not open `%s' for appending";
char	*e_badstrop 	= "illegal operation on string '%0.20s'";
char	*e_badsw 	= "bad switch case, %d, in `%s'";
/* char	*e_edom =	"function argument outside valid range: %g"; */
/* char	*e_erange =	"%g caused arithmetic overflow"; */
/* char	*e_fpe =	"floating point exception"; */
char	*e_geonearg 	= "`%s' requires at least one argument";
char	*e_indexunf 	= "no indices on stack for array reference";
char 	*e_nominmax 	= "structs, strings, cursors and booleans have no ranges";
char	*e_nopfile 	= "task `%s' has no param file";
char	*e_badpfile 	= "cannot read parameter file `%s'";
char	*e_nostrcnv 	= "may not convert string to other types";
/* char	*e_notbool =	"parameter `%s' is not boolean"; */
char	*e_onearg 	= "`%s' expects one argument";
char	*e_pambig 	= "ambiguous parameter `%s' within `%s'";
char	*e_pckambig 	= "ambiguous package `%s'";
char	*e_pcknonexist	= "package `%s' not found";
char	*e_posargs 	= "too many positional arguments for `%s'";
char	*e_pnonexist 	= "parameter `%s' not found";
char	*e_ropen 	= "cannot open `%s' for reading";
char	*e_simplep 	= "use simple parameter name only for `%s'";
char	*e_strplusreal	= "attempt to add operand of type real to string `%s'";
char	*e_soverflow 	= "stack overflow (cs:%d,os:%d)";
char	*e_sunderflow 	= "stack underflow";
char	*e_tambig 	= "ambiguous task `%s'";
char	*e_twoargs 	= "`%s' expects two arguments";
char	*e_tnonexist 	= "task `%s' not found";
/* char	*e_unlink =	"cannot remove file `%s'"; */
char	*e_uopcode 	= "undefined opcode %d";
char	*e_wopen 	= "cannot open `%s' for writing";
char	*e_lookparm 	= "error searching for parameter `%s'.";
char	*e_invaldef	= "conflicting attributes in definition of `%s'.";
char	*e_fdivzero 	= "floating divide by zero";
char	*e_idivzero 	= "integer divide by zero";

/* The 'errlog' variable is used to avoid duplicate error logging by the
 * builtin clerror() and the error function cl_error() below.  When a script
 * or executable tasks calls the CL language 'error' function, the builtin
 * clerror() logs the error message.  Otherwise, we'll log it here.
 */
int	errlog = 0;

extern  int u_doprnt();


/* CL_ERROR -- print error info according to errtype on our t_stderr, pop back
 * to an interactive task and do a longjmp back to setjmp (errenv) in
 * main(); thus, whomever calls error() should not expect it to return.
 *  
 * If errtype is or'd with E_P, also call perror() for more info.
 * If we are a background task, print the task ordinal to tell the user
 * which task aborted.
 */
void
cl_error (int errtype, char *diagstr, ...)
{
	va_list	args;
	register struct task *tp;
	static	int nfatal = 0;
	static	int break_locks = 1;

	va_start (args, diagstr);

	/* Safety measure, in the event of error recursion.
	 */
	if (nfatal)
	    clexit();

	if (errlev++ > 2) {
	    nfatal++;
	    eprintf ("Error recursion.  Cl dies.\n");
	    clexit();
	}

	/* The first setjmp(errenv) is not done until we start the main loop.
 	 * Set validerrenv when start the first interactive cl to indicate that
	 * we may safely longjmp back to main's loop on an error.  ERRENV is
	 * not set for bkg jobs since error restart is not permitted.
	 */
	if (!validerrenv && !(firstask->t_flags & T_BATCH)) {
	    nfatal++;
            u_doprnt (diagstr, &args, currentask->t_stderr);
	    if (errtype & E_P)
	        perror ("\nOS errmsg");
	    else
		eprintf ("\n");
	    eprintf ("Fatal startup error.  CL dies.\n");
	    clexit();
	}

	/* Any error occurring during logout is fatal.
	 */
	if (loggingout || gologout) {
	    nfatal++;
            u_doprnt (diagstr, &args, currentask->t_stderr);
	    if (errtype & E_P)
	        perror ("\nOS errmsg");
	    else
		eprintf ("\n");
	    eprintf ("Fatal logout error.  CL dies.\n");
	    clexit();
	}

	/* Perform any ONERROR error recovery in the vos first.  Initialize
	 * the error recovery mechanism (necessary since the iraf main is not
	 * being allowed to do error recovery).
	 */
	c_xonerr (1);
	XER_RESET();	/* TODO: move into LIBC interface */

	/* Clear terminal raw mode if still set. */
	c_fseti ((XINT)STDIN, F_RAW, NO);

	if (firstask->t_flags & T_BATCH)
	    eprintf ("\n[%d] ", bkgno);
	if (errtype & E_IERR)
	    eprintf ("INTERNAL ");
	if (errtype & E_FERR)
	    eprintf ("FATAL ");
	if (currentask->t_flags & T_SCRIPT)
	    eprintf ("ERROR on line %d: ", currentask->t_scriptln);
	else
	    eprintf ("ERROR: ");

	u_doprnt (diagstr, &args, currentask->t_stderr);
	if (errtype & E_P)
	    perror ("\nOS errmsg");
	else
	    eprintf ("\n");

	/* Log the error message if from a script or an executable.
	 */
	if (!errlog && keeplog() && log_errors()) {
	    if (currentask->t_flags & T_SCRIPT || currentask->t_pid != -1) {
	    	PKCHAR  buf[SZ_LINE+1];
		FILE	*fp;
		int     fd;

		fd = c_stropen (buf, SZ_LINE, NEW_FILE);
		fp = fdopen (fd, "w");

		fprintf (fp, "ERROR: ");
		u_doprnt (diagstr, &args, fp);

		fclose (fp);
		c_close (fd);
	    	putlog (currentask, c_strpak (buf, (char *)buf, SZ_LINE));
	    }
	}
	errlog = 0;

	/* Initialize the current command block but do not log the command
	 * which aborted.
	 */
	yy_startblock (NOLOG);

	/* Delete all pipefiles.  Call iofinish() first as some OS's may
	 * require that the files be closed before they can be deleted.
	 */
	for (tp=currentask; !(tp->t_flags & T_INTERACTIVE); tp=next_task(tp)) {
	    iofinish (tp);
	    if (tp == firstask)
		break;
	}
	delpipes (0);

	/* Do not go on if this is a fatal error or we are unattended.
	 */
	if (errtype & E_FERR) {
	    nfatal++;
	    pr_dumpcache (0, break_locks);
	    clexit();
	} else if (firstask->t_flags & T_BATCH)
	    clshutdown();

	/* Reset state variables. */
	/* Most of these probably needn't be reset, but we'll play
	 * it safe.
	 */
	nestlevel = 0;			/* set nesting to 0 		*/
	offsetmode (0);			/* offset mode to index 	*/
	ncaseval = 0;			/* number of case values 	*/
	n_indexes = 0;
	imloopset = 0;			/* in an implicit loop 		*/
	n_oarr = 0;			/* implicit loop indicators 	*/
	i_oarr = 0;
	maybeindex = 0;			/* sexagesimal/index range	*/
	parse_state = PARSE_FREE;
	if (last_parm) {		/* have we tried to add a param	*/
	    last_parm->p_np = NULL;
	    currentask->t_pfp->pf_lastpp = last_parm;
	    last_parm = NULL;
	}


	/* Get back to an interactive state.
	 */
	taskunwind();

	/* If an abort occurs while interrupts are disabled they will never get
	 * reenabled unless we do so here.
	 */
	intr_reset();

	/* Go back to main loop in main().
	 */
	va_end (args);
	longjmp (errenv, 1);
}
