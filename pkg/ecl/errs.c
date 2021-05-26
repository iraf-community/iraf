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
extern	int cltrace;		/* trace execution			*/
extern	int errlev;		/* for detecting error recursion	*/
extern	int bkgno;		/* bkg task number, if batch job	*/
extern	int validerrenv;	/* set in main once get past login()	*/
extern	int loggingout;		/* set while reading from logout file	*/
extern	int gologout;		/* set when getting ready to " " "	*/
extern	int currentline;	/* current script line being executed 	*/
extern	int errorline;		/* error script line being recovered 	*/
extern	jmp_buf errenv;		/* setjmp() is in main().		*/

char	*e_appopen 	= "can not open `%s' for appending";
char	*e_badstrop 	= "illegal operation on string '%0.20s'";
char	*e_badsw 	= "bad switch case, %d, in `%s'";
char	*e_geonearg 	= "`%s' requires at least one argument";
char	*e_indexunf 	= "no indices on stack for array reference";
char 	*e_nominmax 	= "structs, strings, cursors and booleans have no ranges";
char	*e_nopfile 	= "task `%s' has no param file";
char	*e_badpfile 	= "cannot read parameter file `%s'";
char	*e_nostrcnv 	= "may not convert string to other types";
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
char	*e_uopcode 	= "undefined opcode %d";
char	*e_wopen 	= "cannot open `%s' for writing";
char	*e_lookparm 	= "error searching for parameter `%s'.";
char	*e_invaldef	= "conflicting attributes in definition of `%s'.";
char	*e_fdivzero 	= "floating divide by zero";
char	*e_idivzero 	= "integer divide by zero";
char	*e_fdzvalue 	= "floating divide by zero - using $err_dzvalue";
char	*e_idzvalue 	= "integer divide by zero - using $err_dzvalue";

/* 
char	*e_edom 	= "function argument outside valid range: %g";
char	*e_erange 	= "%g caused arithmetic overflow";
char	*e_fpe 		= "floating point exception";
char	*e_unlink 	= "cannot remove file `%s'";
char	*e_notbool 	= "parameter `%s' is not boolean";
*/

/* The 'errlog' variable is used to avoid duplicate error logging by the
 * builtin clerror() and the error function cl_error() below.  When a script
 * or executable tasks calls the CL language 'error' function, the builtin
 * clerror() logs the error message.  Otherwise, we'll log it here.
 */
int	errlog    = NO;

/* These variables are the various 'erract' options.
 */
int	err_abort      = YES;	/* abort on error		*/
int	err_beep       = YES;	/* beep on error		*/
int	err_trace      = YES;	/* print calling traceback	*/
int	err_flpr       = YES;	/* flush process cache		*/
int	err_clear      = YES;	/* clear CL status params	*/
int	err_full       = YES;	/* print full traceback?	*/
ErrCom	errcom;

extern	int in_iferr, do_error;
extern  char *onerr_handler;

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

        /* (Re)-initialize the error action.
         */
        erract_init();

	/* Safety measure, in the event of error recursion.
	 */
	if (err_abort) {
	    if (nfatal)
	        clexit();

	    if (errlev++ > 2) {
	        nfatal++;
	        eprintf ("Error recursion.  Cl dies.\n");
	        clexit();
	    }
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

	/* Disable error tracing if requested.
	 */
	if (err_trace == YES || (errtype & E_UERR)) {
	    if (currentask->t_flags & T_SCRIPT &&
	        currentask->t_flags & T_INTERACTIVE)
		    eprintf ("ERROR on line %d: ", errorline);
	    else
	        eprintf ("ERROR: ");

	    u_doprnt (diagstr, &args, currentask->t_stderr);
	    if (errtype & E_P)
	        perror ("\nOS errmsg");
	    else
	        eprintf ("\n");
	}

	/* Log the error message if from a script or an executable.
	 */
	if (!errlog && keeplog() && log_errors()) {
	    if (currentask->t_flags & T_SCRIPT || currentask->t_pid != -1) {
	    	PKCHAR  buf[SZ_LINE+1];
		FILE	*fp;
		int     fd;

		fd = c_stropen (buf, SZ_LINE, NEW_FILE);
		fp = fdopen (fd, "w");

		fprintf (fp, "ERROR on line %d: ", errorline);
		u_doprnt (diagstr, &args, fp);

		fclose (fp);
		c_close (fd);
	    	putlog (currentask, c_strpak (buf, (char *)buf, SZ_LINE));
	    }
	}
	errlog = 0;

	/* Initialize the current command block but do not log the command
	 * which aborted.  If we're only trapping errors and not fully 
  	 * recovering, don't reset the command block so we have the option
	 * to continue execution.
	 */
	if ((err_abort == YES && do_error == NO) || 
	    (do_error == YES || (errtype & E_UERR))) 
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


	/* Set the error flag.  */
	errcom.errflag++;
	errcom.nhandlers++;

	/* Get back to an interactive state.  We simply return if we're
	 * trapping errors except when processing a E_UERR.  These type
	 * messages come from the CL itself and require user attention to
	 * correct (e.g. task not found, parameter type/syntax errors, etc).
	 * The calling procedure is not expecting us to return, so we cannot
	 * properly trap without rewriting the calling code.
	 */
	if (cltrace) {
	    eprintf ("cl_error: abort=%d  beep=%d  trace=%d  flpr=%d\n", 
	        err_abort, err_beep, err_trace, err_flpr);
	    eprintf ("cl_error: code=%d do_err=%d errtype=%d/%d task='%s'\n", 
	        errcom.errcode, do_error, errtype, errtype&E_UERR, 
	        currentask->t_ltp->lt_lname);
	}
	if ((err_abort == YES && do_error == NO) || 
	    (do_error == YES || (errtype & E_UERR))) {
	        extern ErrCom errcom;
	        register struct param *pp;

	    	if (!errcom.errcode && (errtype & E_UERR)) {

                    errcom.errcode = errtype;
                    strcpy (errcom.errmsg, diagstr);
                    strcpy (errcom.task, currentask->t_ltp->lt_lname);

                    pp = paramfind (firstask->t_pfp, "$errno", 0, YES);
                    pp->p_val.v_i = errcom.errcode;
                    pp = paramfind (firstask->t_pfp, "$errmsg", 0, YES);
                    pp->p_val.v_s = errcom.errmsg;
                    pp = paramfind (firstask->t_pfp, "$errtask", 0, YES);
                    pp->p_val.v_s = errcom.task;
	    	}
	    	taskunwind();

	    	/* If an abort occurs while interrupts are disabled they will 
	    	 * never get reenabled unless we do so here.
	    	 */
	    	intr_reset();

	    	/* Go back to main loop in main().
	    	 */
	    	va_end (args);
	    	longjmp (errenv, 1);

	} else {
	    va_end (args);
	    return;
	}
}


/*  ERRACT_INIT -- Initialize the error recovery option string 'erract'.
 *  To be consistent this should really be a CL parameter similar to 
 *  'ehinit' but for the moment we don't want to change the CL pset so
 *  we implement it as an environment variable.  Also, unlike the params
 *  we allow the value to reset individual options and build the final
 *  value string at the end and put it back in the environment.
 */

#define NEXT_TOKEN  while (*act == ' ' || *act == '\t' || *act == '\n') act++;\
                    if (!*act) break;
#define NEXT_WHITE  while (*act != ' ' && *act != '\t' && *act != '\0') act++;

void 
erract_init (void)
{
	char *act, *envget();
	char opt[SZ_LINE];

	/* Parse the erract string to pick up new options.
	 */
	if ((act = envget ("erract"))) {
            while (*act) {
                NEXT_TOKEN;

                if      (strncmp (act, "abort", 3) == 0)
                    err_abort = YES;
                else if (strncmp (act, "noabort", 3) == 0)
                    err_abort = NO;
                else if (strncmp (act, "beep", 3) == 0)
                    err_beep = YES;
                else if (strncmp (act, "nobeep", 3) == 0)
                    err_beep = NO;
                else if (strncmp (act, "trace", 3) == 0)
                    err_trace = YES;
                else if (strncmp (act, "notrace", 3) == 0)
                    err_trace = NO;
                else if (strncmp (act, "flpr", 3) == 0)
                    err_flpr = YES;
                else if (strncmp (act, "noflpr", 4) == 0)
                    err_flpr = NO;
                else if (strncmp (act, "clear", 3) == 0)
                    err_clear = YES;
                else if (strncmp (act, "noclear", 3) == 0)
                    err_clear = NO;
                else if (strncmp (act, "full", 3) == 0)
                    err_full = YES;
                else if (strncmp (act, "nofull", 4) == 0)
                    err_full = NO;
                else if (*act != '\0')
                    eprintf ("unrecognized erract set-option `%s'\n", act);

                NEXT_WHITE;
            }
	}

	/* Now restore the environment variable to define all the options.
	 */
	sprintf (opt, "%s %s %s %s %s",
	    (err_abort      ? "abort"      : "noabort"),
	    (err_beep       ? "beep"      : "nobeep"),
	    (err_trace      ? "trace"      : "notrace"),
	    (err_flpr       ? "flpr"       : "noflpr"),
	    (err_clear      ? "clear"      : "noclear"),
	    (err_full       ? "full"       : "nofull") );
	c_envreset ("erract", opt);
}
