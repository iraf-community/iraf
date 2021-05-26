/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_fset
#define import_error
#define import_ctype
#define import_stdio
#define import_alloc
#define import_ttset
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "mem.h"
#include "operand.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "proto.h"


/*
 * BUILTIN -- This file contains the functions that perform the built-in
 *   commands of the cl, such as task, set, and package. also here is the
 *   code that adds these functions to the initial set of ltasks within the
 *   cl when it first starts up.
 * Setbuiltins() contains a table of functions and their user names; add
 *   to this table when adding new builtin functions.
 * The first comment line for each of the functions indicates the syntax of
 *   how it should be used by the user.  The grammar allows the arguments
 *   to be optionally surrounded by parentheses.
 *
 * It must be emphasized that these builtin commands do, in fact, run as tasks
 * just as any other task. the currentask pointer is pointing to this task.
 * since most of the commands manipulate the dictionary and these changes were
 * intended for the previous task (the one that did the command) the builtins
 * must modify the topd value saved in the previous task so the effect stays
 * when the builtin's task finishes; thus, the builtins do a kind of "keep".
 *
 * Further, when called, the dictionary contains the fake parameter file
 * manufactured for the builtin, as pointed to by currentask->t_pfp, but topd
 * and parhead have been put back the way they were before the command was
 * started.  Thus, if the builtin adds to the dictionary, it will overwrite its
 * parameters.  This is avoided by using pushxparams() which pushes the value
 * and name fields of the parameters in a pfile as operands.  The builtin may
 * then access these fields of its parameters, by popping them off the stack,
 * yet make dictionary additions.  The number of parameters is given by
 * the function nargs().
 */

extern	int cldebug, cltrace;		/* debug/trace flags		     */
extern	int lastjobno;			/* last background job spawned	     */
extern	int gologout;			/* flag to execute() to cause logout */
extern  int logout_status;      	/* optional arg to logout()          */
extern  int errorline;			/* error recover line		     */
extern  int currentline;		/* line currently being executed     */
extern	char *findexe();

extern	int do_error;			/* for error recovery/trapping 	     */

/* Device Allocation stuff (really should be in a separate package).
 */
#define	SZ_DEVNAME	12
#define	MAX_ALLOCDEV	10

struct d_alloc {
	short	allocated;
	char	devname[SZ_DEVNAME+1];
};

static	int nallocdev = 0;		/* Count of allocated devices	*/
static	int nlogouts = 0;		/* Count of logout attempts	*/
static	struct d_alloc
	allocdev[MAX_ALLOCDEV];		/* Save names of alloc devices	*/


/* BYE -- Called by our parent as the regular "bye" directive when it is
 * finished.  All we need to do is pop the currentask.  The normal handling
 * of builtins does an oneof() which will perform the actions for our parent. 
 * See execnewtask() for builtins.
 */
void
clbye (void)
{
	currentask = poptask();
}


/* LOGOUT -- Logout from a CL session.  Ignore the first attempts if there
 * are allocated devices, but if the user persists permit the logout with
 * the devices still allocated.
 */
void
cllogout (void)
{
	register int	n;
	register struct	d_alloc	*dv;
	register struct pfile *pfp;
	struct   operand o;
	char	 owner[SZ_FNAME+1];


	/* Set logout status value */
	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) > 0) {
	    pushbparams (pfp->pf_pp);   /* push so first popped is 1st param */
	    popop();                    /* discard the $n name          */
	    o = popop();                /* pop logout status number     */
	
	    if ((o.o_type & OT_BASIC) == OT_STRING) {
		eprintf ("Warning: logout status `%s' not a number\n",
	            o.o_val.v_s);
		nlogouts++;
		gologout = 1;		/* LOGOUT on third attempt */
		return;
	    }
	
	    pushop (&o);
	    opcast (OT_INT);
	    o = popop();
	    logout_status = o.o_val.v_i;
	} else
	    logout_status = 0;


	/* Clean up any allocated devices.
	 */
	if (nallocdev > 0) {
	    /* Examine each apparently allocated device to see if it is in
	     * fact still allocated.
	     */
	    for (n=0;  n < MAX_ALLOCDEV;  n++) {
		dv = &allocdev[n];
		if (dv->allocated)
		    if (c_devowner(dv->devname,owner,SZ_FNAME) != DV_DEVALLOC) {
			dv->allocated = NO;
			--nallocdev;
		    }
	    }

	    /* Always print message if devices are allocated.
	     */
	    if (nallocdev) {
		eprintf ("The following devices are still allocated:");
		for (n=0;  n < MAX_ALLOCDEV;  n++)
		    if (allocdev[n].allocated)
			eprintf (" %s", allocdev[n].devname);
		eprintf ("\n");
	    }

	    if (nallocdev <= 0 || nlogouts++ > 1)
		gologout = 1;		/* LOGOUT on third attempt */

	} else
	    gologout = 1;		/* LOGOUT */
}


/* CLBYE -- Like cl(), but sets end of file on the current file.  This is
 * done by the simple expedient of reopening the currentasks t_in as the null
 * file, to ensure that anything which reads from the stream will see EOF.
 * The reopen is performed in exec.c.
 */
void
clclbye (void)
{
}


/* CACHE ltask [, ltask...]
 * read in and keep pfiles for given ltasks. since they are pre-loaded,
 *   used to avoid reading pfile for each invokation of tasks. since they
 *   will not be above the new topd when the task bye's, they won't get
 *   flushed out either unless an explicit UPDATE is done or until the task
 *   that called us bye's.
 * we check that the pfile is not already loaded and do nothing if it is.
 */
void
clcache (void)
{
	register struct pfile *pfp;
	char	pfilename[SZ_PATHNAME];
	char	**list, **next;
	struct	operand o;
	int	n, npfile;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) < 1) {
	    static  int first_col=7, maxch=20, ncol=0;
	    int	    last_col;

	    last_col = c_envgeti ("ttyncols");

	    /* List all currently loaded paramfiles.
	     */
	    for (npfile=0, pfp = reference (pfile, parhead);  pfp != NULL;
	    pfp = pfp->pf_npf) {
		if (!(pfp->pf_flags & (PF_FAKE|PF_COPY)))
		    npfile++;
	    }

	    list = next = (char **)memneed (npfile);
	    for (pfp = reference (pfile, parhead);  pfp != NULL;
	    pfp = pfp->pf_npf)
		if (!(pfp->pf_flags & (PF_FAKE|PF_COPY))) {
		    strcpy (pfilename, pfp->pf_ltp->lt_pkp->pk_name);
		    strcat (pfilename, ".");
		    strcat (pfilename, pfp->pf_ltp->lt_lname);
		    *next++ = comdstr (pfilename);
		}
	    strsort (list, npfile);
	    strtable (newtask->t_stdout, list, npfile, first_col, last_col,
		maxch, ncol);

	} else {
	    /* Add listed pfiles to the cache.
	     */
	    pushbparams (pfp->pf_pp);
	    while (n--) {
		popop();			/* discard fake name.	*/
		o = popop();			/* get ltask		*/
		pfilesrch (o.o_val.v_s);
	    }

	    /* Retain the pfiles read in. */
	    keep (prevtask);
	}
}


/* CL_LOCATE -- Locate the named task in the package list.
 */
void
cl_locate (char *task_spec, int first_only)
{
	char	buf[SZ_LINE];
	char	*pkname, *ltname, *junk;
	struct	package *pkp;
	struct  ltask   *stat;
	int	found = 0;

	strcpy (buf, task_spec);
	breakout (buf, &junk, &pkname, &ltname, &junk);

	if (pkname[0] != '\0') {	/* explicit package named	*/
	    if ((pkp = pacfind (pkname)) == NULL)
		cl_error (E_UERR, e_pcknonexist, pkname);
	    if ((stat = ltaskfind (pkp, ltname, 1)) == (struct ltask *) NULL)
		oprintf ("%s'\n", pkname);

	} else {			/* search all packages		*/
	    pkp = reference (package, pachead);
	    stat = NULL;

	    while (pkp != NULL) {
		stat = ltaskfind (pkp, ltname, 1);
		if (stat == (struct ltask *) ERR)
	    	    cl_error (E_UERR, e_tambig, ltname);
		else if (stat != NULL) {
	            oprintf ("%s", pkp->pk_name);
		    found++;
		    if (first_only == YES)
			break;
	            oprintf (" ");
		}
		pkp = pkp->pk_npk;
	    }
	}

	if (found == NULL)
	    oprintf ("%s: task not found.\n", task_spec);
	else
	    oprintf ("\n");
}


/* CLWHICH -- Locate the named task in the package list.
 */
void
clwhich (void)
{
	register struct pfile *pfp;
	struct	operand o;
	int	n;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) < 1)
	    cl_error (E_UERR, e_geonearg, "which");

	pushbparams (pfp->pf_pp);
	while (n--) {
	    popop();			/* discard fake name.		*/
	    opcast (OT_STRING);
	    o = popop();		/* get ltask			*/

	    cl_locate (o.o_val.v_s, YES);
	}
}



/* CLWHEREIS -- Locate all occurances of named task in the package list.
 */
void
clwhereis (void)
{
	register struct pfile *pfp;
	struct	operand o;
	int	n;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) < 1)
	    cl_error (E_UERR, e_geonearg, "whereis");

	pushbparams (pfp->pf_pp);
	while (n--) {
	    popop();			/* discard fake name.		*/
	    opcast (OT_STRING);
	    o = popop();		/* get ltask			*/

	    cl_locate (o.o_val.v_s, NO);
	}
}


/* FLPRCACHE -- Flush the process cache.  If no args, flush all but locked
 * processes.  If arg=0, flush all processes and override locks.  If argn=N,
 * flush process N.
 */
void
clflprcache (void)
{
	register struct pfile *pfp;
	register int n, pid;
	struct	operand o;
	struct	ltask *ltp;
	int	break_locks = 1;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0) {
	    pr_dumpcache (0, !break_locks);
	    return;
	}

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */
	while (--n >= 0) {
	    popop();			/* discard the $n name		*/
	    o = popop();		/* pop proc name or number	*/

	    if ((o.o_type & OT_BASIC) == OT_STRING) {
		ltp = ltasksrch ("", o.o_val.v_s);
		if (ltp->lt_flags & (LT_SCRIPT|LT_BUILTIN|LT_FOREIGN|LT_PSET))
		    pid = NULL;
		else
		    pid = pr_pnametopid (findexe(ltp->lt_pkp,
			ltp->lt_u.ltu_pname));
		if (pid == NULL) {
		    eprintf ("Warning: task `%s' not in cache\n", o.o_val.v_s);
		    continue;
		}
	    } else {
		pushop (&o);
		opcast (OT_INT);
		o = popop();
		pid = o.o_val.v_i;
	    }

	    pr_dumpcache (pid, break_locks);
	}
}


/* FLPR_TASK -- Flush the named task from the process cache.  
 */
void 
flpr_task (char *task)
{
	register int pid, break_locks = 1;
	struct	ltask *ltp;

	if (strcmp (task, "cl") == 0)
	    return;

	ltp = ltasksrch ("", task);
	if (ltp->lt_flags & (LT_SCRIPT|LT_BUILTIN|LT_FOREIGN|LT_PSET))
	    pid = NULL;
	else
	    pid = pr_pnametopid (findexe(ltp->lt_pkp, ltp->lt_u.ltu_pname));

	if (pid)
	    pr_dumpcache (pid, break_locks);
}


/* CLPRCACHE -- If no args list the contents of the process cache, else lock
 * the named tasks into the cache, connecting the associated process if
 * necessary.
 */
void
clprcache (void)
{
	register struct pfile *pfp;
	register int n, pid;
	struct	operand o;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0) {
	    pr_listcache (currentask->t_stdout);
	    return;
	}

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */
	while (--n >= 0) {
	    popop();			/* discard the $n name		*/
	    o = popop();

	    if ((o.o_type & OT_BASIC) == OT_STRING) {
		if ((pid = pr_cachetask (o.o_val.v_s)) == ERR)
		    continue;
	    } else {
		pushop (&o);
		opcast (OT_INT);
		o = popop();
		pid = o.o_val.v_i;
	    }

	    pr_lock (pid);
	}
}


/* CLGFLUSH -- Flush any buffered graphics output.  Output to stdplot is
 * buffered to permit appending to a plot.  We are called to flush this
 * last plot to the plotter.
 */
void
clgflush (void)
{
	c_gflush (STDGRAPH);
	c_gflush (STDIMAGE);
	c_gflush (STDPLOT);
}


static	char cd_curr[SZ_PATHNAME];	/* current directory	*/
static	char cd_prev[SZ_PATHNAME];	/* previous directory	*/
static	char cd_emsg[] = "Cannot change directory to `%s'";

/* CHDIR -- Change the current working directory.  If the change is successful
 * update the cwd of all child processes as well.
 */
void
clchdir (void)
{
	register struct pfile *pfp;
	struct	operand o;
	char	*dirname;
	char	*index(), *envget();

	pfp = newtask->t_pfp;
	if (nargs (pfp) <= 0) {
	    o.o_type = OT_STRING;
	    if ((o.o_val.v_s = envget ("home")) == NULL)
		cl_error (E_UERR, "No home directory defined in environment");
	} else {
	    pushbparams (pfp->pf_pp);
	    popop();				/* discard the $1 	*/
	    opcast (OT_STRING);
	    o = popop();			/* get directory spec	*/
	}

	/* Record the current directory the first time we are called.
	 */
	if (cd_curr[0] == EOS)
	    c_fpathname ("", cd_curr, SZ_PATHNAME);

	/* Attempt to change the directory.
	 */
	dirname = o.o_val.v_s;
	if (o.o_type != OT_STRING)
	    cl_error (E_UERR, cd_emsg, "??");
	else if (c_fchdir (dirname) == ERR)
	    cl_error (E_UERR, cd_emsg, dirname);

	/* Update cwd in all connected child processes. */
	pr_chdir (0, dirname);

	/* Update current and previous directory names. */
	strcpy (cd_prev, cd_curr);
	c_fpathname ("", cd_curr, SZ_PATHNAME);
}


/* BACK -- Return to the previous directory.
 */
void
clback (void)
{
	char	dirname[SZ_PATHNAME];

	if (cd_prev[0] == EOS)
	    cl_error (E_UERR, "no previous directory");
	else
	    strcpy (dirname, cd_prev);

	if (c_fchdir (dirname) == ERR)
	    cl_error (E_UERR, cd_emsg, dirname);

	/* Update cwd in all connected child processes. */
	pr_chdir (0, dirname);

	/* Update current and previous directory names. */
	strcpy (cd_prev, cd_curr);
	strcpy (cd_curr, dirname);

	/* Since we are the source of the directory name, rather than the
	 * user, print new directory name to ensure that there are no
	 * surprises.
	 */
	oprintf ("%s\n", dirname);
}


/* ERROR -- error code, message
 * Print message on our stderr and pop back to a terminal cl task
 * by handling it just like any other abortive type error.
 */
void
clerror (void)
{
	register struct	param *arg1, *arg2, *pp;
	register struct pfile *pfp;
	int	errcode;
	char	*errmsg;
	extern  char *onerr_handler;
	extern  ErrCom errcom;
	
	erract_init();
	if (currentline < currentask->t_scriptln)
	    errorline = currentline;

	pfp = newtask->t_pfp;
	if (nargs (pfp) != 2)
	    cl_error (E_IERR, e_twoargs, "error()");
	arg1 = pfp->pf_pp;
	arg2 = arg1->p_np;

	if (arg1 && (arg1->p_valo.o_type & OT_BASIC) == OT_INT)
	    errcode = arg1->p_val.v_i;
	else
	    errcode = 1;
	if (arg2 && (arg2->p_valo.o_type & OT_BASIC) == OT_STRING)
	    errmsg = arg2->p_val.v_s;
	else
	    errmsg = "";

	/* Call any posted error handlers.
	 */
	if (onerr_handler) {
	    /* Context:
    	     *    onerr_handler			- handler task
	     *    prevtask->t_ltp->lt_lname	- toplevel task
	     *    currentask->t_ltp->lt_lname	- task that failed
	     *    newtask->t_ltp->lt_lname	- "error()"
	    if (cltrace) 
		eprintf ("clerror: calling '%s' onerr_handler\n",
		    onerr_handler);
	    onerr_spawn (onerr_handler);
	     */
	}

	/* Pop the ERROR task, i.e., us.
	 */
	currentask = poptask();

	/* Log the error message if from a script or an executable.  Also, 
	 * tell the CL error handler that we've already logged the error, by
	 * setting the 'errlog' flag.
	 */
	if (keeplog() && log_errors()) {
	    if (currentask->t_flags & T_SCRIPT || currentask->t_pid != -1) {
	    	char  buf[SZ_LINE];
		extern	int  errlog;			/* see errs.c */

		strcpy (buf, "ERROR: ");
		strcat (buf, errmsg);
	    	putlog (currentask, buf);
		errlog = 1;
	    }
	}

	/* Save the error state. 
	 */
	errcom.errcode = errcode;
	strcpy (errcom.errmsg, errmsg);
	strcpy (errcom.task, currentask->t_ltp->lt_lname);

	pp = paramfind (firstask->t_pfp, "$errno", 0, YES);
	pp->p_val.v_i = errcom.errcode;
	pp = paramfind (firstask->t_pfp, "$errmsg", 0, YES);
	pp->p_val.v_s = errcom.errmsg;
	pp = paramfind (firstask->t_pfp, "$errtask", 0, YES);
	pp->p_val.v_s = errcom.task;


	/* Error Recovery -- If we're in an IFERR and ignoring errors don't
	 * go through with the abort.  Simply clean up, print/save the message,
	 * and return so we can continue execution.
	 */
	if (err_abort == NO || do_error == NO) {
	    /* Note we don't properly check the stack before getting the
	     * calling frame.  Could be a problem if we weren't called in
	     * response to an error from a connected subprocess and using
	     * this trick to get the calling script.
	     */ 
	    struct task *script = (struct task *)&stack[topcs+TASKSIZ];
	    extern ErrCom errcom;

	    iofinish (currentask);
	    if (err_beep)
		clbeep();
	    if (err_flpr)
	 	flpr_task (errcom.task);

	    errcom.errflag++;
	    errcom.errcode = errcode;
	    strcpy (errcom.errmsg, errmsg);
            if (script->t_ltp && script->t_ltp->lt_lname)
                strcpy (errcom.script, script->t_ltp->lt_lname);
            else
                strcpy (errcom.script, "CL");


	    if (err_trace == YES) {
	        eprintf ("Error (%d): on line %d of '%s' from '%s':\n\t'%s'\n", 
		    errcom.errcode, errcom.linenum, errcom.script,
		    errcom.task, errmsg);
	    }

	    return;
	}

	/* ERROR terminates a task like BYE.  Pop the task which issued
	 * the error statement, provided it was not the first task.
	 */
	iofinish (currentask);
	if (err_beep)
	    clbeep();
	if (err_flpr)
	    flpr_task (errcom.task);
	if (currentask != firstask)
	    currentask = poptask();

	/* Now abort.  This will unwind us back to the last interactive
	 * task.  Any external child processes will be interrupted.  If
	 * a child process issued the ERROR it will not be interrupted,
	 * because we already popped it above.
	 */
	cl_error (E_UERR, "%s", errmsg);
}


/* ? and ?? help commands.
 * see listhelp() and listallhelp() in gram.c.
 * note that since these names, ? and ??, do not fall under the ident lex
 * rule, they need a special entry in the lex rule tables.
 */
void
clhelp (void)
{
	register struct pfile *pfp;
	register struct package *pkp;
	struct	operand o;
	int	n, nleft, show_invis=NO;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0)
	    listhelp (curpack, show_invis);
	else {
	    pushbparams (pfp->pf_pp);
	    for (nleft=n;  nleft > 0;  nleft--) {
		popop();
		o = popop();
		if ((o.o_type & OT_BASIC) != OT_STRING)
		    cl_error (E_UERR, "non-string argument");
		if (o.o_val.v_s[0] == CH_INVIS) {
		    show_invis = YES;
		    if (n == 1)
			listhelp (curpack, show_invis);
		} else if ((pkp = pacfind (o.o_val.v_s)) == NULL) {
		    eprintf ("Warning: package '%s' not found\n", o.o_val.v_s);
		} else if ((XINT) pkp == ERR) {
		    cl_error (E_UERR, e_pckambig, o.o_val.v_s);
		} else {
		    if (n > 1)
			oprintf ("    %s:\n", pkp->pk_name);
		    listhelp (pkp, show_invis);
		}
	    }
	}
}

void
clallhelp (void)
{
	int	show_invis = NO;

	listallhelp (show_invis);
}


/* CLHISTORY -- Print the command history.  We keep the number of history
 * blocks to print in static storage, starting with a default of 20.  This
 * number is "learned" if the user calls history with the max_history arg.
 */
void
clhistory (void)
{
	register struct pfile *pfp;
	struct	operand o;
	static	int default_max_history = 15;
	int	max_history;

	max_history = default_max_history;
	pfp = newtask->t_pfp;

	if (nargs (pfp) > 0) {
	    pushbparams (pfp->pf_pp);
	    popop();				/* discard the $1 	*/
	    o = popop();			/* get max records 	*/
	    if (o.o_type != OT_INT)
		cl_error (E_UERR,
		    "'history' arg is max number of records to print");
	    max_history = o.o_val.v_i;

	    /* Negative valued argument does not permanently change the
	     * default.
	     */
	    if (max_history >= 0)
		default_max_history = max_history;
	    else
		max_history = -max_history;
	}

	show_history (newtask->t_stdout, max_history);
}


/* CLTRACE -- Enable or disable instruction tracing (d_trace).
 */
void
dotrace (void)
{
	register struct pfile *pfp;
	struct operand o;
	int value = !cltrace;

	pfp = newtask->t_pfp;

	if (nargs (pfp) > 0) {
	    pushbparams (pfp->pf_pp);
	    popop();				/* discard the $1 	*/
	    o = popop();
	    if (o.o_type != OT_INT)
		cl_error (E_UERR, "trace arg should be an integer");
	    value = o.o_val.v_i;
	}

	d_trace (value);
}


/* CLEHISTORY -- Edit command history.  (dummy - see history.c)
 */
void
clehistory (void)
{
}


/* CLSERVICE -- Service a query from a task in the background.  The argument
 * is the job number, default [1].
 */
void
clservice (void)
{
	register struct pfile *pfp;
	struct	operand o;
	int	bkgjob;

	pfp = newtask->t_pfp;
	if (nargs (pfp) < 1)
	    bkgjob = lastjobno;
	else {
	    pushbparams (pfp->pf_pp);
	    popop();				/* discard the $1 	*/
	    o = popop();			/* get max records 	*/
	    if (o.o_type != OT_INT)
		cl_error (E_UERR,
		    "'service' arg is ordinal of bkg job to be serviced");
	    bkgjob = o.o_val.v_i;
	}

	service_bkgquery (bkgjob);
}


/* keep
 * this command is used when changes to the dictionary, as with task
 * or package directives for example, are to be saved after the task that
 * issues the "keep" dies. since the keep command itself is handled as a
 * task, this means the t_topd value saved two levels above the current
 * task has to be modified.
 * control stack grows downward so previous tasks are higher than currentask.
 * because it was the very first task, it makes no sense for the initial
 * interactive cl to do a keep.
 */
void
clkeep (void)
{
	register struct	task *tp, *root_task = (struct task *) NULL;


	if (strncmp ("keep", currentask->t_ltp->lt_lname, 4) == 0) {
	    if (nargs (newtask->t_pfp) > 0)
	        cl_error (E_UERR, "`keep' command has no arguments");
	} else if (prevtask == firstask)
	    return;

	/* If reading from the standard input, keep only the context of our
	 * caller.
	 */
	if (prevtask->t_in == firstask->t_in) {
	    keep (next_task(prevtask));
	    return;
	}

	/* Find the earliest task on the control stack which is reading from
	 * the same command input stream (script file) as our caller, and
	 * keep the context of all tasks from that point up to the present.
	 */
	for (tp=prevtask;  tp != firstask;  tp = next_task(tp)) {
	    if (tp->t_in == prevtask->t_in)
		root_task = tp;
	}

	for (tp=prevtask;  tp != firstask;  tp = next_task(tp)) {
	    keep (next_task(tp));
	    if (tp == root_task)
		break;
	}
}


/* kill job [, job]
 * zap background jobs, as defined by their one-indexed "job number".
 * job zero is a special case that means kill all jobs.
 * see bkg.c for more discussion and bkgkill().
 */
void
clkill (void)
{
	register struct pfile *pfp;
	register int n, jn;
	struct operand o;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0)
	    cl_error (E_UERR, "must specify job number(s)");

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */

	while (n--) {
	    popop();		/* discard the $n name		*/
	    opcast (OT_INT);	/* insure we get an integer	*/
	    o = popop();		/* pop job number, as int	*/
	    jn = o.o_val.v_i;

	    bkg_kill (jn);
	}
}


/* EPARAM -- Parameter set editor.
 */
void
cleparam (void)
{
	register struct pfile *pfp;
	int	n, nleft, quit;
	struct	operand o;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0)
	    return;

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */
	quit = NO;

	for (nleft=n;  nleft > 0;  nleft--) {
	    popop();		/* discard the $n name			*/
	    o = popop();	/* get task name (value of the param)	*/

	    if (!quit && (o.o_type & OT_BASIC) == OT_STRING)
		quit = (epset (o.o_val.v_s) == ERR);
	    else
		cl_error (E_UERR,
		    "eparam: argument must be taskname or pfilename");
	}
}


/* LPARAM name1, name2, ...
 * go through params for each named task and list their names, current value,
 * and prompt string. go through twice, giving all non-hidden ones first.
 * if a pfile is needed and it is not in core already, it is read in just
 *   long enough to display then discarded. it might be argued that lparam
 *   should have a kind of implied pre-loading cache effect since a task whose
 *   params are being inspected is likely to be used soon. if this effect is
 *   wanted, just add the topd saving line as with task, cache, etc.
 */
void
cllparam (void)
{
	register struct ltask *ltp;
	register struct pfile *pfp;
	struct	operand o;
	int	n, nleft;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0)
	    return;

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */

	for (nleft=n;  nleft > 0;  nleft--) {
	    popop();		/* discard the $n name			*/
	    o = popop();	/* get task name (value of the param)	*/
	    if ((o.o_type & OT_BASIC) == OT_STRING) {
		pfp = pfilesrch (o.o_val.v_s);
		ltp = pfp->pf_ltp;
		if (n > 1)
		    oprintf ("    %s:\n", ltp->lt_lname);
		listparams (pfp);
	    } else
		cl_error (E_UERR, "lparam: argument must be a taskname");
	}
}


/* DPARAM name1, name2, ...
 * Dump the parameters for the named tasks to the standard output in the
 * form of a series of `task.param=value' assignments.
 */
void
cldparam (void)
{
	register struct pfile *pfp;
	struct	operand o;
	int	n, nleft;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0)
	    return;

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */

	for (nleft=n;  nleft > 0;  nleft--) {
	    popop();		/* discard the $n name			*/
	    o = popop();	/* get task name (value of the param)	*/

	    if ((o.o_type & OT_BASIC) == OT_STRING) {
		pfp = pfilesrch (o.o_val.v_s);
		dumpparams (pfp);
	    } else
		cl_error (E_UERR, "dparam: argument must be a taskname");
	}
}


/* PACKAGE name
 * this function is to create a new package structure off pachead
 *   so that when the previous process continues, it will be its new curpack.
 *   the packages pfile is to be the parent's also.
 * since we want the effect to remain for the parent, we store the new
 *   package pointer in (currentask+1)->t_curpack so restor() will stuff it
 *   into curpack. we also need to "keep" the new topd so that restor doesn't
 *   lob off the new package again. this is a complexity that results from
 *   running builtin functions as tasks in their own right.
 * no point in setting curpack as it will get overwritten by restor() as soon
 *   as this returns.
 * if called without arguments, just give a list of packages, in current 
 *   circular search order.
 * set LT_DEFPCK if the new package name is the same as the task defining it.
 *   used by cmdsrch() to guard against rerunning a script that defines a pkg.
 * call error() and do not return if this would redefine the package.
 */
void
clpack (void)
{
	register struct pfile	*pfp;
	register struct task	*tp;
	register struct package *pkp;
	char	*paknam, *bindir;
	struct	operand o1, o2;
	int	n;

	pfp = newtask->t_pfp;
	if (nargs (pfp) > 2)
	    cl_error (E_UERR, "too many arguments");

	if ((n = nargs(pfp)) == 0) {
	    pkp = curpack;
	    do {
		oprintf ("    %s\n", pkp->pk_name);
		if ((pkp = pkp->pk_npk) == NULL)
		    pkp = reference (package, pachead);
	    } until (pkp == curpack);
	    return;
	}

	/* Get name of new package. */
	pushbparams (pfp->pf_pp);
	popop();		/* discard param's $n name	*/
	opcast (OT_STRING);
	o1 = popop();
	paknam = o1.o_val.v_s;

	/* Search up the task stack for a script task with the same name as
	 * the new package.  Note that if other packages were loaded before
	 * the PACKAGE statement was executed, the task descriptor for the
	 * package script task will not be the previous task.
	 */
	for (tp = prevtask;  tp != firstask; tp = next_task(tp))
	    if (!strcmp (paknam, tp->t_ltp->lt_lname))
		break;

	/* Determine the bindir for the package.  This may be given on the
	 * command line, otherwise we inherit the bindir of the package to
	 * which the new package being defined belongs.
	 */
	if (n > 1) {
	    opcast (OT_STRING);
	    o2 = popop();
	    opcast (OT_STRING);
	    o2 = popop();
	    bindir = o2.o_val.v_s;
	} else
	    bindir = tp->t_ltp->lt_pkp->pk_bin;

	/* Check for redefinition. */
	if (pacfind (paknam) != NULL)
	    cl_error (E_UERR, "package redefinition: `%s'", paknam);

	/* Enter the new package definition into the dictionary.  */
	pkp = newpac (paknam, bindir);

	/* Set the pfile pointer for the new package to the pfile for the
	 * containing script task of the same name.  Flag the ltask entry
	 * to indicate that the ltask is a package.
	 */
	pkp->pk_pfp = tp->t_pfp;
	tp->t_ltp->lt_flags |= LT_DEFPCK;

	/* Set the current process cache process number (assigned in time
	 * order) for the task immediately preceding the one which called
	 * us.  This causes restor() to prune all recently connected processes
	 * from the process cache when we exit.
	 */
	if (tp != firstask)
	    next_task(tp)->t_pno = pr_getpno();

	/* Patch the saved curpack of the previous task (whatever it was) so
	 * that when we return the newly declared package will become the
	 * current package.  Call KEEP so that the new entry does not go away
	 * when the PACKAGE decl-task exits.
	 */
	prevtask->t_curpack = pkp;
	keep (prevtask);
}


/* _CURPACK -- Print the name of the "current" package, i.e., the name of
 * the first package in the search path for a command.
 */
void
clcurpack (void)
{
	tprintf ("%s\n", curpack->pk_name);
}


/* clpackage
 * this is just a null function to allow changing the current package to
 * clpackage. it is necessary due to the way cmdsrch() works, which looks
 * for an ltask named clpackage, then checks to see if there is a package
 * of the same name. if there is, it changes to it. thus, we need a fake
 * "task" for cmdsrch() to find so we may change to clpackage.
 */
void
clpkg (void)
{
}

/* language
 * Fake task for the "language" package.
 */
void
lapkg (void)
{
}


/* CLPRINT -- Formatted output.  Print arguments on the standard
 * output.
 */
void
clprint (void)
{
	do_clprint ("stdout");
}


/* CLFPRINT -- Formatted output.  Print arguments 2-N on the stream or
 * in the param named by the first argument.
 */
void
clfprint (void)
{
	do_clprint ("");
}


void
do_clprint (char *dest)
{
	/* x1 and x2 are just place holders for the call to breakout.
	 */
	struct	pfile *pfp;
	struct	param *pp;
	FILE	*fout;
	char	*pkname, *ltname, *pname, *field;
	char	outbuf[SZ_LINE];
	struct	operand o, out;
	int	type, op, n, nleft;

	pfp = newtask->t_pfp;
	pushbparams (pfp->pf_pp);  /* push so first popped is first param */

	/* Get the number of the first argument.  If not "$1", i.e. when
	 * calling as "print (,x,y,z)", default dest to the standard output.
	 * Otherwise, get the first parameter (name of the destination
	 * stream or param) and save for later.
	 */

	if ((n = nargs (pfp)) < 1)
	    goto argerr;

	out = popop();		/* get argument number "$n"		*/
	if (strcmp (dest, "stdout") == 0 || strcmp (out.o_val.v_s, "$1") != 0) {
	    /* n == 1 is ok here: syntax "print (,xx)"	*/
	    pushop (&out);
	    out.o_val.v_s = "stdout";
	} else {
	    out = popop();	/* get dest name (param name or stream)	*/
	    if (n == 1)
argerr:		cl_error (E_UERR, "Too few arguments to print or fprint");
	    n = n - 1;
	}

	/* Format the output string.
	 */
	op = 0;
	outbuf[op] = '\0';
	for (nleft = n;  nleft > 0;  nleft--) {
	    popop();		/* discard the $n name			*/
	    o = popop();
	    sprop (&outbuf[op], &o);
	    while (outbuf[op] != '\0')
		op++;
	    /* If operand is a number, add a space after the number.
	     */
	    type = o.o_type & OT_BASIC;
	    if (type == OT_INT || (type == OT_REAL && nleft > 1)) {
		outbuf[op++] = ' ';
		outbuf[op] = '\0';
	    }
	    if (op >= SZ_LINE)
		cl_error (E_UERR, "Output line too long in 'print'");
	}
		
	/* Examine the destination string and output the formatted
	 * string.  Destination may be stdout, stderr, or a parameter.
	 */
	breakout (out.o_val.v_s, &pkname, &ltname, &pname, &field);

	makelower (pname);
	fout = NULL;
	if (pkname[0] == '\0' && ltname[0] == '\0') {
	    if (strcmp (pname, "stdout") == 0 || pname[0] == '\0')
		fout = currentask->t_stdout;
	    else if (strcmp (pname, "stderr") == 0)
		fout = currentask->t_stderr;
	}

	if (fout != NULL) {		/* send to task stdout or err	*/
	    outbuf[op++] = '\n';	/* append newline		*/
	    outbuf[op] = '\0';
	    fputs (outbuf, fout);
	} else {
	    o.o_type = OT_STRING;	/* destination is a param	*/
	    o.o_val.v_s = outbuf;
	    pushop (&o);
	    pp = paramsrch (pkname, ltname, pname);
	    paramset (pp, field[0]);
	}
}


/* CLPRINTF -- Formatted print command (interface to VOS printf).
 */
void
clprintf (void)
{
	struct	pfile *pfp;
	struct	operand o;
	int	arg, n;

	pfp = newtask->t_pfp;
	pushbpvals (pfp->pf_pp);
	if ((n = nargs (pfp)) < 1)
	    cl_error (E_UERR, "printf: insufficient arguments\n");

	/* Output format. */
	o = popop();
	if ((o.o_type & OT_BASIC) != OT_STRING)
	    cl_error (E_UERR, "printf: bad format string\n");
	c_fprintf (fileno(currentask->t_stdout), o.o_val.v_s);

	/* Pass the operand values. */
	for (arg=2;  arg <= n;  arg++) {
	    o = popop();
	    if (opindef(&o)) {
		c_pargstr ("INDEF");
	    } else if (opundef(&o)) {
		cl_error (E_UERR, "printf: argument %d has undefined value\n",
		    arg);
	    } else {
		switch (o.o_type & OT_BASIC) {
		case OT_BOOL:
		case OT_INT:
		    c_pargi (o.o_val.v_i);
		    break;
		case OT_REAL:
		    c_pargd (o.o_val.v_r);
		    break;
		case OT_STRING:
		    c_pargstr (o.o_val.v_s);
		    break;
		default:
		    cl_error (E_UERR, "printf: bad operand type\n");
		}
	    }
	}
}


/* CLSCAN -- The scan function called as a task to scan from the standard
 * input, e.g. a pipe.  (Name changed to clscans to avoid a name clash
 * with fmtio.clscan).
 */
void
clscans (void)
{
	struct	pfile *pfp;

	pfp = newtask->t_pfp;
	pushbpvals (pfp->pf_pp);
	cl_scan (nargs(pfp)-1, "stdin");
	popop();
}


/* CLSCANF -- Formatted scan function.
 */
void
clscanf (void)
{
	struct	pfile *pfp;
	struct	operand o;
	int	n;

	pfp = newtask->t_pfp;
	pushbpvals (pfp->pf_pp);
	if ((n = nargs (pfp)) < 1)
	    cl_error (E_UERR, "scanf: insufficient arguments\n");

	/* Get scan format. */
	o = popop();
	if ((o.o_type & OT_BASIC) != OT_STRING)
	    cl_error (E_UERR, "scanf: bad format string\n");

	cl_scanf (o.o_val.v_s, nargs(pfp)-2, "stdin");
	popop();
}


/* PUTLOG user-msg 
 * Write a user message to the logfile.  The current pkg.task, bkg info, and
 * a time stamp are added by the putlog() function (in history.c).
 */
void
clputlog (void)
{
	register struct pfile *pfp;
	struct	operand o;
	char	*usermsg;
	int	n;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) < 1)
	    usermsg = "";
	else {
	    pushbparams (pfp->pf_pp);
	    popop();			/* discard fake name.		*/
	    opcast (OT_STRING);
	    o = popop();		/* get user string 		*/
	    usermsg = o.o_val.v_s;
	    while (--n) {		/* get rid of any extra args */
	    	popop();		/* discard fake name	*/
	    	popop();		/* discard extra arg	*/
	    }
	}

	/* Call putlog with the calling task and the user's message.
	 */
	putlog (prevtask, usermsg);
}


/* set [name = value]
 * if (no arguments)
 *   give a list of existing enviroment settings
 * else
 *   add an entry into the environment table name=value.
 *   update environ list in all connected child procs.
 */
void
clset (void)
{
	register struct pfile *pfp;
	struct	operand onam, oval;
	int	scantemp, n, show_redefs=YES;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) == 0)
	    c_envlist (fileno(currentask->t_stdout), "    ", show_redefs);
	else {
	    pushfparams (pfp->pf_pp);
	    while (n--) {
		opcast (OT_STRING);
		onam = popop();
		if (sscanf (onam.o_val.v_s, "$%d", &scantemp) == 1)
		    cl_error (E_UERR, "set must use name=value pairs");
		opcast (OT_STRING);
		oval = popop();
		c_envputs (onam.o_val.v_s, oval.o_val.v_s);
		pr_envset (0, onam.o_val.v_s, oval.o_val.v_s);
		if (strcmp ("erract", onam.o_val.v_s) == 0)
		    erract_init();
	    }

	    /* Prevent envfree in poptask when SET terminates from discarding
	     * this definition!!
	     */
	    c_envmark (&prevtask->t_envp);
	}
}


/* reset [name = value]
 * if (no arguments)
 *   give a list of existing enviroment settings
 * else
 *   reset (overwrite) the value of the named environment variable.
 *   update environ list in all connected child procs.
 */
void
clreset (void)
{
	register struct pfile *pfp;
	struct	operand onam, oval;
	int	scantemp, n, show_redefs=YES;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) == 0)
	    c_envlist (fileno(currentask->t_stdout), "    ", show_redefs);
	else {
	    pushfparams (pfp->pf_pp);
	    while (n--) {
		opcast (OT_STRING);
		onam = popop();
		if (sscanf (onam.o_val.v_s, "$%d", &scantemp) == 1)
		    cl_error (E_UERR, "reset must use name=value pairs");
		opcast (OT_STRING);
		oval = popop();
		c_envreset (onam.o_val.v_s, oval.o_val.v_s);
		pr_envset (0, onam.o_val.v_s, oval.o_val.v_s);
		if (strcmp ("erract", onam.o_val.v_s) == 0)
		    erract_init();
	    }

	    /* Prevent envfree in poptask when SET terminates from discarding
	     * this definition!!
	     */
	    c_envmark (&prevtask->t_envp);
	}
}


/* show [name]
 * if (no arguments)
 *   give a list of existing enviroment settings, but do not show redefinitions
 *   as 'set' does.
 * else
 *   show value of specified environment variable(s).
 */
#define  SZ_VALUE  SZ_COMMAND

void
clshow (void)
{
	register struct pfile *pfp;
	struct	operand onam;
	int	n, show_redefs=NO;
	char	val[SZ_VALUE];

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) == 0)
	    c_envlist (fileno(currentask->t_stdout), "    ", show_redefs);
	else {
	    pushbparams (pfp->pf_pp); /* push so first popped is first param */
	    while (n--) {
		popop();			/* discard the $n */
		opcast (OT_STRING);
		onam = popop();
  	        if (c_envfind (onam.o_val.v_s, val, SZ_VALUE) < 0)
		    cl_error (E_UERR, "No such environment variable");
		else
		    oprintf ("%s\n", val);
	    }
	}
}


/* STTY -- Set terminal driver options.  This is merely an interface to the VOS
 * sttyco() procedure, which does all the work.  Our function is merely to
 * collect the arguments into a long string and then call sttyco() to perform
 * the operation.  The dictionary must be "kept" after the call to sttyco since
 * new values of the terminal, ttyncols, and ttynlines variables may be set.
 */
void
clstty (void)
{
	register struct	pfile *pfp;
	register char	*ip, *op;
	char	sttycmd[2048], args[1024], *argp[100];
	int	argc, i;
	XINT	std_in = STDIN, std_out = STDOUT;


	pfp = newtask->t_pfp;

	/* Construct an array of pointers to the argument strings.  argp[1] is
	 * the first argument; argp[0] is the task name.
	 */
	argc = mkarglist (pfp, args, argp);

	/* Concatenate the stty argument list.  */
	for (op=sttycmd, i=1;  i <= argc;  i++) {
	    for (ip=argp[i];  (*op = *ip++);  op++)
		;
	    if (i < argc)
		*op++ = ' ';
	}
	*op++ = EOS;

	/* Call STTYCO to set the terminal driver options. */
	c_sttyco (sttycmd, std_in, std_out, fileno(newtask->t_stdout));
	keep (prevtask);
}


/* TASK [lname1, lname2, ...,] lnamen = pname
 * Define the one or more logical tasks to be in the given physical file name.
 * The new task defn's will built starting at topd, which has already been
 * reset to what it was before the call to this built started.  Thus, the
 * params pointed to by t_pfp will be overwritten and they must be saved.
 * Also, we need to "keep" the new topd so restor doesn't lob off the new
 * structures when going back to the previous task.  See the disclaimer with
 * clpack().
 *
 * Task names which begin with underscore are invisible to the user and
 * are not shown in menus.  The LT_INVIS flag is set by "addltask" if the
 * first char in the task name is an underscore.
 */
void
cltask (int redef)
{
	register struct pfile *pfp;
	struct	operand o;
	int	n, scantmp;
	char	*physname, *logname;
	int	foreign_task, flags;


	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0)
	    cl_error (E_UERR, e_geonearg, "task");

	pushfparams (pfp->pf_pp);    /* push so first popped is last param  */
	o = popop();
	logname = o.o_val.v_s;
	if (sscanf (logname, "$%d", &scantmp) == 1)
	    cl_error (E_UERR,
		"physical task name must be explicit in last arg");

	opcast (OT_STRING);
	o = popop();
	physname = o.o_val.v_s;

	/* Check for a foreign (host system) task, a type of builtin.
	 */
	if ((foreign_task = (*physname == '$'))) {
	    if (strcmp (physname, "$foreign") == 0)
		physname = "";
	    else
		physname++;
	}

	if (foreign_task) {
	    flags = LT_FOREIGN;
	    if (logname[0] == '$') {
		logname++;
		flags &= ~LT_PFILE;
	    }
	    newbuiltin (curpack, logname, clforeign, flags, physname, redef);
	} else
	    addltask (curpack, physname, logname, redef);

	while (--n) {
	    popop();			/* discard $n param name	*/
	    opcast (OT_STRING);
	    o = popop();		/* get logical name		*/
	    logname = o.o_val.v_s;

	    if (foreign_task) {
		flags = LT_FOREIGN;
		if (logname[0] == '$') {
		    logname++;
		    flags &= ~LT_PFILE;
		}
		newbuiltin (curpack, logname,clforeign,flags,physname, redef);
	    } else
		addltask (curpack, physname, o.o_val.v_s, redef);
	}

	keep (prevtask);		/* retain changes for prev task	*/
}

/* these are hooks to cltask that just select whether redefs are to be
 * permitted. they are both used as described for cltask().
 */
void
clrtask (void)
{
	cltask (YES);
}

void
clntask (void)
{
	cltask (NO);
}


/* CLFOREIGN -- Execute a foreign task.  A foreign task is a special type of
 * builtin task to the CL.  All foreign tasks vector to CLFOREIGN for
 * execution.  Our function is to build up a command line for the foreign
 * task and submit it to the host system for execution with c_oscmd().
 * The parameters to a foreign task are output as blank separated strings
 * in pfile order.  The name of the foreign task defaults to the same as the
 * name of the ltask.  Commonly foreign tasks have no pfile, hence the
 * parameters are whatever the user entered on the command line.  Note however
 * that a parameter string may be the result of any CL expression; the argument
 * list of a foreign task is parsed by the CL like it is for any task.
 * CL metacharacters must be quoted or escaped to be included as strings in
 * the command line to the host system.  I/O redirection is supported.
 *
 * A foreign task command line is built up by argument substitution, scanning
 * the so-called `ftprefix' command template string for symbolic argument
 * references of the form $1, $2, etc., to match individual arguments, or $*
 * to match the full argument list.  $(N) denotes the host equivalent of
 * virtual filename argument N.  If no $arg references are found the argument
 * list is simply appended to the ftprefix string, in which case it really is
 * a prefix string.
 */
void
clforeign (void)
{
	register struct	pfile *pfp;
	register char	*ip, *op;
	char	oscmd[1024], args[1024], *argp[100], *ap;
	int	dolseen, mapfname;
	int	argc, n1, n2, ch, n;

	pfp = newtask->t_pfp;

	/* Construct an array of pointers to the argument strings.  argp[1] is
	 * the first argument; argp[0] is the task name.
	 */
	argc = mkarglist (pfp, args, argp);

	/* Build up the host command by inserting the CL command line arguments
	 * into the command template given in the foreign task declaration.
	 */
	dolseen = 0;
	for (ip=newtask->t_ltp->lt_ftprefix, op=oscmd; (*op = *ip); op++,ip++) {
	    if (*ip == '\\' && *(ip+1) == '$')
		*op = *(++ip);
	    else if (*ip == '$') {
		dolseen++;
		ch = *(++ip);

		/* A $(N) or $(*) causes the argument strings to be treated as
		 * virtual filenames and mapped into their host equivalents for
		 * use in the host command string.
		 */
		mapfname = 0;
		if (ch == '(') {
		    mapfname++;
		    ch = *(++ip);
		    ip++;
		}

		if (isdigit (ch)) {
		    n1 = n2 = ch - '0';
		} else if (ch == '*') {
		    n1 = 1;
		    n2 = argc;
		} else {
		    *(++op) = ch;
		    continue;
		}

		for (n=n1; n <= n2;  n++) { 
		    char    osfn[SZ_PATHNAME+1];

		    if (n >= 0 && n <= argc) {
			if (n > n1)
			    *op++ = ' ';
			if (mapfname) {
			    c_fmapfn (argp[n], osfn, SZ_PATHNAME);
			    ap = osfn;
			} else
			    ap = argp[n];
			while ( (*op = *ap++) )
			    op++;
		    }
		}

		op--;
	    }
	}

	/* If there were no $arg references in the command template, append
	 * the argument list to the prefix string.
	 */
	if (!dolseen)
	    for (n=1;  n <= argc;  n++) {
		*op++ = ' ';
		for (ap=argp[n];  (*op = *ap++);  op++)
		    ;
	    }

	if (cltrace) {
	    d_fmtmsg (stderr, "\t    ", oscmd, 80 - 13);
	    eprintf ("\t--------------------------------\n");
	}

	/* Call the host system to execute the command.  If i/o redirection
	 * was indicated on the command line pointers to the names of the
	 * referenced files will have been stored in the task structure by
	 * the CL metacode instructions o_redir, o_redirall, etc.  If the
	 * task was called by a parent whose output was redirected then we
	 * must call clsystem, which will spool the output of the OS cmd
	 * in temporary files and then copy it to the parent's output streams.
	 */
	if ((newtask->t_stdout != stdout && newtask->ft_out == NULL) ||
	    (newtask->t_stderr != stderr && newtask->ft_err == NULL)) {

	    clsystem (oscmd, newtask->t_stdout, newtask->t_stderr);

	} else {
	    /* Parents i/o is not redirected, hence we can redirect i/o
	     * directly without a temp file.
	     */
	    char *in, *out, *err;
	    int append_all;

	    in  = newtask->ft_in  ? newtask->ft_in  : "",
	    out = newtask->ft_out ? newtask->ft_out : "",
	    err = newtask->ft_err ? newtask->ft_err : "";
	    append_all = (out == err);

	    if (newtask->t_flags & T_APPEND) {
		register int ch;
		register FILE *fp=NULL, *outfp=NULL;
		char tmpfile[SZ_PATHNAME];

		/* Execute the command spooling the output in a temporary
		 * file (OSCMD cannot directly append to an output file).
		 */
		if (!c_mktemp ("tmp$ft", tmpfile, SZ_PATHNAME))
		    strcpy (tmpfile, "tmp$ft.out");
		c_oscmd (oscmd, in, tmpfile, append_all ? tmpfile : err);

		/* Append the spooled output to the user-specified output
		 * redirection file.
		 */
		if ((fp = fopen (tmpfile, "r")) != NULL &&
			(outfp = fopen (out, "a")) != NULL) {
		    while ((ch = fgetc(fp)) != EOF)
			fputc (ch, outfp);
		}

		if (fp)
		    fclose (fp);
		if (outfp)
		    fclose (outfp);
		c_delete (tmpfile);

	    } else
		c_oscmd (oscmd, in, out, err);
	}
}


/* UNLEARN (ltask|package) [, (ltask|package)...]
 * Restore the package default parameters for each ltask, or for all of
 * the ltasks in the named package.
 */
void
clunlearn (void)
{
	static	char errfmt[] = "Warning: Cannot unlearn params for `%s'\n";
	register struct pfile *pfp;
	register struct ltask *ltp, *ltt;
	char	*x1, *pk, *t, *x2;
	struct	operand o;
	int	n;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) < 1)
	    cl_error (E_UERR, e_geonearg, "unlearn");

	pushbparams (pfp->pf_pp);
	while (n--) {
	    popop();			/* discard fake name.		*/
	    opcast (OT_STRING);
	    o = popop();		/* get ltask|package name	*/
	    breakout (o.o_val.v_s, &x1, &pk, &t, &x2);
	    if (!(ltp = cmdsrch (pk, t)))
		continue;

	    /* If package, unlearn each task. */
	    if (ltp->lt_flags & LT_PACCL) {
		/* Unlearn each task in the package. */
		for (ltt=ltp->lt_pkp->pk_ltp;  ltt != NULL;  ltt=ltt->lt_nlt)
		    if (pfileinit (ltt) == ERR)
			eprintf (errfmt, ltt->lt_lname);

		/* Unlearn the package parameters. */
		if ( (ltt = ltasksrch (pk, t)) ) {
		    if (pfileinit(ltt) == ERR)
			eprintf (errfmt, ltt->lt_lname);
		}

	    } else if (pfileinit (ltp) == ERR)
		eprintf (errfmt, ltp->lt_lname);
	}
}


/* UPDATE ltask [, ltask...]
 * force the in-core pfile for the given tasks to be written out.
 * used when the pfile has been pre-loaded with cache but it is to be
 * saved before it would automatically be due to bye'ing task.
 * since the given task might be running, if we were run from it for example,
 * we also force the working copy to get copied back to its original.
 * (the check that it is indeed a copy is in pfcopyback()).
 */
void
clupdate (void)
{
	/* x1 and x2 are just place holders for the call to breakout.
	 */
	register struct pfile *pfp;
	register struct ltask *ltp;
	char	*x1, *pk, *t, *x2;
	struct	operand o;
	int	n;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) < 1)
	    cl_error (E_UERR, e_geonearg, "update");

	pushbparams (pfp->pf_pp);
	while (n--) {
	    popop();			/* discard fake name.		*/
	    opcast (OT_STRING);
	    o = popop();		/* get ltask			*/
	    breakout (o.o_val.v_s, &x1, &pk, &t, &x2);
	    ltp = ltasksrch (pk, t);
	    if (!(ltp->lt_flags & LT_PFILE))
		cl_error (E_UERR, e_nopfile, ltp->lt_lname);
	    if ((pfp = pfilefind (ltp)) == NULL)
		cl_error (E_UERR, "pfile not loaded for `%s'",
		    ltp->lt_lname);
	    pfcopyback (pfp);		/* IT checks whether pfp is a copy */
	    pfileupdate (pfp);
	}
}

/* HIDETASK ltask [, ltask...]
 * Set the flags for this task to LT_INVIS so that it does not
 * become an active part of the users environment.  This function does
 * not require the underscore to hide the task.
 */
void
clhidetask (void)
{
	/* x1 and x2 are just place holders for the call to breakout.
	 */
	register struct pfile *pfp;
	register struct ltask *ltp;
	char	*x1, *pk, *t, *x2;
	struct	operand o;
	int	n;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) < 1)
	    cl_error (E_UERR, e_geonearg, "hidetask");

	pushbparams (pfp->pf_pp);
	while (n--) {
	    popop();			/* discard fake name.		*/
	    opcast (OT_STRING);
	    o = popop();		/* get ltask			*/
	    breakout (o.o_val.v_s, &x1, &pk, &t, &x2);
	    ltp = ltasksrch (pk, t);
	    ltp->lt_flags |= LT_INVIS;
	}
}


/* WAIT -- Wait for a job or jobs to terminate.  The default is to wait for
 * all jobs.
 */
void
clwait (void)
{
	register struct pfile *pfp;
	register int n, jn;
	struct operand o;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0)
	    jn = 0;

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */

	if (n > 0) {
	    while (n--) {
		popop();		/* discard the $n name		*/
	 	opcast (OT_INT);	/* insure we get an integer	*/
		o = popop();		/* pop job number, as int	*/
		jn = o.o_val.v_i;

		bkg_wait (jn);
	    }
	} else
	    bkg_wait (jn);
}


/* JOBS -- Show status of a job or jobs.  The default is to show the status
 * of all jobs running or that have recently run.
 */
void
cljobs (void)
{
	register struct pfile *pfp;
	register int n, jn;
	struct operand o;

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) <= 0) {
	    bkg_jobstatus (currentask->t_stdout, 0);
	    return;
	}

	pushbparams (pfp->pf_pp);    /* push so first popped is first param */
	while (--n >= 0) {
	    popop();			/* discard the $n name		*/
	    opcast (OT_INT);		/* insure we get an integer	*/
	    o = popop();		/* pop job number, as int	*/
	    jn = o.o_val.v_i;

	    bkg_jobstatus (currentask->t_stdout, jn);
	}
}


/* CLFUNC -- Called when one of the dummy intrinsic functions entered in
 * the language package (to prompt the user) is called as a task.
 */
void
clfunc (void)
{
	cl_error (E_UERR, "Function `%s' cannot be called as a task",
	    currentask->t_ltp->lt_lname);
}


/* BEEP -- Beep the terminal.
 */
void
clbeep (void)
{
	putchar ('\007');
}


/* TIME -- Print the current time and date on the standard output.
 */
void
cltime (void)
{
	char	buf[SZ_LINE];

	c_cnvtime (c_clktime(0L), buf, SZ_LINE);
	oprintf ("%s\n", buf);
}


/* CLEAR -- Clear the terminal screen and home the cursor.  Uses the TTY
 *  package (device independent terminal interface), which requires an entry
 *  in the dev$termcap file for the terminal.  In addition to clearing the
 *  screen, we also turn standout mode and raw mode off, just in case.
 */
void
clclear (void)
{
	XINT	tty, sout = STDOUT;

	if ((tty = c_ttyodes ("terminal")) == ERR)
	    c_erract (EA_ERROR);

	c_ttyso (sout, tty, NO);
	c_ttyclear (sout, tty);
	c_ttycdes (tty);
	c_fseti (sout, F_RAW, NO);
}


/* SLEEP -- Suspend execution for the specified number of seconds.
 */
void
clsleep (void)
{
	register struct pfile *pfp;
	struct	operand o;

	pfp = newtask->t_pfp;
	pushbparams (pfp->pf_pp);	/* push sofirst popped is first param */
	if ( nargs (pfp) <= 0)
	    return;
	else {
	    popop();			/* discard the $n name	*/
	    opcast (OT_INT);
	    o = popop();		/* get the number of seconds */
	    c_tsleep (o.o_val.v_i);
	}
}


/* EDIT -- Call up a host system editor to edit a file.  The name of the editor
 * to be used is defined in the IRAF environment.  The command to be sent to
 * the host system to run the editor is defined by an SPRINTF style format
 * string in the EDCAP editor database.  The SPRINTF format is assumed to
 * contain exactly one %s sequence to be replaced by the name of the file(s)
 * to be edited.  If no %s sequence is present in the EDCAP entry, the
 * host_editor() function will add one at the end so that the filenames are
 * concatenated to the string in the EDCAP entry.
 */
void
cledit (void)
{
	register struct pfile *pfp;
	char	oscmd[SZ_LINE], os_filelist[SZ_LINE];
	char	osfn[SZ_PATHNAME];
	struct	operand o;
	char	*envget();
	int	n;

	pfp = newtask->t_pfp;

	if ((n = nargs(pfp)) > 0) {
	    pushbparams (pfp->pf_pp);

	    /* Process the argument list into a list of files to be edited.
	     */
	    os_filelist[0] = EOS;
	    while (--n >= 0) {
		popop();		/* discard the $1 	*/
		o = popop();
		c_fmapfn (o.o_val.v_s, osfn, SZ_PATHNAME);
		if (os_filelist[0] != EOS)
		    strcat (os_filelist, " ");
		strcat (os_filelist, osfn);
	    }
	}

	/* Format the host editor command, and call the host system editor
	 * to edit the file(s).
	 */
	sprintf (oscmd, host_editor (envget ("editor")), os_filelist);
	c_oscmd (oscmd, "", "", "");
}


/* _ALLOCATE -- Allocate a device.  The parent process (i.e. the CL) allocates
 * (or mounts, depending on the system) the device, rendering it ready for
 * exclusive i/o by any subprocesses.  (Called from the allocate.cl and
 * deallocate.cl scripts in the SYSTEM pkg.)
 */
void
clallocate (void)
{
	register struct pfile *pfp;
	register int	n;
	static char	noalloc[] = "cannot allocate device %s";
	struct	operand o;
	char	device[SZ_FNAME+1];
	char	owner[SZ_FNAME+1];

	pfp = newtask->t_pfp;
	if ((n = nargs (pfp)) == 0)
	    return;

	pushbparams (pfp->pf_pp);
	popop();			/* throw $1 away	*/
	opcast (OT_STRING);		/* param 1 == device	*/
	o = popop();
	strcpy (device, o.o_val.v_s);

	/* Verify that the device can be allocated.
	 */
	switch (c_devowner (device, owner, SZ_FNAME)) {
	case DV_DEVFREE:
	    break;			/* ok to allocate	*/
	case DV_DEVALLOC:
	    eprintf ("device %s is already allocated\n", device);
	    return;			/* already allocated	*/
	case DV_DEVINUSE:
	    cl_error (E_UERR, "device %s is already allocated to %s\n",
		device, owner);
	default:
	    cl_error (E_UERR, noalloc, device);
	}

	/* Allocate the device. */
	if (c_allocate (device) == ERR)
	    cl_error (E_UERR, noalloc, device);

	/* Keep count and save names of allocated devices.
	 */
	for (n=0;  n < MAX_ALLOCDEV;  n++) {
	    if (!allocdev[n].allocated)
		continue;
	    if (strcmp (allocdev[n].devname, device) == 0)
		return;	/* device already in table */
	}

	/* Find empty slot */
	for (n=0;  n < MAX_ALLOCDEV && allocdev[n].allocated;  n++)
	    ;
	if (n >= MAX_ALLOCDEV)
	    cl_error (E_UERR, "too many allocated devices");

	/* Save name of device */
	strncpy (allocdev[n].devname, device, SZ_DEVNAME);
	allocdev[n].devname[SZ_DEVNAME] = EOS;
	allocdev[n].allocated = 1;
	nallocdev++;
}


/* _DEALLOCATE -- Deallocate a device.
 */
void
cldeallocate (void)
{
	register struct pfile *pfp;
	register int	n;
	static char	nodealloc[] = "cannot deallocate device %s";
	struct	operand o;
	char	device[SZ_FNAME+1];
	char	owner[SZ_FNAME+1];
	int	rewind=0, n_args;

	pfp = newtask->t_pfp;
	if ((n_args = nargs (pfp)) <= 0)
	    return;

	pushbparams (pfp->pf_pp); 	/* params in correct order	*/
	popop();			/* throw $1 away		*/
	opcast (OT_STRING);		/* param 1 == device name	*/
	o = popop();
	strcpy (device, o.o_val.v_s);

	if (n_args > 1) {
	    popop();			/* throw $2 away		*/
	    opcast (OT_BOOL);		/* param 2 == rewind flag	*/
	    o = popop();
	    rewind = o.o_val.v_i;
	}

	/* Verify that the device can be deallocated.
	 */
	switch (c_devowner (device, owner, SZ_FNAME)) {
	case DV_DEVFREE:
	    eprintf ("device %s is not allocated\n", device);
	    return;
	case DV_DEVALLOC:
	    break;			/* ok to deallocate	*/
	case DV_DEVINUSE:
	    cl_error (E_UERR, "device %s is currently allocated to %s\n",
		device, owner);
	default:
	    cl_error (E_UERR, nodealloc, device);
	}

	/* Deallocate the device. */
	if (c_deallocate (device, rewind) == ERR)
	    cl_error (E_UERR, nodealloc, device);

	/* Keep count and save names of allocated devices.
	 */
	for (n=0;  n < MAX_ALLOCDEV;  n++) {
	    if (!allocdev[n].allocated)
		continue;
	    if (strcmp (allocdev[n].devname, device) == 0) {
		allocdev[n].allocated = 0;
		--nallocdev;
		break;
	    }
	}
}


/* _DEVSTATUS -- Print the status of an allocatable device on the standard
 * output.
 */
void
cldevstatus (void)
{
	register struct pfile *pfp;
	struct	operand o;
	char	device[SZ_FNAME+1];

	pfp = newtask->t_pfp;
	if (nargs (pfp) <= 0)
	    return;

	pushbparams (pfp->pf_pp); 	/* params in correct order	*/
	popop();			/* throw $1 away		*/
	opcast (OT_STRING);		/* param 1 == device name	*/
	o = popop();
	strcpy (device, o.o_val.v_s);

	/* Print the device status.  */
	c_devstatus (device, STDOUT);
}


/* XERROR -- Runtime error recovery opcode procedures.
 */

void 
clerrpsh (void)
{
        extern int do_error;
	extern ErrCom errcom;

        do_error = NO;
	errcom.nhandlers++;
	errcom.errflag = OK;
	if (cldebug)
	    eprintf ("in cl_errpush: do_error = %d\n", do_error);
}


void 
clerreset (void)
{
	extern ErrCom errcom;

        do_error = NO;
	errcom.errflag = OK;
	errcom.nhandlers = 0;
	if (cldebug)
	    eprintf ("in cl_erreset: do_error = %d\n", do_error);
}


/* CLONERROR -- Post an error handler to be called each time an error
 * is found.  
 *
 * NOTE: The handler posting is implemented but the call from the error
 * recovery itself is not due to complexities in establishing the context.
 * We will leave this in for now but it is effectively a no-op  (5/25/05)
 *
 */
void 
clonerror (void)
{
	struct	operand o;
	struct  pfile *pfp = newtask->t_pfp;
	struct  ltask *ltp;
	struct	package	*pkp;
	static  char handler[SZ_FNAME+1];
	extern  char *onerr_handler;


	handler[0] = NULL;
	pushbparams (pfp->pf_pp);
	if (nargs(pfp) > 0) {
	    popop();			/* discard the $n name		*/
	    o = popop();		/* get the handler arg name	*/
	    strcpy (handler, o.o_val.v_s);

	    /* Search all packages for the named task. _ltasksrch() does not 
	     * call error() and so can be used to check cleanly whether the
	     * task exists.
             */
	    if (handler[0] && (ltp = _ltasksrch ("", handler, &pkp)) == NULL) {
	        onerr_handler = NULL;
		cl_error (E_UERR, "onerr handler '%s' not found", handler);

	    } else {
	        onerr_handler = (handler[0] ? handler : NULL);

	        if (cldebug) {
	          if (onerr_handler)
		    eprintf ("onerror: posting handler '%s'\n", onerr_handler);
	          else
		    eprintf ("onerror: clearing onerr handler\n");
	        }
	    }
	}
}



/* ----------------------------------------------
 * End of builtin functions.
 * What follows is their support code.
 */

/* SETBUILTINS -- Add the builtin functions to package at pkp (this should
 * always just be clpackage).  To add more functions, write the support function
 * and enter it into the builtin table, btbl.  Reverse alpha due to lifo nature
 * of list.  Aliases can be made easily with multiple b_names using the same
 * b_f.  Setting LT_INVIS will keep it from being seen in the menu.
 */
void
setbuiltins (register struct package *pkp)
{
	/* Debugging functions are in debug.c.
	 */
	extern void d_f(), d_l(), d_d(), d_off(), d_on(), d_p(), d_t();
	extern void d_asmark(), d_assemble(), d_prof(), d_trace();
	extern void pr_listcache();

	static struct builtin {
		char	*b_name;
		void	(*b_f)();
		int	b_flags;
	} btbl[] = {
	  { "d_asmark", d_asmark, LT_INVIS},	/* mark assembler stack pos  */
	  { "d_assemble", d_assemble, LT_INVIS},/* "assemble" a CL script    */
	  { "d_f", d_f, LT_INVIS},	/* shows available file descr	     */
	  { "d_l", d_l, LT_INVIS},	/* shows defined ltasks		     */
	  { "d_m", d_d, LT_INVIS},	/* shows memory usage		     */
	  { "d_off", d_off, LT_INVIS},	/* disables debugging msgs	     */
	  { "d_on", d_on, LT_INVIS},	/* enables debugging msgs	     */
	  { "d_trace",dotrace,LT_INVIS},/* instruction tracing toggle	     */
	  { "d_p", d_p, LT_INVIS},	/* shows loaded param files	     */
	  { "d_prof", d_prof, LT_INVIS},/* script execution profiling	     */
	  { "d_t", d_t, LT_INVIS},	/* shows running tasks		     */
	  { "prcache", clprcache, 0},	/* show process cache		     */
	  { "?", clhelp, LT_INVIS},	/* tasks in current package	     */
	  { "??", clallhelp, LT_INVIS},	/* all tasks in all packs	     */
	  { "wait", clwait, 0},		/* wait for all bkg jobs	     */
	  { "jobs", cljobs, 0},		/* show status of bkg jobs	     */
	  { "unlearn", clunlearn, 0},	/* unlearn params 		     */
	  { "update", clupdate, 0},	/* write out a changed pfile	     */
	  { "hidetask",clhidetask, 0},  /* make these tasks invisible        */
	  { "task", clntask, 0},	/* define new ltask/ptask	     */
	  { "set", clset, 0},		/* make environ table entry	     */
	  { "reset", clreset, 0},	/* reset value of envvar	     */
	  { "show", clshow, 0},		/* show value of environ var	     */
	  { "stty", clstty, 0},		/* set terminal driver options	     */
	  { "redefine", clrtask, 0},	/* redfine ltasl/ptask		     */
	  { "package", clpack, 0},	/* define new package		     */
	  { "_curpack", clcurpack,
		LT_INVIS},		/* name the current package	     */
	  { "print", clprint, 0},	/* formatted output to stdout	     */
	  { "printf", clprintf, 0},	/* formatted output to stdout	     */
	  { "fprint", clfprint, 0},	/* formatted output		     */
	  { "putlog", clputlog, 0}, 	/* put a message to the logfile      */
	  { "dparam", cldparam, 0},	/* dump params for tasks	     */
	  { "lparam", cllparam, 0},	/* list params for tasks	     */
	  { "eparam", cleparam, 0},	/* edit params for tasks	     */
	  { "ehistory", clehistory, 0},	/* edit command history		     */
	  { "history", clhistory, 0},	/* print command history	     */
	  { "service", clservice, 0},	/* respond to bkg query		     */
	  { "kill", clkill, 0},		/* kill a background job	     */
	  { "keep", clkeep, 0},		/* keep new defn's after bye	     */
	  { "error", clerror, 0},	/* error msg from child		     */
	  { ROOTPACKAGE, lapkg,
	    	LT_INVIS|LT_DEFPCK},	/* fake task for language.	     */
	  { CLPACKAGE, clpkg,
		LT_INVIS|LT_DEFPCK},	/* fake task for clpackage.	*/
	  { "chdir", clchdir, 0},	/* change directory		*/
	  { "cd",    clchdir, 0},	/* change directory		*/
	  { "back",  clback, 0},	/* change to previous directory	*/
	  { "flprcache", clflprcache, 0},/* flush the process cache	*/
	  { "gflush", clgflush, 0},	/* flush graphics output	*/
	  { "cache", clcache, 0},	/* pre-load a tasks pfile	*/
	  { "which", clwhich, 0},	/* locate named task		*/
	  { "whereis", clwhereis, 0},	/* locate all instances of task	*/
	  { "clbye", clclbye, LT_CL|LT_CLEOF},	/* cl() with EOF	*/
	  { "bye", clbye, 0},		/* restore previous state	*/
	  { "logout", cllogout, 0},	/* log out of the CL		*/

	  { "scan", clscans, 0},	/* scan from a pipe		*/
	  { "scanf", clscanf, 0},	/* formatted scan 		*/
	  { "fscan", clfunc, 0},	/* intrinsic function entries	*/
	  { "defpac", clfunc, 0},	/* 		"		*/
	  { "defpar", clfunc, 0},	/* 		"		*/
	  { "defvar", clfunc, 0},	/* 		"		*/
	  { "deftask", clfunc, 0},	/* 		"		*/
	  { "access", clfunc, 0},	/* 		"		*/
	  { "imaccess", clfunc, 0},	/* 		"		*/
	  { "mktemp", clfunc, 0},	/* 		"		*/
	  { "envget", clfunc, 0},	/* 		"		*/
	  { "radix",  clfunc, 0},	/* 		"		*/
	  { "osfn",   clfunc, 0},	/* 		"		*/
	  { "beep", clbeep, 0},		/* beep the terminal		*/
	  { "time", cltime, 0},		/* show the current time	*/
	  { "clear", clclear, 0},	/* clear the terminal screen	*/
	  { "edit", cledit, 0},		/* edit a file or files	        */
	  { "sleep", clsleep, 0},	/* suspend process execution	*/
	  { "_allocate", clallocate, LT_INVIS},
	  { "_deallocate", cldeallocate, LT_INVIS},
	  { "_devstatus", cldevstatus, LT_INVIS},
	  { "_errpsh", clerrpsh, LT_INVIS},	/* push error handler	     */
	  { "_erreset", clerreset, LT_INVIS},	/* reset error handler	     */
	  { "onerror", clonerror, 0},		/* post user error handler   */
	};

	register struct builtin *bp;

	for (bp = btbl; bp < &btbl[sizeof(btbl)/sizeof(struct builtin)]; bp++)
	    newbuiltin (pkp, bp->b_name, bp->b_f, bp->b_flags, "", 0);
}


/* NEWBUILTIN -- Make a new ltask off pkp that will serve as a cl directive
 * builtin function.  Link in exactly the same fashion as newltask() but use
 * lt_f rather than lt_pname.  See paramsrch().  FP is a pointer to the function
 * that will perform the directive.  Flags is to be or'd in with lt_flags in
 * the new ltask.  Call error if no more core.
 */
void
newbuiltin (
  struct package *pkp,		/* package which owns task	*/
  char	*lname,			/* ltask name			*/
  void	(*fp)(void),		/* pointer to builtin fcn	*/
  int	flags,			/* task flags			*/
  char	*ftprefix,		/* OSCMD prefix if foreign	*/
  int	redef			/* permit redefinitions		*/
)
{
	register struct ltask *newltp;

	newltp = addltask (pkp, NULL, lname, redef);

	/* If no OSCMD prefix string is given use the logical task name,
	 * which must therefore be the same as the host task name.
	 */
	if (*ftprefix)
	    newltp->lt_ftprefix = comdstr (ftprefix);
	else
	    newltp->lt_ftprefix = newltp->lt_lname;

	newltp->lt_f = fp;
	newltp->lt_flags = (flags | LT_BUILTIN);
}


/* MKARGLIST -- Reconstruct the argument list of a task as an array of arg
 * pointers to arg strings of the form "expr" or "keyword=value".  Upon
 * output, argp[0] contains the task name and the function value is the
 * number of arguments, excluding argp[0].
 */
int
mkarglist (
    register struct pfile *pfp,	/* pfile pointer		*/
    char *args,			/* string buffer for arg chars	*/
    char *argp[]		/* array of arg pointers	*/
)
{
	register char	*ip, *op;
	struct	operand o_v, o_n;
	int	argc, n;


	/* Construct an array of pointers to the argument strings.  argp[1] is
	 * the first argument; argp[0] is the task name.
	 */
	if ((argc = nargs(pfp)) > 0) {
	    pushbparams (pfp->pf_pp);
	    op = args;

	    argp[0] = newtask->t_ltp->lt_lname;

	    for (n=1;  n <= argc;  n++) {
		argp[n] = op;

		/* Get the parameter name.  If this is $N then we have a
		 * positional argument, otherwise we have a keyword=value
		 * argument, and the arg should be encoded in that form.
		 */
		o_n = popop();
		ip = o_n.o_val.v_s;
		if (*ip != '$') {
		    while ((*op = *ip++))
			op++;
		    *op++ = '=';
		}

		/* Get the parameter value. */
		opcast (OT_STRING);
		o_v = popop();
		ip = opindef(&o_v) ? "INDEF" : o_v.o_val.v_s;
		while (ip && (*op++ = *ip++))
		    ;
	    }

	    argp[n] = NULL;
	}

	return (argc);
}


/* PUSHFPARAMS -- Push the parameter list starting with pp forwards, that is,
 * push the pp first and work towards the last parameter.  Push two operands
 * per parameter: first the value, then the name.  Used when the parameters for
 * a builtin will be accessed right-to-left.
 */
void
pushfparams (register struct param *pp)
{
	struct operand onam;

	onam.o_type = OT_STRING;
	for (;  pp;  pp = pp->p_np) {
	    paramget (pp, 'V');
	    onam.o_val.v_s = pp->p_name;
	    pushop (&onam);
	}
}


/* PUSHBPARAMS -- Push the parameter list starting with pp backwards, that is,
 * push the last param in the list first and work back up to pp.  Push two
 * operands per parameter: first the value, then the name.  Used when the
 * parameters for a builtin will be accessed left-to-right.
 */
void
pushbparams (struct param *pp)
{
	struct operand onam;
	struct param *npp;

	if (pp == NULL)
	    return;		/* just a guard	*/
	npp = pp->p_np;
	if (npp != NULL)
	    pushbparams (npp);

	paramget (pp, 'V');
	onam.o_type = OT_STRING;
	onam.o_val.v_s = pp->p_name;
	pushop (&onam);
}


/* PUSHBPVALS -- Like pushbparams, but only the parameter value is pushed.
 */
void
pushbpvals (struct param *pp)
{
	struct param *npp;

	if (pp == NULL)
	    return;		/* just a guard	*/
	npp = pp->p_np;
	if (npp != NULL)
	    pushbpvals (npp);

	paramget (pp, 'V');
}


/* NARGS -- Count the number of parameters in a parameter list, and hence
 * the number of command line arguments to a builtin.
 */
int
nargs (struct pfile *pfp)
{
	struct	param	*pp;
	int	n;

	for (pp=pfp->pf_pp, n=0;  pp != NULL;  pp=pp->p_np)
	    n++;

	return (n);
}


/* KEEP -- Preserve additions to the dictionary and environment when the
 * referenced task terminates.
 */
void
keep (register struct task *tp)
{
	if (cldebug) {
	    eprintf ("currentask: %d, prevtask: %d\n",currentask,prevtask);
	    eprintf ("keep(): tp: %d\n",tp);
	}
	tp->t_topd = topd;
	c_envmark (&tp->t_envp);
}
