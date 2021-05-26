/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_xwhen
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "mem.h"
#include "opcodes.h"
#include "operand.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "grammar.h"
#include "proto.h"


/*
 * EXEC -- Functions that prepare tasks for running, the actual runtime
 * interpreter, and functions involved in wrapping up when a task dies.
 */

extern int cldebug;
extern int cltrace;

#define	SZ_STARTUPMSG	4000	/* cmd sent to subprocess to run task	*/
#define	BINDIR		"bin$"	/* where installed executables go	*/	

extern	FILE *yyin;		/* yyparse's input			*/
extern	int alldone;		/* set when oneof pops firstask		*/
extern	int yeof;		/* parser saw EOF			*/
extern	int gologout;		/* user typed logout()			*/
extern	int loggingout;		/* in the process of logging out	*/

char	*findexe();


/* RUN -- Run the code beginning at pc until we run an EXEC instruction of
 *   something other than a builtin command or END instruction.
 *   The EXEC instruction means that a new task is being started and we should
 *   return to the parser in the main "parse/run" loop in main.  If, however,
 *   the exec was for a builtin (or procedure, someday) then no parsing is to
 *   be done and we just continue on with the current code.
 * Note that execing the bye builtin is not a special case since it does a
 *   restor() which resets the pc to the instruction immediately following the
 *   exec IN THE PARENT task and we continue on with it.
 * Increment pc after each "fetch" cycle and before the "exec" cycle.
 *   If any if the instructions fail, they will call error(). this will do
 *   a longjmp(errenv,1), causing setjmp to return (in main) and an
 *   immediate retreat to the most recent terminaltask with unwind().
 */
void 
run (void)
{
	register struct codeentry *cp;
	register int opcode;

	if (cltrace)
	    eprintf ("\t----- task %s -----\n",
		currentask->t_ltp->lt_lname);

	do {
	    cp = coderef (pc);
	    opcode = cp->c_opcode;
	    if (cltrace)
		d_instr (stderr, "\t", pc);
	    if (cldebug)
		eprintf ("run: pc = %d, opcode = %d\n", pc, opcode);
	    pc += cp->c_length;
	    (*opcodetbl[opcode]) (&cp->c_args);

	} until ((opcode == EXEC && !(newtask->t_flags & T_BUILTIN)) ||
		  opcode == END || alldone);
}


/* CALLNEWTASK -- Called from CALL instruction to push and setup a new task
 *   structure.  If find a known ltask with given name create a new task on
 *   control stack, set up newtask and defaults for the pseudofiles.
 *   Pseudofiles may be effected by other instructions before it gets to exec.
 * Make sure we have a pfile list; either try to read it if task is 
 *   supposed to have a real one or manufacture the beginnings of one if it
 *   isn't and set PF_FAKE.  New task runs with a copy of the pfile if it
 *   wasn't fake.  Guard against making more than one copy.  Also, don't dup
 *   the cl's params to maintain the meaning of "firstask".  Things like mode,
 *   logfile and abbreviations should be global and permanent.
 * Special case for package names essentially runs a cl but with a new curpack,
 *   the only real semantic intent of "running" a package.
 *   This lets a package name given as a command appear to change the current
 *   package and yet remain interactive.  Since it really is a new task, state
 *   saving and restoring on error will work right and we also achieve an
 *   ability to have multiple package defn's in a script ltask.
 *   Any parameter references will refer to the cl's also.
 */
void
callnewtask (char *name)
{
	/* x1 and x2 are just place holders to call breakout().
	 */
	char	*x1, *pk, *t, *x2;	
	struct	ltask *ltp;
	int	flags, ltflags;

	if (cldebug)
	    eprintf ("callnewtask: name=%s, currentask=%x\n", name, currentask);

	/* Save current dictionary and stack pointers. They get restored when
	 * the new task dies normally and the current task is to continue.
	 * Save pc when get to the EXEC instruction so it continues from there.
	 */
	currentask->t_topos = topos;	/* save these two just in case 	*/
	currentask->t_basos = basos;	/* something is left on the stk	*/
	currentask->t_topcs = topcs;	/* save before adding newtask	*/
	currentask->t_topd  = topd;	/* save before adding pfile	*/
	currentask->t_curpack = curpack;/* save in case changing to a new one*/
	c_envmark (&currentask->t_envp);/* save env stack pointer	*/
	currentask->t_pno = 0;		/* set only if task defines pkg	*/

	newtask = pushtask();
	flags = 0;

	/* Search for the command to run.  A leading '$' signifies that
	 * execution is to be time but is not part of the name.  Set ltp
	 * and newtask->t_pfp depending on whether we are running a task or
	 * a package.
	 */
	if (*name == '$') {
	    flags |= T_TIMEIT;
	    name++;
	}

	breakout (name, &x1, &pk, &t, &x2);
	ltp = cmdsrch (pk, t);

	if (ltp->lt_flags & LT_CL) {
	    /* Change curpack if LT_PACCL. (cmdsrch() set lt_pkp).  Just
	     * changing packages; use cl's ltask and pfile.  Push a new cl()
	     * on the control stack, with the T_PKGCL and T_CL flags set.
	     */
	    if (ltp->lt_flags & LT_PACCL) {
		flags |= T_PKGCL;
		curpack = ltp->lt_pkp;
	    } else if (ltp->lt_flags & LT_CLEOF)
		flags |= T_CLEOF;

	    ltp = firstask->t_ltp;
	    newtask->t_pfp = firstask->t_pfp;

	    /* Initialize the lexical analyzer (necessary to recognize BOL).
	     */
	    lexinit();

	} else {
	    if (ltp->lt_flags & LT_PFILE) {
		register struct pfile *pfp;

		/* This task has a real pfile. read in if not already in
		 * core.  Copy if not already one and not just cl.
		 */
		newtask->t_pfp = NULL;
		if ((pfp = pfilefind (ltp)) == NULL)
		    pfp = pfileload (ltp);
		if (!(pfp->pf_flags & PF_COPY) && ltp != firstask->t_ltp)
		    pfp = pfilecopy (pfp);
		newtask->t_pfp = pfp;

		/* Also load any pset files associated with the main pfile.
		 * These are linked into a list with the main pfile at the
		 * head of the list, pointed to by the task descriptor.
		 */
		if (pfp->pf_flags & PF_PSETREF) {
		    register struct param *pp;
		    struct  operand o;
		    char    *pset;

		    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
			if (!(pp->p_type & PT_PSET))
			    continue;
			o = pp->p_valo;
			if (opundef(&o) || *(pset = o.o_val.v_s) == EOS)
			    pset = pp->p_name;
			pfp = pfp->pf_npset = pfilecopy (pfilesrch (pset));
			pfp->pf_psetp = pp;
		    }
		}

	    } else {
		/* This task does not have a real pfile so start a fake one.
		 */
		newtask->t_pfp = newpfile (ltp);
		newtask->t_pfp->pf_flags = PF_FAKE;
	    }
	}

	newtask->t_pfp->pf_n = 0;	/* init number of command line args */
	newtask->t_ltp = ltp;
	newtask->t_pid = -1;		/* gets set if do a real exec	*/
	newtask->t_stdin = currentask->t_stdin;	/* inherit files	*/
	newtask->t_stdout = currentask->t_stdout;
	newtask->t_stderr = currentask->t_stderr;
	newtask->t_stdgraph = currentask->t_stdgraph;
	newtask->t_stdimage = currentask->t_stdimage;
	newtask->t_stdplot = currentask->t_stdplot;

	/* Init i/o redirection for a foreign task.
	 */
	newtask->ft_in = newtask->ft_out = newtask->ft_err = NULL;

	/* Set up flags describing the kind of task we are about to run. the
	 * absence of any of these flags will imply a genuine executable task.
	 * the flags in t_flags are more of a convenience than anything since
	 * later tests could use the same tests used here.
	 */
	ltflags = ltp->lt_flags;

	if (ltflags & LT_PSET) {
	    flags = (T_SCRIPT|T_PSET);
	} else if (ltflags & LT_SCRIPT) {
	    newtask->t_scriptln = 0;
	    flags = T_SCRIPT;
	} else if (ltflags & LT_FOREIGN) {
	    flags = T_BUILTIN | T_FOREIGN;	/* a type of builtin */
	} else if (ltflags & LT_BUILTIN) {
	    flags = T_BUILTIN;
	} else if (ltflags & LT_CL) {
	    /* Or, not assign: preserve T_PKGCL and T_CLEOF flags if set. */
	    flags |= T_CL;
	}

	if (ltflags & LT_STDINB)
	    flags |= T_STDINB;
	if (ltflags & LT_STDOUTB)
	    flags |= T_STDOUTB;

	newtask->t_flags = flags;
}


/* EXECNEWTASK -- Called from the EXEC instruction after all param and stdio
 * processing for the new task is complete.  Here we actually run the new task,
 * either directly in the case of a builtin function, or as a new case for
 * main()'s loop.  Do not set newtask to NULL so that run() can tell what it
 * exec'd.
 */
void
execnewtask (void)
{
	/* VMS C V2.1 cannot handle this (see below).
	 * register struct pfile *pfp;
	 */
	static	struct pfile *pfp;

	struct	param *pp;
	FILE	*fopen();

	if (newtask == NULL)
	    /* if this ever happens, i don't want to know about it. */
	    return;

	currentask->t_pc = pc;		/* instruction after EXEC	*/

	if (cldebug)
	    eprintf ("execnewtask: pc = %d\n", pc);

	if (newtask->t_flags & T_BUILTIN) {
	    /* set yyin in case a builtin reads someday; none do now.
	     * unlink newtask's fake param file and reset top of dictionary
	     *   to what it was before the fake param file was added; it is
	     *   still there, however, for the builtin to use. this is done
	     *   since some builtins (eg task) want to add things that are
	     *   to stay on the dictionary and the tools all start at topd.
	     * the return is back to run(); it will continue since it will
	     *   see that newtask was just a builtin.
	     * note that we do not reset pf_n, as with other fake pfiles,
	     *   as this is the way builtins get their number of arguments
	     *   (it's faster than building them a $nargs).
	     */
	    yyin = newtask->t_in = currentask->t_in;	/* inherit pipe */
	    newtask->t_out = currentask->t_out;
	    newtask->t_modep = currentask->t_modep;	/* inherit mode */

	    /* VMS C 2.1 Optimizer cannot handle this.
	     * parhead = dereference (reference (pfile, parhead)->pf_npf);
	     */
	    pfp = reference (pfile, parhead);
	    parhead = dereference (pfp->pf_npf);

	    topd = currentask->t_topd;
	    currentask = newtask;
	    newtask->t_flags |= T_RUNNING;

	    if (cldebug)
		eprintf ("execnewtask: calling new task@%x\n", newtask);
	    if (cltrace)
		eprintf ("\t----- exec %s %s -----\n",
		    (newtask->t_flags & T_FOREIGN) ? "foreign" : "builtin",
		    newtask->t_ltp->lt_lname);

	    (*newtask->t_ltp->lt_f)();
	    oneof();		/* proceed as though this task saw eof	  */
	    return;
	}

	pfp = newtask->t_pfp;

	/* If the new task is a cl, we are not running in background and
	 * its t_in is stdin, it is interactive.  Note that when a package
	 * is loaded by a script task rather than interactively by the user,
	 * the t_in of the cl() in the package script task will be reading
	 * from the calling script task rather than from the original stdin
	 * (the user terminal), hence is not interactive.  If this task is
	 * flagged interactive, taskunwind() may elect to restart it on an
	 * error so save present state for restor().
	 */
	if (newtask->t_flags & T_CL) {
	    if (cldebug)
		eprintf ("execnewtask: new task is the CL\n");
	    if (cltrace)
		eprintf ("\t----- exec cl -----\n");

	    /* Call set_clio to set the command input and output streams
	     * t_in and t_out for a cl() or package_name() command.
	     */
	    set_clio (newtask);

	    /* This code is a temporary patch to allow packages to be 
	     * loaded from within scripts regardless of whether there 
	     * are enclosing brackets.  If a CL statement is executed
	     * within a script which is itself called within another
	     * script, then we will do an implicit keep before the CL.
	     */
	    if (topcs + 2*TASKSIZ <= STACKSIZ)
		if ((strcmp (newtask->t_ltp->lt_lname, "cl") == 0) ||
 		    (strcmp (newtask->t_ltp->lt_lname, "clbye") == 0))
		    if ((currentask->t_flags &  T_SCRIPT) &&
		        (prevtask->t_flags & T_SCRIPT))
			keep(prevtask);

	    /* If newtask is cleof(), close the input stream of the current
	     * task (the task whose input contained the cleof), and reopen
	     * as the null file.
	     */
	    if (newtask->t_flags & T_CLEOF) {
		if (currentask->t_in != stdin)
		    fclose (currentask->t_in);
		if (currentask != firstask)
		    currentask->t_in = fopen ("dev$null", "r");
	    }

	    if (!(firstask->t_flags & T_BATCH) &&
		 (newtask->t_in == stdin) && (newtask->t_out == stdout)) {
		newtask->t_flags |= T_INTERACTIVE;
		newtask->t_topd = topd;
		newtask->t_topos = topos;
		newtask->t_topcs = topcs;
		newtask->t_curpack = curpack;
	    }
	}

	/* Standardize the pfile.
	 * Set (or create if necessary) `$nargs', number of command line args,
	 *   based on pf_n which is set for each command line argument by
	 *   posargset, et al.
	 * If this ltask had no paramfile and we built one up from the
	 *   command line, then we need to add a `mode' param.  If it did have
	 *   a paramfile, then pfileload has already added it for us.
	 *   Point t_modep to the mode param for newtask.
	 */
	pp = paramfind (pfp, "$nargs", 0, YES);
	if (pp == NULL || (XINT)pp == ERR) {
	    char nabuf[FAKEPARAMLEN];
	    sprintf (nabuf, "$nargs,i,h,%d\n", pfp->pf_n);
	    pp = addparam (pfp, nabuf, NULL);
	    pp->p_mode |= M_FAKE;		/* never flush out $nargs */
	} else
	    pp->p_val.v_i = pfp->pf_n;

	if (pfp->pf_flags & PF_FAKE) {
	    newtask->t_modep = addparam (pfp, "mode,s,h,q\n", NULL);
	    /* pf_n will be used by paramsrch() to count positional arg
	     * matches; see it and param.h.
	     */
	    pfp->pf_n = 0;
	} else {
	    newtask->t_modep = paramfind (pfp, "mode", 0, YES);
	}

	if (newtask->t_modep == NULL)
	    cl_error (E_IERR, "no mode param for task `%s'",
		newtask->t_ltp->lt_lname);

	/* If task is being run in menu mode, call up eparam so that the user
	 * can edit/inspect the parameters.  If eparam is exited with ctrl/c
	 * do not run the task or update the pfile.  The parameter editor
	 * will make a copy of the task's pfile(s), edit it, and if necessary
	 * update the incore version created earlier by callnewtask().
	 */
	if ((taskmode(newtask) & M_MENU) || (newtask->t_flags & T_PSET)) {
	    if (epset (newtask->t_ltp->lt_lname) == ERR) {
		if (newtask->t_flags & T_PSET)
		    cl_error (E_UERR, "parameter file not updated");
		else
		    cl_error (E_UERR, "menu mode task execution aborted");
	    }
	}

	/* Set up bascode so new task has a good place to start building
	 * code.  See how the pc is set up before each call to the parser in
	 * main() loop.
	 */
	newtask->t_bascode = topos + 1;

	/* Set up io paths.  If the new task is cl(), it's command input
	 * and output streams are connected to those of the task which
	 * called currentask.  If the currentask is the firstask, there
	 * was no caller (no prevtask), so we must watch out for that.
	 * In the case of a script, commands are read from the script.
	 * In the case of a process, commands are read from the process.
	 */
	if (newtask->t_flags & T_PSET) {
	    newtask->t_in = fopen ("dev$null", "r");
	    newtask->t_out = newtask->t_stdout;

	} else if (newtask->t_flags & T_SCRIPT) {
	    if (cltrace)
		eprintf ("\t----- exec script %s (%s) -----\n",
		    newtask->t_ltp->lt_lname, newtask->t_ltp->lt_pname);

	    newtask->t_in = fopen (newtask->t_ltp->lt_pname, "r");
	    if (newtask->t_in == NULL)
		cl_error (E_UERR|E_P, "can not open script file `%s'",
		    newtask->t_ltp->lt_pname);
	    newtask->t_out = newtask->t_stdout;

	} else if (newtask->t_flags & T_CL) {
	    /* The command streams t_in and t_out have already been
	     * set up above by set_clio() in the test for T_INTERACTIVE.
	     */
	    /* Do nothing */

	} else {
	    char    startup_msg[SZ_STARTUPMSG+1];
	    int     timeit;

	    /* Connect to an executable process.
	     */
	    mk_startupmsg (newtask, startup_msg, SZ_STARTUPMSG);
	    timeit = (newtask->t_flags & T_TIMEIT) != 0;
	    if (cltrace)
		eprintf ("\t----- exec external task %s -----\n",
		    newtask->t_ltp->lt_lname);
	    newtask->t_pid = pr_connect (
		findexe (newtask->t_ltp->lt_pkp, newtask->t_ltp->lt_pname),
		startup_msg,
		&newtask->t_in, &newtask->t_out,
		newtask->t_stdin, newtask->t_stdout, newtask->t_stderr,
		newtask->t_stdgraph, newtask->t_stdimage, newtask->t_stdplot,
		timeit);
	}

	yyin = newtask->t_in;	/* set the input for the parser	*/

	/* Tell parser what to expect.
	 */
	parse_state = PARSE_FREE;
	if (newtask->t_flags & T_SCRIPT) {
	    proc_script = (newtask->t_flags & T_PSET) ? NO : procscript(yyin);

	    if (proc_script) {
		parse_state = PARSE_BODY;
		/* Skip to the BEGIN statement */
		newtask->t_scriptln = skip_to (yyin, "begin");
		if (newtask->t_scriptln == ERR)
		    cl_error (E_UERR, "No BEGIN statement in procedure script");

		/* Reset pointer here.
		 */
		proc_script = NO;
	    }
	}

	/* Log a start message for script and executable tasks.
	 */
	if (keeplog() && log_trace())
	    if (newtask->t_flags & T_SCRIPT || newtask->t_pid != -1) {
	    	char  logmsg[SZ_LINE];
	    	sprintf (logmsg, "Start (%s)", newtask->t_ltp->lt_pname);
	    	putlog (newtask, logmsg);
	    }

	newtask->t_flags |= T_RUNNING;
	currentask = newtask;	/* continue as new the new task; at last. */

	if (cldebug)
	    eprintf ("Returning from execnewtask.yyin, ct_in, nt_in:%d %d %d\n",
		yyin, currentask->t_in, newtask->t_in);
}


/* MK_STARTUPMSG -- Format the command to be sent to the interpreter in the
 * IRAF Main in the child to execute the indicated logical task.  The format
 * of this command is
 *
 *	taskname redir_args paramset_args
 *
 * where "redir_args" are used to either inform the task that a stream has
 * been redirected by the CL (file "$") or to actually redirect a stream,
 * and where "paramset_args" are assignments of the form "param=value".
 * For example, "4 > $" tells the task that its standard output (4 = integer
 * value of STDOUT) has been redirected.  Only parameters with static values,
 * i.e., with predefined values that are not expected to change during task
 * execution (no queries) may be passed on the command line.
 */
void
mk_startupmsg (
    struct task *tp,			/* task being executed		*/
    char *cmd,				/* receives formatted command	*/
    int maxch				/* max chars out		*/
)
{
	register char	*ip, *op, *cp;
	struct	pfile *pfp;
	struct	operand o;
	struct	param *pp;
	char	redir[20];

	/* Start with the task name.
	 * Task names which begin with an underscore are used to implement
	 * "invisible" commands which are not intended to be part of the
	 * user interface.  The distinction between these and regular
	 * commands is restricted to the CL, hence the leading underscore
	 * is stripped from the task name sent to the process.
	 */
	ip = tp->t_ltp->lt_lname;
	while (*ip == CH_INVIS)
	    ip++;
	strcpy (cmd, ip);

	/* Add redirection information.  We can omit the pseudofile stream
	 * codes for the standard input and output as the iraf main will
	 * assume those streams if no stream code is given, though we must
	 * be explicit for stderr and the graphics streams.
	 */
	if (tp->t_flags & (T_MYIN|T_MYOUT|T_MYERR)) {
	    if (tp->t_flags & T_MYIN)
		strcat (cmd, " < $");
	    if (tp->t_flags & T_MYOUT)
		strcat (cmd, " > $");
	    if (tp->t_flags & T_MYERR) {
		sprintf (redir, " %d> $", STDERR);
		strcat (cmd, redir);
	    }
	}
	if (tp->t_flags & (T_MYSTDGRAPH|T_MYSTDIMAGE|T_MYSTDPLOT)) {
	    if (tp->t_flags & T_MYSTDGRAPH) {
		sprintf (redir, " %d> $", STDGRAPH);
		strcat (cmd, redir);
	    }
	    if (tp->t_flags & T_MYSTDIMAGE) {
		sprintf (redir, " %d> $", STDIMAGE);
		strcat (cmd, redir);
	    }
	    if (tp->t_flags & T_MYSTDPLOT) {
		sprintf (redir, " %d> $", STDPLOT);
		strcat (cmd, redir);
	    }
	}

	for (cp=cmd;  *cp;  cp++)
	    --maxch;

	/* Add parameter assignments for all non list-structured parameters
	 * whose access would not cause a query, i.e., those parameters which
	 * already have a legal value and which are either hidden or were set
	 * on the command line.  Passing the values of these parameters on the
	 * command line speeds task startup by reducing the number of parameter
	 * requests that must be processed by handshaking over the IPC.
	 */
	for (pfp = tp->t_pfp;  pfp;  pfp = pfp->pf_npset) {
	    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
		o = pp->p_valo;

		/* Do not cache parameters which have an undefined value or
		 * for which the value is indirect to another parameter.
		 * Also, array parameters can not be cached currently.
		 */
		if (o.o_type & OT_UNDEF)
		    continue;
		if ((o.o_type & OT_BASIC) == OT_STRING &&
		    (o.o_val.v_s[0] == PF_INDIRECT))
		    continue;

		if (pp->p_type & PT_ARRAY)
		    continue;

		if (!(pp->p_type & PT_LIST) && !(effmode(pp) & M_QUERY)) {
		    char   buf[SZ_LINE+1];
		    char   val[SZ_LINE+1];

		    /* First format the param=value string in buf.
		     */

		    /* Start with "param=" if main pfile, or "pset.param=" if
		     * pset-param pfile.
		     */
		    if (pfp->pf_psetp != NULL) {
			ip = pfp->pf_psetp->p_name;
			for (op=buf;  (*op = *ip++);  op++)
			    ;
			*op++ = '.';
		    } else
			op = buf;

		    for (ip=pp->p_name;  (*op = *ip++);  op++)
			;
		    *op++ = '=';

		    /* Add "value".  If the parameter is string valued enclose
		     * the string in quotes and convert any newlines into \n.
		     */
		    sprop (val, &pp->p_valo);
		    if ((pp->p_type & OT_BASIC) == OT_STRING)
			*op++ = '"';

		    for (ip=val;  (*op = *ip++);  op++)
			if (*op == '\n') {
			    *op++ = '\\';
			    *op = 'n';
			} else if (*op == '"') {
			    *op++ = '\\';
			    *op = '"';
			}

		    if ((pp->p_type & OT_BASIC) == OT_STRING)
			*op++ = '"';

		    *op = EOS;

		    /* Now check to see if there is room in the output buffer.
		     * If not we can just quit, as the task will automatically
		     * query for any parameters not set on the command line.
		     * If there is room break the current line by appending \\n
		     * (an escaped newline) and append the new line.
		     */
		    maxch -= (strlen(buf) + 2);
		    if (maxch <= 0)
			break;

		    *cp++ = '\\';
		    *cp++ = '\n';

		    for (ip=buf;  (*cp = *ip++);  cp++)
			;
		}
	    }
	}

	/* Terminate the command line by appending an unescaped newline.
	 */
	*cp++ = '\n';
	*cp = EOS;

	if (cldebug)
	    eprintf ("CALL %s", cmd);
}


/* FINDEXE -- Search a set of standard places for an executable file to be
 * run.  Currently, we check first in the logical directory BIN for the
 * "installed" version of the executable, and if that is not found, use
 * the pathname given, which is the pathname specified in the TASK declaration.
 */
char *
findexe (
    struct package *pkg,	/* package in which task resides */
    char *pkg_path	/* pathname of exe file given in TASK statement	*/
)	
{
	static	char bin_path[SZ_PATHNAME+1], loc_path[SZ_PATHNAME+1];
	char	root[SZ_FNAME+1], root_path[SZ_PATHNAME+1];
	char	bindir[SZ_FNAME+1], *ip = NULL, *arch = NULL;
	char	bin_root[SZ_PATHNAME+1];
	char   *envget();


	memset (root, 0, SZ_FNAME);
	memset (bindir, 0, SZ_FNAME);
	memset (bin_path, 0, SZ_PATHNAME);
	memset (loc_path, 0, SZ_PATHNAME);
	memset (bin_root, 0, SZ_PATHNAME);
	memset (root_path, 0, SZ_PATHNAME);

	c_fnroot (pkg_path, root, SZ_FNAME);
	c_fpathname ((pkg ? pkg->pk_bin : BINDIR), root_path, SZ_PATHNAME);
	sprintf (bin_path, "%s%s.e", pkg ? pkg->pk_bin : BINDIR, root);
	sprintf (loc_path, "./%s.e", root);
	arch = envget ("arch");


	if (c_access (bin_path, 0, 0) == YES) {
	    return (bin_path);
	} else if (arch != NULL) {
	    /*  The binary wasn't found in the expected bin directory, but
	     *  on certain platforms look for alternate binaries that may
	     *  work.  This supports backward compatability with older
	     *  packages that may not have been upgraded to architecture
	     *  conventions in this release but which may contain usable
	     *  binaries (e.g. 32-bit 'linux' binaries on 64-bit systems
	     *  or older 'redhat' binaries where the core arch is 'linux').
	     */
	    memset (bin_root, 0, SZ_PATHNAME);
	    strcpy (bin_root, root_path);
	    if ((ip = strstr (bin_root, arch)))
		*ip = '\0';
	    else {
		int len = strlen (bin_root);
		if (bin_root[len-1] == '/')
		    bin_root[len-1] = '\0';
	    }

	    if (strcmp (arch, ".linux64") == 0) {
		/*  On 64-bit Linux systems we can use either of the
		 *  available 32-bit binaries if needed.  In v2.15 and
		 *  later, 'linux' is the preferred arch but look for
		 *  'redhat' in case it's a package that hasn't been
		 *  updated.
		 */
		sprintf (bin_path, "%s.linux/%s.e", bin_root, root);
		if (c_access (bin_path, 0, 0) == YES)
	    	    return (bin_path);

		sprintf (bin_path, "%s.redhat/%s.e", bin_root, root);
		if (c_access (bin_path, 0, 0) == YES)
	    	    return (bin_path);

	    } else if (strcmp (arch, ".linux") == 0) {
		/*  On 32-bit Linux systems, check for older 'redhat' binaries.
		 */
		sprintf (bin_path, "%s.redhat/%s.e", bin_root, root);
		if (c_access (bin_path, 0, 0) == YES)
	    	    return (bin_path);

	    } else if (strcmp (arch, ".macos64") == 0) {
		/*  On 64-bit Mac systems, check for older 'macintel' binaries.
		 */
		sprintf (bin_path, "%s.macintel/%s.e", bin_root, root);
		if (c_access (bin_path, 0, 0) == YES)
		    return (bin_path);

	    } else if (strcmp (arch, ".macintel") == 0) {
		/*  On 64-bit Mac systems, check for older 32-bin binaries.
		 */
		sprintf (bin_path, "%s.macosx/%s.e", bin_root, root);
		if (c_access (bin_path, 0, 0) == YES)
	    	    return (bin_path);

	    } else if (strcmp (arch, ".macosx") == 0) {
		/*  On 32-bit Mac systems, check for older 'macintel' binaries.
		 */
		sprintf (bin_path, "%s.macintel/%s.e", bin_root, root);
		if (c_access (bin_path, 0, 0) == YES)
	    	    return (bin_path);
	    }
	}

	if (c_access (pkg_path, 0, 0) == YES)
	    return (pkg_path);
	else
	    return (loc_path);
}


/* SET_CLIO -- Set the command input and output for the new cl().  If the
 * standard input or output has been redirected, use that, otherwise inherit
 * the t_in, t_out of the task preceeding the most recent non-CL task that has
 * the same t_in as the current task (this is not obvious, but permits packages
 * to be called or loaded within scripts).  In the case of a cl() type task
 * used to change packages, change the current package and push a cl() on the
 * control stack but continue reading from the current command stream.
 */
void
set_clio (register struct task *newtask)
{
	register struct task *tp;

	if ((newtask->t_stdin == currentask->t_stdin) &&
	    (currentask->t_in != stdin)) {
	    newtask->t_in = NULL;

	    if (newtask->t_flags & T_PKGCL) {		/* package()	*/
		newtask->t_in = currentask->t_in;
		tp = currentask;
	    } else {					/* cl()		*/
		for (tp=currentask;  tp != firstask; tp = next_task(tp))
		    if (!(tp->t_flags & T_CL) &&
			 (tp->t_in == currentask->t_in)) {
			tp = next_task(tp);
			newtask->t_in = tp->t_in;
			break;
		    }
	    }
	    if (newtask->t_in == NULL)
		cl_error (E_IERR, "Cannot find t_in for cl()");

	} else {					/* pk|cl <	*/
	    tp = NULL;
	    newtask->t_in = newtask->t_stdin;
	}

	if ((newtask->t_stdout == stdout) && (tp != NULL))
	    newtask->t_out = tp->t_out;
	else
	    newtask->t_out = newtask->t_stdout;		/* pk|cl >	*/
}


/* PPFIND -- Search the list of loaded psets for a task for the named
 * parameter.  If a taskname is given, search only the pset with that
 * taskname, else search all the psets associated with the running task.
 * This is called by the routines in opcodes.c to perform command line
 * assignments to parameters.
 */
struct param *
ppfind (
    struct pfile *pfp,		/* first pfile in chain		*/
    char *tn,			/* psetname (taskname) or null	*/
    char *pn,			/* parameter name		*/
    int pos,			/* for paramfind		*/
    int abbrev			/* for paramfind		*/
)
{
	struct	param *pp, *m_pp;
	struct	pfile *m_pfp = NULL;
	int	nchars;

	if (tn != NULL && *tn != EOS) {
	    /* Locate the named pset and search it. */
	    for (nchars=strlen(tn), m_pp=NULL;  pfp;  pfp = pfp->pf_npset) {
		if ((pp = pfp->pf_psetp)) {
		    if (strncmp (pp->p_name, tn, nchars) == 0) {
			if (strlen (pp->p_name) == nchars)
			    return (paramfind (pfp, pn, pos, abbrev));
			else if (m_pp)
			    return ((struct param *)ERR);
			else {
			    m_pp = pp;
			    m_pfp = pfp;
			}
		    }
		}
	    }

	    /* Unique abbreviation for pset was given. */
	    if (m_pp)
		return (paramfind (m_pfp, pn, pos, abbrev));
	    else
		return (NULL);

	} else {
	    /* Search all psets. */
	    for (;  pfp;  pfp = pfp->pf_npset)
		if ((pp = paramfind (pfp, pn, pos, abbrev)) != NULL)
		    return (pp);
	    return (NULL);
	}
}


/* PSETRELOAD -- Called when a pset parameter is assigned into by a command
 * line argument.  The previous value of the pset param will already have
 * been used by callnewtask() to load a pset.  We must replace the old pset
 * by the new one.
 */
void
psetreload (
    struct pfile *main_pfp,		/* main task pfile	*/
    struct param *psetp			/* pset param		*/
)
{
	struct	pfile *o_pfp, *n_pfp, *prev_pfp;
	struct	pfile *next_pfp = NULL;

	if (cldebug)
	    eprintf ("psetreload, pset %s\n", psetp->p_name);

	/* Locate the old pfile in the list of psets off the main task pfile.
	 */
	prev_pfp = main_pfp;
	for (o_pfp=prev_pfp->pf_npset;  o_pfp;  o_pfp = o_pfp->pf_npset)
	    if (o_pfp->pf_psetp == psetp)
		break;
	    else
		prev_pfp = o_pfp;
	
	if (o_pfp == NULL)
	    cl_error (E_IERR, "in psetreload: cannot find npset");
	else
	    next_pfp = o_pfp->pf_npset;

	/* Unlink the old pfile and its copy.  This must be done before loading
	 * the new pfile, else pfilesrch will simply reference the old pfile.
	 */
	pfileunlink (o_pfp->pf_oldpfp);
	pfileunlink (o_pfp);

	/* Load the new pfile.  */
	n_pfp = pfilecopy (pfilesrch (psetp->p_name));

	/* Link it into the pset list */
	prev_pfp->pf_npset = n_pfp;
	n_pfp->pf_npset = next_pfp;
	n_pfp->pf_psetp = o_pfp->pf_psetp;
}


/* IOFINISH -- Flush out and wrap up all pending io for given task.
 *   Called when the task is dying and it wants to close all files it opened.
 *   This includes a pipe if it used one, a file if it was a script and io
 *   redirections as indicated by the T_MYXXX flags.  The T_MYXXX flags are
 *   set only when the redirections were done for this task, ie, they were
 *   not simply inherited.
 * Just as a fail-safe measure, always check that a real stdio file is
 *   not being closed.
 * Don't call error() because in trying to restor to an interactive task
 *   it might call us again and cause an inf. loop.
 */
void
iofinish (register struct task *tp)
{
	register FILE *fp;
	int	flags;

	flags = tp->t_flags;

	/* Make sure we do not close files more than once.
	 */
	if (flags & T_RUNNING)
	    tp->t_flags &= ~T_RUNNING;
	else
	    return;

	if (cldebug)
	    eprintf ("flushing io for task `%s'\n", tp->t_ltp->lt_lname);

	if (flags & T_MYIN) {
	    fp = tp->t_stdin;
	    if (fp != stdin)
		fclose (fp);
	}
	if (flags & T_MYOUT) {
	    fflush (fp = tp->t_stdout);
	    if (fp != stdout)
		fclose (fp);
	}
	if (flags & T_MYERR) {
	    fflush (fp = tp->t_stderr);
	    if (fp != stderr)
		fclose (fp);
	}

	/* Close any redirected graphics output streams.
	 */
	if (flags & (T_MYSTDGRAPH|T_MYSTDIMAGE|T_MYSTDPLOT)) {
	    if (flags & T_MYSTDGRAPH)
		if (tp->t_stdgraph != tp->t_stdimage &&
		    tp->t_stdgraph != tp->t_stdplot)
		    fclose (tp->t_stdgraph);
	    if (flags & T_MYSTDIMAGE)
		if (tp->t_stdimage != tp->t_stdplot)
		    fclose (tp->t_stdimage);
	    if (flags & T_MYSTDPLOT)
		fclose (tp->t_stdplot);
	}

	/* If task i/o is redirected to a subprocess send the done message.
	 */
	if (flags & T_IPCIO)
	    fputs (IPCDONEMSG, tp->t_out);
	fflush (tp->t_out);

	/* Close files only for script task, not for a cl, a builtin, or
	 * a process.  Do call disconnect if the task lives in a process.
	 */
	if (flags & T_SCRIPT) {
	    fp = tp->t_in;
	    if (fp != stdin)
		fclose (fp);
	} else if (flags & (T_CL|T_BUILTIN)) {
	    ;
	} else if (tp->t_pid != -1)
	    pr_disconnect (tp->t_pid);

	/* Log a stop message for script and executable tasks.
	 */
	if (keeplog() && log_trace())
	    if (tp->t_flags & T_SCRIPT || tp->t_pid != -1)
	    	putlog (tp, "Stop");
}


/* RESTOR -- Restor all global variables for the given task and insure the
 *   integrity of the dictionary and control stack.
 * Go through the dictionary and properly disgard any packages, ltasks,
 *   pfiles, environments and params that may be above the new topd.
 * Write out any pfiles that are not just working copies that have been
 *   updated before discarding them.
 * Don't call error() because in trying to restor to an interactive task
 *   it might call us again and cause an inf. loop.  Instead, issue fatal error
 *   which will kill the cl for good.  This seems reasonable since we might
 *   as well die if we can't  restor back to an interactive state.
 * N.B. we assume that a pfile's params will either all lie above or all
 *   below tp->t_topd.  If this can ever happen, must add a further check
 *   of each pfile below topd and lob off any params above topd.
 *   The way posargset, et al, and call/execnewtask are now, we are safe.
 */
void
restor (struct task *tp)
{
	memel *topdp;
	register struct ltask *ltp;
	register struct package *pkp;
	register struct param *pp;
	register struct pfile *pfp;
	struct	param *last_pp;
	int	n;

	if (cldebug) {
	    eprintf ("restoring task `%s', tp: %d\n", tp->t_ltp->lt_lname,tp);
	    eprintf ("  topd %d/%d\n", topd, tp->t_topd);
	}

	topd = tp->t_topd;
	pc = tp->t_pc;
	topos = tp->t_topos;
	basos = tp->t_basos;
	topcs = tp->t_topcs;
	curpack = tp->t_curpack;

	yyin = tp->t_in;
	parse_state = PARSE_FREE;

	topdp = daddr (topd);

	/* Set pachead to first package below new topd.  Then lob off any ltasks
	 *   all remaining packages might have above topd.  It is sufficient to 
	 *   stop the ltask checks for a given package once find an ltask
	 *   below topd since the dictionary always grows upward.
 	 *   (Recall that since new ltasks are always added at the top of the
	 *   dictionary, and pkp->pk_ltp always points to the most recently
	 *   added ltask, then the thread moves to lower and lower addrs.)
	 *   Thus, work downward and throw out all ltasks until find one below
	 *   the new topd.
	 */
	for (pkp = reference (package, pachead);  pkp;  pkp = pkp->pk_npk)
	    if ((memel)pkp < (memel)topdp) {
		pachead = dereference (pkp);
		break;
	    }
	if (pkp == NULL)
	    cl_error (E_FERR, "package list broken");

	for (;  pkp;  pkp = pkp->pk_npk) {
	    for (ltp = pkp->pk_ltp; ltp; ltp = ltp->lt_nlt)
		if ((memel)ltp < (memel)topdp) {
		    pkp->pk_ltp = ltp;
		    break;
		}
	    if ((memel)pkp->pk_ltp >= (memel)topdp)
		/* All ltasks in this package were above topd */
		pkp->pk_ltp = NULL;
	}

	/* Similarly for pfiles and their params; however, since new params
	 *   are always added at the top of the dictionary and linked in at the
	 *   END of the list (at pfp->pf_lastpp), the thread off pfp->pf_pp
	 *   moves to higher and higher addrs.  Thus, we work our way up and
	 *   throw out all params above the new topd.  Also, close off any open
	 *   list files from discarded params along the way, if any.
	 * Also, see if any of the params were P_SET and set PF_UPDATE.
	 *   This avoids having to set PF_UPDATE for each assignment when the
	 *   is not always easily found.
	 * N.B. hope mode param that some t_modep is using is never disgarded..
	 * Also, guard against writing out pfiles in background.
	 */
	for (pfp = reference (pfile, parhead);  pfp;  pfp = pfp->pf_npf) {
	    /* Lob off any pfiles above new topd.  Go through their
	     * params, updating if necessary and closing any lists.
	     */
	    if ((memel)pfp < (memel)topdp) {
		parhead = dereference (pfp);
		break;
	    }

	    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
		/* Close if list file and enable flushing if P_SET.
		 */
		if (pp->p_type & PT_LIST)
		    closelist (pp);
		if (pp->p_flags & P_SET)
		    pfp->pf_flags |= PF_UPDATE;
	    }
	    if (((pfp->pf_flags & (PF_UPDATE|PF_COPY)) == PF_UPDATE) &&
		!(firstask->t_flags & T_BATCH))
		    pfileupdate (pfp);
	}

	/* Discard any recently added parameters above topd, where the pfile
	 * itself is below topd.  This happens when a new parameter is added
	 * to an existing incore pfile, e.g., in a declaration.
	 */
	for (;  pfp;  pfp = pfp->pf_npf) {
	    if ((memel)(pfp->pf_lastpp) < (memel)topdp)
		continue;		/* quick check */
	    last_pp = NULL;
	    n = 0;

	    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
		if ((memel)pp >= (memel)topdp) {
		    if (cldebug)
			fprintf (stderr, "chop pfile for task %s at param %s\n",
			    pfp->pf_ltp->lt_lname, last_pp->p_name);
		    if (last_pp)
			last_pp->p_np = NULL;
		    pfp->pf_lastpp = last_pp;
		    pfp->pf_n = n;
		    break;
		} else {
		    last_pp = pp;
		    n++;
		}
	    }
	}

	/* Delete any SET environment statements processed since this task
	 * was spawned.  If any redefs are uncovered the original values are
	 * reset in all connected subprocesses.
	 */
	if (tp->t_envp)
	    c_prenvfree (0, tp->t_envp);

	/* If the task being restored defined a package, dump all processes
	 * in the process cache spawned since the package was loaded.
	 */
	if (tp->t_pno)
	    pr_prunecache (tp->t_pno);
}


/* ONEOF -- "on eof" (not "one of"):
 * The current task has issued eof, either directly or via the "bye" command.
 *   Flush out all pending io, copy working pfile back to original if have one,
 *   pop a state back to the previous state and restore its environment.
 *   Avoid calling effecmode() if called from a builtin task since builtins
 *   do not have the "mode" parameter.
 *
 * If currentask is the first cl or we are batch, then we are truely done.
 *   Return true to the caller (EXECUTE), causing a return to the main.
 */
void
oneof (void)
{
	register struct pfile *pfp;
	register struct package *pkp;
	static	int	nerrs = 0;
	int	flags;

	if (cldebug)
	    eprintf ("received `%s' from `%s'\n", yeof ? "eof" : "bye",
		currentask == firstask ? "root" : currentask->t_ltp->lt_lname);

	if (!(firstask->t_flags & T_BATCH))
	    if (currentask == firstask && !gologout && !loggingout &&
		isatty (fileno (stdin)) && nerrs++ < 8)
		cl_error (E_UERR, "use `logout' to log out of the CL");

	flags = currentask->t_flags;

	if (!(flags & (T_BUILTIN|T_CL|T_SCRIPT|T_BATCH)))
	    fflush (currentask->t_out);
	iofinish (currentask);

	/* Copy back the main pfile and any pset-param files.  If the task
	 * which has terminated is a package script task, fix up the pfile
	 * pointer in the package descriptor to point to the updated pset.
	 */
	if (currentask->t_ltp->lt_flags & LT_PFILE) {
	    pfcopyback (pfp = currentask->t_pfp);
	    if (currentask->t_ltp->lt_flags & LT_DEFPCK)
		if ( (pkp = pacfind(currentask->t_ltp->lt_lname)) )
		    if (pkp->pk_pfp == pfp)
			pkp->pk_pfp = pfp->pf_oldpfp;
	    for (pfp = pfp->pf_npset;  pfp != NULL;  pfp = pfp->pf_npset)
		pfcopyback (pfp);
	}

	if (currentask == firstask)
	    alldone = 1;
	else {
	    currentask = poptask();
	    if (currentask->t_flags & T_BATCH)
		alldone = 1;
	}

	restor (currentask);		/* restore environment 		*/
}


/* PRINTCALL -- Print the calling sequence for a task.  Called by killtask()
 * to print stack trace.
 */
void
printcall (FILE *fp, struct task *tp)
{
	register struct param *pp;
	int	 notfirst = 0;

	fprintf (fp, "    %s (", tp->t_ltp->lt_lname);
	for (pp = tp->t_pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
	    if (pp->p_flags & P_CLSET) {
		if (notfirst)
		    fprintf (fp, ", ");
		notfirst++;
		if (!(tp->t_pfp->pf_flags & PF_FAKE) && !(pp->p_mode & M_FAKE))
		    fprintf (fp, "%s=", pp->p_name);

		/* Use only low level routines to print the parameter value to
		 * avoid error recursion.  In particular, parameter indirection
		 * is not resolved.
		 */
		if (!(pp->p_valo.o_type & OT_UNDEF))
		    fprop (fp, &pp->p_valo);
		else
		    fprintf (fp, "UNDEF");
	    }
	}
	fprintf (fp, ")\n");
}


/* KILLTASK -- Abort the currently executing task.  Only call this when a task
 *  is to be killed spontaneously, as from interrupt, not when it is just dying
 *   due to a "bye" or eof.
 * Close all pipes and pseudofiles, being careful not to close any that
 *   are real stdio files.
 * Note that our function is to kill an external task, not the process in which
 *   it resides.  The process is left running in the cache in case it is needed
 *   again.
 */
void
killtask (register struct task *tp)
{
	char	buf[128];

	/* Print stack trace, with arguments.
	 */
	if (!(tp->t_ltp->lt_flags&LT_INVIS) && !(firstask->t_flags&T_BATCH) &&
	    !(strcmp (tp->t_ltp->lt_lname, "error") == 0))
	    printcall (currentask->t_stderr, tp);

	/* If task is running in a subprocess, interrupt it and read the ERROR
	 * message.  Not certain there isn't some case where this could cause
	 * deadlock, but it does not seem so.  Interrupts are disabled during
	 * process startup.  If task issues ERROR then it is popped before
	 * we are called, without issuing the signal.
	 */
	if (tp->t_pid != -1) {
	    fflush (tp->t_out);
	    c_prsignal (tp->t_pid, X_INT);
	    fgets (buf, 128, tp->t_in);
	}

	iofinish (tp);
}
