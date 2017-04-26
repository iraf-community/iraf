/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_knames
#define import_xwhen
#define import_ctype
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "operand.h"
#include "clmodes.h"
#include "mem.h"
#include "errs.h"
#include "param.h"
#include "task.h"
#include "proto.h"


/*
 * BKG -- All the functions relating to background ("&" asychronous) jobs.
 *
 * Here's how it works: yyparse() compiles code into the stack in the usual
 * way, incrementing pc as it goes.  If an '&' is seen, a snapshot of the
 * dictionary, the stack, and all related pointers is written to a file
 * immediately.  The new code is discarded (by putting the pc back where it was)
 * and yyparse() is called again.  See the forever() loop in main.
 * When started as a background cl, the snapshot file is read in and main
 * jumps immediately to run() as though yyparse() had just finished compiling.
 * Thus, background code is compiled in the parent but sent to the child cl
 * to be executed. The t_flags T_BATCH bit is set in the new cl's currentask
 * as well as firstask.  The former is used by bkg_abort() to abort
 * grandchildren.
 *
 *		 bkg_init ()			setup bkg job
 *		bkg_spawn (cmd)			spawn bkg job
 *		 bkg_wait (job)			wait for termination
 *		 bkg_kill (job)			kill bkg job
 *	    bkg_jobstatus (fp, job)		print job status
 *   bool = bkg_jobactive (job)			job is active
 *	       bkg_update (pmsg)		update bkg job status
 *
 *	      bkg_startup ()			called in bkg job to startup
 *	        bkg_abort ()			called by bkg job on interrupt
 *
 * Job numbers start at 1 and count up to the maximum number of bkg jobs
 * permitted.  In all of the above commands, the function will be performed
 * for a single job if job>0.  If job=0 the function is applied to all jobs.
 */

extern int cldebug;

/* We need to pass the pipe file names along to the bkg cl because the name
 * of the pipe file to use is determined AT PARSE TIME, not when the file
 * gets opened.  Without this, rmpipe() doesn't have the right names and
 * dreg pipe files will be left around.
 */
extern	int pipetable[];	/* pipe stack (pipecodes)		*/
extern	int nextpipe;		/* pipe stack pointer (next index)	*/
extern	int dobkg;		/* flag bkg execution 			*/

extern	memel cl_dictbuf[];	/* static dictionary area		*/
extern	long c_clktime();
extern	char *findexe();

#define	BKGHDRSIZ	(sizeof (struct bkgfilehdr))
#define	SZ_CMD		40		/* command in jobs table	*/
#define	SZ_BKCMD	80		/* command in bkg file		*/
#define	SZ_ENVDEF	1024		/* max size environment define	*/
#define	WAIT_PERIOD	5		/* bkg_wait wait interval	*/
#define	BKG_MAGIC	237
#define	SZ_BKGMSG	64
#define	CLDIR		"iraf$pkg/vocl/"

char	bkgmsg[SZ_BKGMSG+1];		/* passed to kernel		*/
int	lastjobno;			/* last job slot used		*/
int	bkgno;				/* job no. assigned by parent	*/
int	ppid;				/* pid of parent CL		*/

/* Template for all the junk that goes into the background status file.
 * Following this is the dictionary, then the stack.
 * TODO: avoid copying binary images of the stack and dictionary
 * areas to permit use of dynamic memory allocation.
 */
struct bkgfilehdr {
	int	b_magic;		/* file identification		*/
	int	b_bkgno;		/* bkg job number of new CL	*/
	int	b_ppid;			/* pid of parent CL		*/
	char	b_cmd[SZ_BKCMD];	/* command entered by user	*/
	int	b_pipetable[MAXPIPES];	/* pipefile database		*/
	int	b_nextpipe;		/* more pipefile database	*/
	int	b_szstack;		/* size of stack area, bytes	*/
	int	b_szdict;		/* size of dictionary, bytes	*/
	memel	*b_dict;		/* ptr to start of dict		*/
	XINT	b_topd,			/* dict ptr			*/
		b_maxd,			/* top of dict			*/
		b_pachead,		/* head of package list		*/
		b_parhead,		/* head of param list		*/
		b_pc,			/* pointer to compiled metacode	*/
		b_topos,		/* top of operand stack		*/
		b_basos,		/* base of operand stack	*/
		b_topcs;		/* top of control stack		*/
	struct task *b_firstask,	/* first task struct		*/
		*b_currentask;		/* current task struct		*/
	struct package *b_curpack;	/* current package		*/
};


/* Job table.  Associate the ordinal job number with the job number returned
 * by the system.  Record the command string which caused the bkg job to be
 * submitted, for output with bkg_jobstatus().
 */
struct _bkgjob {
	int	b_jobno;		/* job no. assigned by system	*/
	short	b_flags;		/* job state flags		*/
	short	b_exitcode;		/* exit status of job		*/
	long	b_clock;		/* start time; elapsed time	*/
	char	b_cmd[SZ_CMD+1];	/* command entered by user	*/
	int	b_verbose;		/* print output from job	*/
} jobtable[NBKG];

#define	J_RUNNING	01		/* job is running or queued	*/
#define	J_SERVICE	02		/* job needs service		*/
#define	J_KILLED	04		/* job was killed 		*/
#define	busy(job)	(jobtable[(job)-1].b_flags & J_RUNNING)

static	void bkg_close();


/* BKG_INIT -- Setup to execute a background job.  Called by the lexical
 * analyzer when the & is seen.  Read in the bkg control string (anything
 * following the & to end of line) and set the dobkg flag to flag background
 * execution of the command block currently being parsed.
 */
void 
bkg_init (
    char *bcs		/* background control string	*/
)
{
	strncpy (bkgmsg, bcs, SZ_BKGMSG);
	dobkg++;
}


/* BKG_SPAWN -- Spawn a new background job.  Called by main() when we have
 * seen an '&'.
 */
void 
bkg_spawn (
    char *cmd		/* command entered by user to spawn job	*/
)
{
	register struct _bkgjob *bk;
	register int	jobno, stat;
	char	clprocess[SZ_PATHNAME];
	char	*wbkgfile();
	char	*bkgfile;

	/* Find first unused slot in a circular search.
	 */
	bkg_update (1);
	jobno = (lastjobno == NBKG) ? 1 : lastjobno + 1;
	while (jobno != lastjobno) {
	    if (!busy (jobno))
		break;
	    if (jobno++ >= NBKG)
		jobno = 1;
	}
	if (jobno == lastjobno)
	    cl_error (E_UERR, "no more background job slots");

	/* Write bkgfile.  Delete any dreg bkg communication files.
	 */
	bkg_delfiles (jobno);
	bkgfile = wbkgfile (jobno, cmd, NULL);

	/* Spawn bkg job.
	 */
	sprintf (clprocess, "%s%s", CLDIR, CLPROCESS);
	intr_disable();
	jobtable[jobno-1].b_jobno = stat =
	    c_propdpr (findexe (firstask->t_curpack, clprocess),
		bkgfile, bkgmsg);

	if (stat == NULL) {
	    c_delete (bkgfile);
	    intr_enable();
	    cl_error (E_IERR, "cannot spawn background CL");
	} else {
	    bk = &jobtable[jobno-1];
	    bk->b_flags = J_RUNNING;
	    bk->b_clock = c_clktime (0L);
            bk->b_verbose = 2;
	    strncpy (bk->b_cmd, cmd, SZ_CMD);
	    *(bk->b_cmd+SZ_CMD) = EOS;
	    intr_enable();
	}

	eprintf ("[%d]\n", lastjobno = jobno);

	/* Make a logfile entry, saying we started the background job.
	 */
	if (keeplog() && log_background()) {
	    char  buf[SZ_LINE];
	    sprintf (buf, "Start [%d]", jobno);
	    putlog (0, buf);
	}
}


/* BKG_WAIT -- Wait for a background job to terminate.  If job=0, wait for
 * all bkg jobs to terminate.
 */
void 
bkg_wait (register int job)
{
	register int	j;
	int	active_jobs;

	if (job < 0 || job > NBKG)
	    return;

	do {
	    bkg_update (1);
	    if (job && !busy(job))
		return;
	    else {
		for (active_jobs=0, j=1;  j <= NBKG;  j++)
		    if (busy (j)) {
			active_jobs++;
			c_tsleep (WAIT_PERIOD);
			break;
		    }
	    }
	} while (active_jobs);
}


/* BKG_KILL -- Kill a background job.  If job=0, kill all background jobs.
 * If the job cannot be killed assume it is because it died unexpectedly.
 */
void 
bkg_kill (int job)
{
	register struct _bkgjob *bk;
	register int	j;

	bkg_update (1);
	if (job < 0 || job > NBKG)
	    eprintf ("[%d] invalid job number\n", job);
	else {
	    for (bk=jobtable, j=1;  j <= NBKG;  j++, bk++) {
		if ((job == 0 && busy(j)) || job == j) {
		    if (!busy(j))
			eprintf ("[%d] not in use\n", j);
		    else if (c_prkill (bk->b_jobno) == ERR)
			bkg_close (j, 2);
		    else {
			bk->b_flags |= J_KILLED;
			bkg_close (j, 2);
		    }
		}
	    }
	}
}


/* BKG_JOBSTATUS -- Print the status of one or more background jobs.
 * format jobno, elapsed clock time, status, user command, e.g.:
 *
 *		[1]   1:34  Running	command_1
 *		[2]  14:09  Stopped	command_2
 *		[3]   1:34 +Done 	command_3
 *		[4]   1:34  Exit 23	command_4
 *
 * A job will remain in the job table until another job is submitted which uses
 * the same slot.
 */
void 
bkg_jobstatus (
    FILE *fp,			/* output file		*/
    int job			/* job(s)		*/
)
{
	register struct _bkgjob *bk;
	register int	j, n, ch;
	register char	*ip;
	long	seconds;
	char	*outstr = NULL;

	bkg_update (1);
	for (bk=jobtable, j=1;  j <= NBKG;  j++, bk++)
	    if ((job == 0 && bk->b_jobno) || job == j) {
		/* Print jobno.  */
		fprintf (fp, "    [%d] ", j);

		/* If the clock is still running b_clock contains the start
		 * time.  If the job terminated it contains the elapsed time
		 * at job termination.
		 */
		if (busy(j))
		    seconds = c_clktime (bk->b_clock);
		else
		    seconds = bk->b_clock;
		fprintf (fp, "%6.0m ", (float)seconds / 60.0);
		fputc ((j == lastjobno) ? '+' : ' ', fp);
		    
		/* Print job status.
		 */
		if (busy(j)) {
		    if (bk->b_flags & J_SERVICE)
			outstr = "Stopped";
		    else
			outstr = "Running";
		} else if (bk->b_flags & J_KILLED) {
		    outstr = "Killed";
		} else if (bk->b_exitcode == OK) {
		    outstr = "Done";
		} else
		    sprintf (outstr, "Exit %d", bk->b_exitcode);
		fprintf (fp, "%-10s", outstr);

		/* Finally, print user command followed by newline.
		 */
		n = c_envgeti ("ttyncols") - (8 + 8 + 10) - 1;
		ip = bk->b_cmd;
		while (--n >= 0 && (ch = *ip++) != EOS)
		    if (ch == '\n' || ch == '\t')
			fputc (' ', fp);
		    else
			fputc (ch, fp);
		fputc ('\n', fp);
	    }
}


/* BKG_JOBACTIVE -- Determine if a background job is active, i.e., if the
 * job is still running.  It does not matter if the job is waiting for
 * service.
 */
int 
bkg_jobactive (int job)
{
	bkg_update (1);
	return (busy (job));
}


/* BKG_UPDATE -- Update the jobtable.  Examine each running process to see if
 * has terminated or if it needs service.  Set the appropriate bits in the
 * state flag in the job table.  When job termination is detected compute the
 * elapsed time and leave it in the table, along with the exit status.  If
 * the notify option is off the done or wait message will not have been printed
 * by the bkg job, so we output the message ourselves.
 */
void 
bkg_update (
    int pmsg			/* print event messages		*/
)
{
	register struct	_bkgjob *bk;
	register int	j;

	for (bk=jobtable, j=1;  j <= NBKG;  j++, bk++) {
	    if (busy(j)) {
		if (c_prdone (bk->b_jobno)) {
		    bkg_close (j, pmsg);
		} else if (bkg_wfservice (j)) {
		    if (pmsg && !notify() && !(bk->b_flags & J_SERVICE))
			eprintf ("[%d] stopped waiting for parameter input\n",
			    j);
		    bk->b_flags |= J_SERVICE;
		} else
		    bk->b_flags &= ~J_SERVICE;
	    }
	}
}


/* BKG_CLOSE -- Close a bkg job.  Called after determining that the job has
 * terminated.
 */
static void
bkg_close (
    int job,			/* job ordinal			*/
    int pmsg			/* print termination message	*/
)
{
	register struct	_bkgjob *bk = &jobtable[job-1];

	bk->b_clock = c_clktime (bk->b_clock);
	bk->b_exitcode = c_prcldpr (bk->b_jobno);
	bk->b_flags &= ~(J_RUNNING|J_SERVICE);

	if (bk->b_verbose && (pmsg > 1 || (pmsg == 1 && !notify()))) {
	    if (bk->b_exitcode != OK)
		eprintf ("[%d] exit %d\n", job, bk->b_exitcode);
	    else
		eprintf ("[%d] done\n", job);
	}

	/* Make a logfile entry, saying the background job ended.
	 */
	if (keeplog() && log_background()) {
	    char  buf[SZ_LINE];
	    sprintf (buf, "Stop [%d]", job);
	    putlog (0, buf);
	}
}


/* BKG_WFSERVICE -- Determine if a bkg job is waiting for service (for the
 * user to answer a query).
 */
int 
bkg_wfservice (int job)
{
	char	bkg_query_file[SZ_PATHNAME];
	char	query_response_file[SZ_PATHNAME];

	get_bkgqfiles (job, c_getpid(), bkg_query_file, query_response_file);
	return (c_access (bkg_query_file,0,0));
}


/* BKG_DELFILES -- Called when a background job is spawned to make sure there
 * are no dreg query service files lying about from a prior job which did not
 * complete normally.
 */
void 
bkg_delfiles (int job)
{
	char	bkg_query_file[SZ_PATHNAME];
	char	query_response_file[SZ_PATHNAME];

	get_bkgqfiles (job, c_getpid(), bkg_query_file, query_response_file);
	c_delete (bkg_query_file);
	c_delete (query_response_file);
}


/* BKG_STARTUP -- Called by a background CL during process startup.  Read in
 * the bkgfile and restore runtime context of the parent.
 */
void 
bkg_startup (char *bkgfile)
{
	rbkgfile (bkgfile);
	setclmodes (firstask);
	currentask->t_flags = firstask->t_flags = T_BATCH;
}


/* BKG_ABORT -- Called by onint() in main.c when we get interrupted while
 * running as a bkg job.  Kill any and all background CL's WE may have
 * started, flush io, close any open pipe files, remove our job seq lock
 * file, kill all tasks back to the one that started us as background and
 * write a message on stderr.
 */
void 
bkg_abort (void)
{
	register int	job;
	register struct task *tp;

	for (job=1;  job <= NBKG;  job++)
	    if (busy (job))
		bkg_kill (job);

	iofinish (currentask);
	delpipes (0);

	tp = currentask;
	while (!(tp->t_flags & T_BATCH)) {
	    killtask (tp);
	    tp = poptask();
	}

	fprintf (stderr, "\n[%d] killed\n", bkgno);
}


/* WBKGFILE -- Create a unique file, write and close the background file.
 * Jobno is the job number the new cl is to think its running for.
 * We don't use the global bkgno because that's OUR number, if we ourselves
 *   are background.
 * Return pointer to the new name.
 * No error return, but we may call error() and never return.
 */
char *
wbkgfile (
    int jobno,			/* ordinal jobnumber of child	*/
    char *cmd,			/* command to be run in bkg	*/
    char *fname			/* filename for env file	*/
)
{
	static	char *bkgwerr = "error writing background job file";
	static	char bkgfile[SZ_PATHNAME];
	struct	bkgfilehdr bh;
	int	n, show_redefs=NO;
	FILE	*fp;


	/* If we're a normal background job no name was specified so 
	 * create a unique uparm file.  Otherwise, use the specified
	 * filename (e.g. for special onerr handling).
	 */
	if (fname == (char *)NULL)
	    c_mktemp ("uparm$bkg", bkgfile, SZ_PATHNAME);
	else {
	    if (c_access (fname,0,0) == YES)
		c_delete (fname);
	    strncpy (bkgfile, fname, strlen(fname));
	}

	/* Open the file. */
	if ((fp = fopen (bkgfile, "wb")) == NULL)
	    cl_error (E_IERR, "unable to create background job file `%s'",
		bkgfile);

	for (n=0;  n < MAXPIPES;  n++)
	    bh.b_pipetable[n] = pipetable[n];
	bh.b_nextpipe = nextpipe;

	strncpy (bh.b_cmd, cmd, SZ_BKCMD);

	bh.b_magic	= BKG_MAGIC;
	bh.b_bkgno	= jobno;
	bh.b_ppid	= c_getpid();
	bh.b_szstack	= STACKSIZ * BPI;
	bh.b_szdict	= topd * BPI;
	bh.b_dict 	= dictionary;
	bh.b_topd 	= topd;
	bh.b_maxd 	= maxd;
	bh.b_parhead 	= parhead;
	bh.b_pachead 	= pachead;
	bh.b_pc 	= pc;
	bh.b_topos 	= topos;
	bh.b_basos 	= basos;
	bh.b_topcs 	= topcs;
	bh.b_firstask 	= firstask;
	bh.b_currentask	= currentask;
	bh.b_curpack 	= curpack;

	/* Write the header structure, followed by the stack area and the
	 * dictionary.
	 */
	if (fwrite ((char *)&bh, BKGHDRSIZ, 1, fp) == NULL)
	    cl_error (E_IERR|E_P, bkgwerr);
	if (fwrite ((char *)stack, STACKSIZ, BPI, fp) == NULL)
	    cl_error (E_IERR|E_P, bkgwerr);
	if (fwrite ((char *)dictionary, topd, BPI, fp) == NULL)
	    cl_error (E_IERR|E_P, bkgwerr);

	/* Write the environment as a sequence of SET statements in binary.
	 * Append a blank line as a terminator.
	 */
	c_envlist (fileno(fp), "set ", show_redefs);
	fputs ("\n", fp);

	fclose (fp);
	return (bkgfile);
}


/* RBKGFILE -- Read in and use background status file with given name.
 * Do not remove the file -- the system does that upon process termination
 * to signal the parent.  If an error occurs do not call cl_error since
 * we are called during process startup and error recovery is not yet
 * possible (a memory fault will result).
 */
void 
rbkgfile (char *bkgfile)
{
	char	set[SZ_ENVDEF];
	struct	bkgfilehdr bh;
	int	n;
	FILE	*fp;


	if ((fp = fopen (bkgfile, "rb")) == NULL) {
	    fprintf (stderr,
		"[B] ERROR: unable to open background job file `%s'\n",
		bkgfile);
	    clexit();
	}

	if (fread ((char *)&bh, BKGHDRSIZ, 1, fp) == NULL)
	    goto abort_;
	if (bh.b_magic != BKG_MAGIC) {
	    fprintf (stderr, "[B] ERROR: bad magic in bkgfile '%s'\n", bkgfile);
	    clexit();
	}

	/* The following assumes that the dictionary is statically allocated
	 * and cannot move around.
	 */
	if (bh.b_dict != cl_dictbuf) {
	    fprintf (stderr,
		"BKG ERROR: new CL installed; logout and try again\n");
	    clexit();
	}

	intr_disable();

	for (n=0;  n < MAXPIPES;  n++)
	    pipetable[n] = bh.b_pipetable[n];
	nextpipe = bh.b_nextpipe;

	bkgno		= bh.b_bkgno;
	ppid		= bh.b_ppid;
	dictionary 	= bh.b_dict;
	topd	 	= bh.b_topd;
	maxd 		= bh.b_maxd;
	pachead 	= bh.b_pachead;
	parhead 	= bh.b_parhead;
	pc 		= bh.b_pc;
	topos 		= bh.b_topos;
	basos 		= bh.b_basos;
	topcs 		= bh.b_topcs;
	firstask 	= bh.b_firstask;
	currentask 	= bh.b_currentask;
	curpack 	= bh.b_curpack;

	/* Read stack area and dictionary.
	 */
	if (fread ((char *)stack, bh.b_szstack, 1, fp) == NULL)
	    goto abort_;
	if (fread ((char *)dictionary, bh.b_szdict, 1, fp) == NULL)
	    goto abort_;

	/* Read and restore the environment.
	 */
	do {
	    if (fgets (set, SZ_ENVDEF, fp) == NULL)
		goto abort_;
	} while (c_envscan (set));

	intr_enable();
	fclose (fp);
	return;
abort_:
	intr_enable();
	eprintf ("[B] ERROR: error reading background file\n");
	clexit();
}
