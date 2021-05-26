/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_setjmp
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
 *	        bkg_abort ()			called by bkg job on interrupt
 *
 * Job numbers start at 1 and count up to the maximum number of bkg jobs
 * permitted.  In all of the above commands, the function will be performed
 * for a single job if job>0.  If job=0 the function is applied to all jobs.
 */

extern int cldebug;
extern jmp_buf child_startup;

/* We need to pass the pipe file names along to the bkg cl because the name
 * of the pipe file to use is determined AT PARSE TIME, not when the file
 * gets opened.  Without this, rmpipe() doesn't have the right names and
 * dreg pipe files will be left around.
 */
extern	int pipetable[];	/* pipe stack (pipecodes)		*/
extern	int nextpipe;		/* pipe stack pointer (next index)	*/
extern	int dobkg;		/* flag bkg execution 			*/

extern	memel cl_dictbuf[];	/* static dictionary area		*/
extern	long c_clktime(long reftime);
extern	unsigned int c_prfodpr(void);
extern	char *findexe();

#define	SZ_CMD		40		/* command in jobs table	*/
#define	SZ_ENVDEF	1024		/* max size environment define	*/
#define	WAIT_PERIOD	5		/* bkg_wait wait interval	*/
#define	SZ_BKGMSG	64
#define	CLDIR		"iraf$pkg/cl/"

char	bkgmsg[SZ_BKGMSG+1];		/* passed to kernel		*/
int	lastjobno;			/* last job slot used		*/
int	bkgno;				/* job no. assigned by parent	*/
int	ppid;				/* pid of parent CL		*/


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
} jobtable[NBKG];

#define	J_RUNNING	01		/* job is running or queued	*/
#define	J_SERVICE	02		/* job needs service		*/
#define	J_KILLED	04		/* job was killed 		*/
#define	busy(job)	(jobtable[(job)-1].b_flags & J_RUNNING)


static void bkg_close (int job, int pmsg);


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
	int	curpid = c_getpid();
	void	pr_initcache();

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

	/* Delete any dreg bkg communication files.
	 */
	bkg_delfiles (jobno);

	/* Spawn bkg job.
	 */
	intr_disable();
	jobtable[jobno-1].b_jobno = stat = c_prfodpr();

	if (stat < 0) {
	    intr_enable();
	    cl_error (E_IERR, "cannot spawn background CL");
	} else if (stat > 0) { /* parent */
	    bk = &jobtable[jobno-1];
	    bk->b_flags = J_RUNNING;
	    bk->b_clock = c_clktime (0L);
	    strncpy (bk->b_cmd, cmd, SZ_CMD);
	    *(bk->b_cmd+SZ_CMD) = EOS;
	    intr_enable();
	} else { /* child */
	    bkgno = jobno;
	    ppid = curpid;
	    currentask->t_flags = firstask->t_flags = T_BATCH;
	    pr_initcache();
	    longjmp(child_startup, 1);
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
		        fprintf (fp, "%-10s", "Stopped");
		    else
		        fprintf (fp, "%-10s", "Running");
		} else if (bk->b_flags & J_KILLED) {
		    fprintf (fp, "%-10s", "Killed");
		} else if (bk->b_exitcode == OK) {
		    fprintf (fp, "%-10s", "Done");
		} else
		    fprintf (fp, "Exit %-5d", bk->b_exitcode);

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

	if (pmsg > 1 || (pmsg == 1 && !notify())) {
	    if (bk->b_exitcode != OK) {
		eprintf ("[%d] exit %d\n", job, bk->b_exitcode);
	    } else {
		eprintf ("[%d] done\n", job);
	    }
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
