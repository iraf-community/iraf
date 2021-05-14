/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_error
#define	import_finfo
#define	import_prstat
#include <iraf.h>

#include "config.h"
#include "errs.h"
#include "task.h"
#include "operand.h"
#include "param.h"
#include "proto.h"


/*
 * PRCACHE -- To minimize spawns, we maintain a cache of processes.  Each
 *   process may contain any number of tasks.  Zero or one tasks may be active
 *   in a process at a given time.  A process is spawned and added to the
 *   cache when a task therein needs to be run.  A process is terminated when
 *   its cache slot is needed by another process or when the cache is flushed.
 *   Error recovery does not normally result in process termination, even when
 *   the error is initiated by a task resident in the process.
 *
 *     pid = pr_connect (process, command, &in,&out, tin,tout,terr, timeit)
 *        pr_disconnect (pid)
 *	        pr_lock (pid)
 *	      pr_unlock (pid)
 *         pr_dumpcache (pid, break_locks)
 *	       pr_chdir (pid, newdir)
 *	      pr_envset (pid, envvar, valuestr)
 *   pid = pr_cachetask (ltname)
 *  pid = pr_pnametopid (pname)
 *         pr_listcache (fp)
 *	    pr_setcache (sz_prcache)
 *      pno = pr_getpno ()
 *        pr_prunecache (pno)
 *
 * The PR_CONNECT procedure executes an ltask resident in an external compiled
 *   process.  A process spawn occurs only if the process is not found in
 *   the cache or is not idle.  PR_DISCONNECT should be called when the ltask
 *   terminates to signal that the process is idle.  Processes may be locked
 *   in the cache, but this facility must be used with great discretion as
 *   it defeats the purpose of the cache and may lead to lockout.
 *
 * A process is passed the environment list and the name of the current working
 *   directory when it is spawned.  New SET environment declarations or chdir
 *   directives may be passed to all processes in the cache without flushing
 *   and refilling the cache, using the PR_CHDIR and PR_ENVSET commands.
 * Pseudofile i/o (xmit and xfer) is handled automatically by the system.
 *   Our function here is to connect the pseudofile streams of the ltask
 *   up to real streams at connect() time, via calls to c_prredir().
 *
 * The size of the cache is a runtime time parameter controlled by the CL
 *   parameter `szprcache'.  The default value of this is set either in
 *   cl$cl.par or in hlib$clpackage.par, hence may vary from site to site
 *   or even from host to host.
 */

extern	int	cldebug;
extern	int	cltrace;

typedef	XINT (*PFI)();

struct process {
	int	pr_pid;			/* process id of subprocess	*/
	long	pr_time;		/* time when process executed	*/
	short	pr_flags;		/* flag bits			*/
	short	pr_pno;			/* prcache process number	*/
	FILE	*pr_in, *pr_out;	/* in, out IPC channels		*/
	struct	process *pr_up;		/* up link (toward head)	*/
	struct	process *pr_dn;		/* down link (toward tail)	*/
	char	pr_name[SZ_PATHNAME+1];	/* filename of process		*/
};

#define	P_ACTIVE	01		/* task in process is in use	*/
#define P_LOCKED	02		/* process is locked in cache	*/

#define	pr_idle(pr)	(((pr)->pr_flags&P_ACTIVE)==0)
#define	pr_busy(pr)	(((pr)->pr_flags&(P_ACTIVE|P_LOCKED))!=0)

int	pr_pno = 1;			/* incremented for each connect	*/
int	sz_prcache = 2;			/* nprocess slots in cache	*/
struct	process pr_cache[MAXSUBPROC];
struct	process *pr_head = NULL, *pr_tail = NULL;
extern	char *findexe();

static void pr_pdisconnect (struct process *pr);
static void pr_tohead (struct process *pr);
static void pr_totail (struct process *pr);
static void pr_unlink (struct process *pr);


/* PR_CONNECT -- Run a task resident in an external process.  Look in the cache
 * for the named process; if not found or already active, spawn the process
 * and add it to the cache.  Send the startup message to the child to start
 * the task in execution.  The startup message specifies the name of the task
 * to be run, whether timing is desired, and any i/o redirection desired.
 * The input and output IPC file pointers are returned to the caller.
 *
 * TODO: This procedure was designed to minimize the changes to the high level
 * code, and is not done right.  Formatting of the startup command should be
 * done in a procedure within this package, rather than at the high level,
 * and should support i/o redirection to named files for (greatly) increased
 * efficiency of pipes.
 */
int
pr_connect (
    char *process,			/* filename of process		*/
    char *command,			/* IRAF Main command		*/
    FILE **in,
    FILE **out,				/* IPC channels (output)	*/
    FILE *t_in,
    FILE *t_out,
    FILE *t_err,			/* task stdin,out,err (input)	*/
    FILE *t_gr,
    FILE *t_im,
    FILE *t_pl,				/* task graphics streams	*/
    int timeit				/* if !0, time command		*/
)
{
	register int	pid;

	/* Connect subprocess. */
	if ((pid = pr_pconnect (process, in, out)) == NULL)
	    c_erract (EA_ERROR);


	/* Set default redirection of the standard i/o streams.
	 */
	c_prredir (pid, STDIN,    fileno(t_in));
	c_prredir (pid, STDOUT,   fileno(t_out));
	c_prredir (pid, STDERR,   fileno(t_err));
 	c_prredir (pid, STDGRAPH, fileno(t_gr));
 	c_prredir (pid, STDIMAGE, fileno(t_im));
 	c_prredir (pid, STDPLOT,  fileno(t_pl));

	/* Send startup message. */
	if (timeit)
	    fputc ('$', *out);
	fputs (command, *out);
	fflush (*out);

	if (cldebug)
	    eprintf ("connect: *in, *out, t_in, t_out: %d %d %d %d\n",
		*in, *out, t_in, t_out);
	if (cltrace) {
	    d_fmtmsg (stderr, "\t    ", command, 80 - 13);
	    eprintf ("\t--------------------------------\n");
	}

	return (pid);
}


/* PR_DISCONNECT -- Called when a task resident in an external process
 * terminates; also called during error recovery, e.g., following X_IPC.
 * Our only function for normal task termination is to clear the active flag.
 * Until the active flag is cleared the process cannot be reused nor terminated.
 */
void
pr_disconnect (
    int pid			/* process id returned by connect	*/
)
{
	struct process *pr;

	pr_checkup();
	for (pr=pr_head;  pr != NULL;  pr = pr->pr_dn) {
	    if (pr->pr_pid == pid) {
		pr->pr_flags &= ~P_ACTIVE;
		return;
	    }
	}
}


/* PR_PCONNECT -- Run a task resident in an external process.  Look in the cache
 * for the named process; if not found or already active, spawn the process
 * and add it to the cache.  Return the process id and file pointers to the
 * IPC channels to the caller.
 */
int
pr_pconnect (
    char *process,			/* filename of process		*/
    FILE **in,
    FILE **out			/* IPC channels (output)	*/
)
{
	struct process *pr;
	struct	process *pr_findproc();
	struct	_finfo fi;
	int	fd_in, fd_out;
	extern  int c_finfo (char *name, struct _finfo *fi);

	if (pr_head == NULL)
	    pr_initcache();
	else
	    pr_checkup();

	/* Search the cache to see if the process is already connected and
	 * inactive.  If the process is found idling in the cache, relink it
	 * at the head of the cache list, otherwise disconnect the inactive
	 * process nearest the tail of the list and spawn the new one to
	 * replace it.  The cached entry is automatically invalidated if the
	 * corresponding executable file has been modified (e.g., relinked),
	 * provided the process is not currently busy.  A process is considered
	 * busy if it is active or if it is locked in the cache.
	 */
	fi.fi_mtime = 0;
	if ((pr = pr_findproc (process)) != NULL && !pr_busy(pr)) {
	    if (c_finfo (process, &fi) == ERR || fi.fi_mtime > pr->pr_time) {
		pr_pdisconnect (pr);
		pr = NULL;
	    }
	}

	if (pr != NULL)
	    pr_tohead (pr);
	else {
	    /* Get process slot. */
	    for (pr=pr_tail;  pr != NULL;  pr=pr->pr_up)
		if (!pr_busy(pr)) {
		    if (pr->pr_pid != NULL)
			pr_pdisconnect (pr);
		    break;
		}
	    if (pr == NULL)
		cl_error (E_UERR, "process cache deadlock");
	    pr_tohead (pr);

	    /* Spawn subprocess.  Turn off interrupts during process startup
	     * to avoid crashing the IPC protocol.
	     */
	    if (cltrace)
		eprintf ("\t----- connect to %s -----\n", process);
	    intr_disable();
	    if ((pr->pr_pid = c_propen (process, &fd_in, &fd_out)) == NULL) {
		intr_enable();
		return (NULL);
	    }
	    intr_enable();

	    if (fi.fi_mtime == 0)
		if (c_finfo (process, &fi) == ERR)
		    fi.fi_mtime = 0;

	    pr->pr_time = fi.fi_mtime;
	    pr->pr_in  = FDTOFP (fd_in);
	    pr->pr_out = FDTOFP (fd_out);
	    pr->pr_flags = 0;
	    pr->pr_pno = pr_getpno();
	    strcpy (pr->pr_name, process);
	}

	pr->pr_flags |= P_ACTIVE;
	*in  = pr->pr_in;
	*out = pr->pr_out;

	return (pr->pr_pid);
}


/* PR_PDISCONNECT -- Remove a process from the process cache.  Processes are
 * disconnected when pushed out of the cache or when the cache is flushed.
 */
static void
pr_pdisconnect (struct process *pr)
{
	/* Ignore attempts to dump active processes.  This might happen
	 * when an active process executes a command which calls dumpcache.
	 */
	if (pr == NULL || pr->pr_pid == NULL || pr_busy(pr))
	    return;

	if (cltrace)
	    eprintf ("\t----- disconnect %s -----\n", pr->pr_name);

	/* Command child process to exit, close down communications.  This
	 * closes the IPC files as well as the terminating the process.
	 */
	c_prclose (pr->pr_pid);

	/* Clear process table entry and move process to tail of list.
	 */
	pr->pr_pid = 0;
	pr_totail (pr);
}


/* PR_SETCACHE -- Set the size of the process cache.  This is automatically
 * called whenever the value of the parameter cl.szprcache is set.  Changing
 * the cache size on an active cache causes the cache to be flushed and all
 * locked processes to be reconnected.
 */
void
pr_setcache (int new_szprcache)
{
	struct	process *pr;
	char	pname[MAXSUBPROC][SZ_PATHNAME+1];
	int	nprocs=0, pid, i;
	FILE	*fdummy;

	if (pr_head == NULL)
	    pr_initcache();
	else {
	    /* Get the names of any processes currently locked into the cache,
	     * then dump the cache.
	     */
	    for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn)
		if (pr->pr_pid != NULL && (pr->pr_flags & P_LOCKED))
		    strcpy (pname[nprocs++], pr->pr_name);
	    pr_dumpcache (0, 1);
	}

	/* Set the new value of sz_prcache. */
	sz_prcache = new_szprcache;
	if (sz_prcache < 2)
	    sz_prcache = 2;
	else if (sz_prcache > MAXSUBPROC)
	    sz_prcache = MAXSUBPROC;

	/* Relink the empty cache for sz_prcache cache slots. */
	pr_initcache();

	/* Attempt to recache the formerly locked processes.  There must be
	 * at least one empty slot left for new subprocesses.
	 */
	if (nprocs+1 > sz_prcache)
	    nprocs = sz_prcache-1;

	for (i=0;  i < nprocs;  i++) {
	    pid = pr_connect (findexe(NULL,pname[i]), "\n", &fdummy, &fdummy,
		stdin, stdout, stderr, 0,0,0, 0);
	    pr_disconnect (pid);
	    pr_lock (pid);
	}
}


/* PR_FINDPROC -- Search the cache for the named process.  Skip active
 * processes.
 */
struct process *
pr_findproc (char *process)
{
	struct	process *pr;

	for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn) {
	    if (pr->pr_pid != NULL && pr_idle(pr))
		if (strcmp (process, pr->pr_name) == 0)
		    return (pr);
	}

	return (NULL);
}


/* PR_CACHETASK -- Cache the process containing the named logical task.
 * If the process is already connected merely returns its pid, else connect
 * the process and return its pid.
 */
int
pr_cachetask (
    char *ltname		/* logical task name	*/
)
{
	register int  pid;
	struct	ltask *ltp;
	FILE	*fdummy;

	ltp = ltasksrch ("", ltname);
	if (ltp->lt_flags & (LT_SCRIPT|LT_BUILTIN))
	    return (ERR);
	if ((pid = pr_pnametopid(findexe(ltp->lt_pkp,ltp->lt_pname))) == NULL) {
	    pid = pr_connect (findexe(ltp->lt_pkp,ltp->lt_pname), "\n", &fdummy,
		&fdummy, stdin, stdout, stderr, 0,0,0, 0);
	    pr_disconnect (pid);
	}

	return (pid);
}


/* PR_LOCK -- Lock a connect process in the cache.  Must be used with caution
 * as deadlock may occur.  Locked processes are also not disconnected by
 * pr_dumpcache, which may not be what is desired.
 */
void
pr_lock (
    register int pid			/* process id			*/
)
{
	struct process *pr;

	if (pid != NULL) {
	    for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn)
		if (pr->pr_pid == pid) {
		    pr->pr_flags |= P_LOCKED;
		    break;
		}
	}
}


/* PR_UNLOCK -- Unlock a process, allowing it to be disconnected either when
 * forced out of the cache by another disconnect, or by a dumpcache.
 *
 * This function is currently unused.
 */
int
pr_unlock (
    register int pid			/* process id			*/
)
{
	struct process *pr;

	if (pid != NULL)
	    for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn)
		if (pr->pr_pid == pid)
		    return (pr->pr_flags &= ~P_LOCKED);

	return (ERR);
}


/* PR_LISTCACHE -- Info command, used to display the contents of the process
 * cache.  Format:  pid [RH][L] process_name
 */
void
pr_listcache (
    FILE *fp			/* output file		*/
)
{
	register struct process *pr;

	pr_checkup();
	for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn)
	    if (pr->pr_pid) {
		int	os_pid;
		char	nodename[SZ_FNAME+1];
		char	out[100];

		/* Print out pid in both decimal and hex, since the host
		 * system might need either.  Also print the VOS pid since
		 * that is what is needed for flprcache (although flprcache
		 * will accept a task name instead).  Note that c_kimapchan
		 * must be called to get the host PID if networking is in use.
		 */
		os_pid = c_kimapchan (pr->pr_pid, nodename, SZ_FNAME);
		sprintf (out, "[%02d] %s!%d(%xX)",
		    pr->pr_pid, nodename, os_pid, os_pid);
		fprintf (fp, "    %-32s %c%c %s\n",
		    out,
		    (pr->pr_flags&P_ACTIVE) ? 'R' : 'H',
		    (pr->pr_flags&P_LOCKED) ? 'L' : ' ',
		    pr->pr_name);

	    } else {
		fprintf(fp, "%12d", 0);
		fputc ('\n',fp);
	    }
}


/* PR_DUMPCACHE -- Disconnect the named process, or disconnect all processes
 * currently running in the cache, and clear the process tables.  A count of
 * the number of active processes not disconnected is returned as the function
 * value.  Locks may be forced if desired, i.e., when dumping the cache prior
 * to process termination.
 */
void
pr_dumpcache (int pid, int break_locks)
{
	register struct	process *pr;
	register int	n;

	pr_checkup();

	/* Do not traverse list using list pointers, because the first
	 * pr_disconnect will leave process pr at the tail of the list,
	 * causing premature termination.
	 */
	for (pr=pr_cache, n=sz_prcache;  --n >= 0;  pr++)
	    if ((pid == 0 && pr->pr_pid) || (pid == pr->pr_pid)) {
		if (break_locks && pr_idle(pr))
		    pr->pr_flags &= ~P_LOCKED;
		pr_pdisconnect (pr);
	    }

	if (break_locks)
	    pr_pno = 1;
}


/* PR_PRUNECACHE -- Disconnect all processes currently running in the cache
 * for which the process number is greater than that given, i.e., which were
 * connected since the given PNO was assigned.  Locked processes are not
 * affected.
 */
void
pr_prunecache (int pno)
{
	register struct	process *pr;
	register int	n;

	pr_checkup();

	/* Do not traverse list using list pointers, because the first
	 * pr_disconnect will leave process pr at the tail of the list,
	 * causing premature termination.
	 */
	for (pr=pr_cache, n=sz_prcache;  --n >= 0;  pr++)
	    if (pr->pr_pid && pr->pr_pno > pno)
		pr_pdisconnect (pr);
}


/* PR_GETPNO -- Get the next process number.  These are supposed to be returned
 * in time order.  If 10 million processes are spawned without setcache being
 * called, the counter might wrap around, but that does not seem likely and is
 * harmless in any case.
 */
int
pr_getpno (void)
{
	return (pr_pno++);
}


/* PR_PNAMETOPID -- Lookup the named process in the cache and return the pid
 * if found, NULL otherwise.
 */
int
pr_pnametopid (char *pname)
{
	register struct process *pr;

	pr_checkup();
	for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn)
	    if (strcmp (pr->pr_name, pname) == 0)
		return (pr->pr_pid);

	return (0);
}


/* PR_CHDIR -- Change the current working directory of a child process, or
 * of all connected but idle processes if pid=0.
 */
void
pr_chdir (register int pid, char *newdir)
{
	register struct process *pr;

	pr_checkup();
	for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn) {
	    if (pr->pr_pid == NULL || !pr_idle(pr))
		continue;
	    else if (pid == NULL || pr->pr_pid == pid)
		c_prchdir (pr->pr_pid, newdir);
	}
}


/* PR_ENVSET -- Set the value of an environment variable in a child process,
 * or in all connected but idle processes if pid=0.
 */
void
pr_envset (register int pid, char *envvar, char *valuestr)
{
	register struct process *pr;

	pr_checkup();
	for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn) {
	    if (pr->pr_pid == NULL || !pr_idle(pr))
		continue;
	    else if (pid == NULL || pr->pr_pid == pid)
		c_prenvset (pr->pr_pid, envvar, valuestr);
	}
}


/* PR_CHECKUP -- Check on the status of all connected child processes to see
 * if any have died.  If a process has died we must disconnect the process
 * to free file descriptors and the process cache slot.
 */
void
pr_checkup (void)
{
	register struct	process *pr;
	register int	n;

	/* Do not traverse list using list pointers, because the first
	 * pr_disconnect will leave process pr at the tail of the list,
	 * causing premature termination.
	 */
	for (pr=pr_cache, n=sz_prcache;  --n >= 0;  pr++) {
	    if (pr->pr_pid != NULL) {
		if (c_prstati (pr->pr_pid, PR_STATUS) == P_DEAD) {
		    pr->pr_flags = 0;
		    pr_pdisconnect (pr);
		}
	    }
	}
}


/* ONIPC -- Call this when get a signal that indicates a write to an IPC
 * channel with no reader.  We are called after the system X_IPC handler
 * has been called to cleanup the internal process tables and file system,
 * disabling any further output to the process.
 */
/* ARGSUSED */
void
onipc (
    int *vex,			/* virtual exception code	*/
    PFI *next_handler		/* next handler to be called	*/
)
{
	register struct	process *pr;

	for (pr=pr_head;  pr != NULL;  pr=pr->pr_dn) {
	    if (pr->pr_pid != NULL) {
		if (c_prstati (pr->pr_pid, PR_STATUS) == P_DEAD)
		    break;
	    }
	}

	cl_error (E_UERR, "Abnormal termination of child process '%s'",
	    pr ? pr->pr_name : "??");
}


/* PR_INITCACHE -- Initialize the process cache, i.e., set up the queue for the
 * first time.  The minimum cache size is 2.
 */
void
pr_initcache (void)
{
	register struct process *pr;
	register int	n;

	for (pr=pr_cache, n=MAXSUBPROC;  --n >= 0;  pr++) {
	    pr->pr_pid = 0;
	    pr->pr_flags = 0;
	    pr->pr_pno = 0;
	    pr->pr_in = pr->pr_out = NULL;
	    pr->pr_up = pr->pr_dn = NULL;
	}

	pr_head = pr_tail = pr_cache;
	for (n=1;  n < sz_prcache;  n++)
	    pr_tohead (&pr_cache[n]);

	pr_pno = 1;
}


/* PR_TOHEAD -- Relink a process at the head of the cache list.
 */
static void
pr_tohead (struct process *pr)
{
	if (pr_head != pr) {
	    pr_unlink (pr);
	    pr->pr_dn = pr_head;
	    pr->pr_up = NULL;
	    pr_head->pr_up = pr;
	    pr_head = pr;
	}
}


/* PR_TOTAIL -- Relink a process at the tail of the cache list.
 */
static void
pr_totail (struct process *pr)
{
	if (pr_tail != pr) {
	    pr_unlink (pr);
	    pr->pr_up = pr_tail;
	    pr->pr_dn = NULL;
	    pr_tail->pr_dn = pr;
	    pr_tail = pr;
	}
}


/* PR_UNLINK -- Unlink a process from the list.
 */
static void
pr_unlink (struct process *pr)
{
	if (pr->pr_up) {
	    (pr->pr_up)->pr_dn = pr->pr_dn;
	    if (pr == pr_tail)
		pr_tail = pr->pr_up;
	}

	if (pr->pr_dn) {
	    (pr->pr_dn)->pr_up = pr->pr_up;
	    if (pr == pr_head)
		pr_head = pr->pr_dn;
	}
}
