/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/wait.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* Process table code.  The high level code assumes that it can open and close
 * processes in any order.  The UNIX "wait" primitive, called when a process is
 * closed, returns the process status and pid of the first process to exit.
 * Hence several calls to wait may be necessary to wait for a given process to
 * exit.  We hide all this behind the pr_wait call, which waits for a PARTICULAR
 * process to exit and returns its exit status.
 *
 * NOT INTERFACE PROCEDURES.  This code is only called internally by other
 * kernel procedures.  All primitives which execute subprocesses, i.e., ZOPCPR,
 * ZOPDPR, ZOSCMD, etc. must call these routines.
 */

struct proctable {
	int	pr_pid;		/* process id				*/
	int	pr_active;	/* if YES, process is still active	*/
	int	pr_inchan;	/* input IPC channel			*/
	int	pr_outchan;	/* output IPC channel			*/
	int	pr_exit_status;	/* process exit_status			*/
} prtable[MAXPROCS];


/* PR_ENTER -- Make a new entry in the process table.  Something is very wrong
 * if the table overflows.
 */
pr_enter (pid, inchan, outchan)
int	pid;
int	inchan, outchan;
{
	register struct proctable *pr;
	struct	proctable *pr_findpid();

	if ((pr = pr_findpid (NULL)) == NULL)
	    kernel_panic ("process table overflow");
	else {
	    pr->pr_pid = pid;
	    pr->pr_active = YES;
	    pr->pr_inchan = inchan;
	    pr->pr_outchan = outchan;
	}
}


/* PR_WAIT -- Wait for the process associated with the given pid to terminate
 * and return it's exit status.  If there is no such process in the table
 * return ERR.  The table entry is cleared by this call.
 */
pr_wait (pid)
int	pid;
{
	register struct proctable *pr;
	int	waitpid, error_code;
	struct	proctable *pr_findpid();
	union	wait exit_status;

	/* Lookup process in table.  Return ERR if there is no entry.
	 */
	if ((pr = pr_findpid (pid)) == NULL)
	    return (ERR);

	if (pr->pr_active == NO) {
	    /* Process has already terminated.  Clear table entry and return
	     * exit status (set in a previous call).
	     */
	    pr->pr_pid = NULL;
	    return (pr->pr_exit_status);

	} else {
	    /* Process is in table but has not yet terminated.  Call wait until
	     * the process exits.  If other processes exit in the meantime
	     * save their exit status in the table and mark them inactive.
	     * If an unknown process terminates ignore it; this will happen
	     * when a killed bkg process terminates after its process slot
	     * has been released.
	     */
	    while ((waitpid = wait (&exit_status)) != ERR)
		if ((pr = pr_findpid (waitpid)) != NULL) {
		    pr->pr_active = NO;

		    /* The integer argument to exit() is returned in the
		     * wait struct defined in <sys/wait.h>.
		     */
		    error_code = exit_status.w_T.w_Retcode;
		    pr->pr_exit_status = error_code ? error_code : XOK;

		    if (waitpid == pid) {
			pr->pr_pid = NULL;
			return (pr->pr_exit_status);
		    }
		}
	    return (ERR);
	}
}


/* PR_GETIPC -- Get the codes for the IPC channels assigned to a process.
 */
pr_getipc (pid, inchan, outchan)
int	pid;
int	*inchan, *outchan;
{
	register struct proctable *pr;
	int	waitpid, exit_status, error_code;
	struct	proctable *pr_findpid();

	/* Lookup process in table.  Return ERR if there is no entry.
	 */
	if ((pr = pr_findpid (pid)) == NULL)
	    return (ERR);
	else {
	    *inchan = pr->pr_inchan;
	    *outchan = pr->pr_outchan;
	    return (pid);
	}
}


/* PR_FINDPID -- Search the process table for a process.  NULL is returned if
 * the process cannot be found, otherwise a pointer to the table entry is
 * returned.
 */
struct proctable *
pr_findpid (pid)
int	pid;
{
	register int	pr;

	for (pr=0;  pr < MAXPROCS;  pr++)
	    if (prtable[pr].pr_pid == pid)
		return (&prtable[pr]);
	
	return (NULL);
}


/* PR_RELEASE -- Release the table entry for the process.  Used when a process
 * is killed and we do not wish to wait for process termination.
 */
pr_release (pid)
int	pid;
{
	register struct proctable *pr;

	if ((pr = pr_findpid (pid)) != NULL)
	    pr->pr_pid = NULL;
}
