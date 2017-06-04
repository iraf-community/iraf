/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <ctype.h>
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

#define import_spp
#define	import_xwhen
#define	import_kernel
#define	import_knames
#include <iraf.h>

#define	QUANTUM		6
/* #define	vfork	fork */

extern void pr_enter (int pid, int inchan, int outchan);
extern int  pr_wait (int pid);
extern void pr_release (int pid);


/* ZOPDPR -- Open a detached process.  In this implementation detached
 * processes begin execution immediately, runing concurrently with the parent.
 * "Jobcode" can be anything we want, provided it is unique.  Since detached
 * processes run concurrently we return the pid of the child as the jobcode.
 */
int
ZOPDPR (
  PKCHAR  *osfn,
  PKCHAR  *bkgfile,
  PKCHAR  *queue,
  XINT	  *jobcode
)
{
	register char	*ip;
	register int	sum;
	int	pid, maxforks = 3;
	int	curpri, priority, delta, neg;


	/* Check that the process file exists and is executable.
	 * Check that the background file exists and is readable.
	 */
	if (access ((char *)osfn, 1) == ERR) {
	    *jobcode = XERR;
	    return (XERR);
	} else if (access ((char *)bkgfile, 4) == ERR) {
	    *jobcode = XERR;
	    return (XERR);
	}

	/* Determine priority at which child process is to run.  A relative
	 * priority of -1 lowers the priority by QUANTUM UNIX units (e.g., nices
	 * the process to 4, 6 or whatever the QUANTUM is).  If an absolute
	 * priority is specified it is used without scaling.
	 */
	curpri = getpriority (PRIO_PROCESS, 0);

	for (ip=(char *)queue;  isspace (*ip);  ip++)
	    ;
	if (*ip != EOS) {
	    if (*ip == '+' || *ip == '-') {
		delta = 1;
		neg = (*ip++ == '-');
	    } else {
		delta = 0;
		neg = 0;
	    }

	    for (sum=0;  isdigit (*ip);  ip++)
		sum = sum * 10 + *ip - '0';
	    if (neg)
		sum = -sum;

	} else {
	    delta = 1;
	    sum = -1;
	}

	if (delta)
	    priority = curpri - (QUANTUM * sum);
	else
	    priority = sum;

	/* Create child process.  Vfork is used to avoid necessity to copy
	 * the full address space of the parent, since we are going to overlay
	 * a new process immediately with Execl anyhow.  The child inherits
	 * the open stdio files.  The fork can fail if swap space is full or
	 * if we have too many processes.
	 */
	while ((pid = vfork()) == ERR) {
	    if (--maxforks == 0) {
		*jobcode = XERR;
		return (XERR);
	    }
	    sleep (2);
	}

	if (pid == 0) {
	    /* New, child process.
	     * Arrange for the local file descriptors of the parent to be
	     * closed in the child if the exec succeeds.  IRAF subprocesses
	     * do not expect to inherit any file descriptors other than
	     * stdin, stdout, and stderr.
	     */
	    struct rlimit rlim;
	    int maxfd, fd;

	    if (getrlimit (RLIMIT_NOFILE, &rlim))
		maxfd = MAXOFILES;
	    else
		maxfd = rlim.rlim_cur;

	    for (fd=3;  fd < min(MAXOFILES,maxfd);  fd++)
		fcntl (fd, F_SETFD, 1);

	    setpriority (PRIO_PROCESS, 0, priority);

	    /* Since we used vfork we share memory with the parent until the
	     * call to execl(), hence we must not close any files or do
	     * anything else which would corrupt the parent's data structures.
	     * Instead, immediately exec the new process (will not return if
	     * successful).  The "-d" flag tells the subprocess that it is a
	     * detached process.  The background file name is passed to the
	     * child, which reads the file to learn what to do, and deletes
	     * the file upon exit.
	     */
	    execl ((char *)osfn, (char *)osfn, "-d", (char *)bkgfile, 
		(char *) 0);

	    /* If we get here the new process could not be executed for some
	     * reason.  Shutdown, calling _exit to avoid flushing parent's
	     * io buffers.  Delete bkgfile to tell parent that child has
	     * terminated.
	     */
	    unlink ((char *)bkgfile);
	    _exit (1);

	} else {
	    /* Existing, parent process.
	     * Save pid in parent's process table.  Entry cleared when
	     * pr_wait is called to wait for process to terminate.
	     */
	    pr_enter (pid, 0, 0);
	}

	*jobcode = pid;

	return (XOK);
}


/* ZCLDPR -- Close a detached process.  If killflag is set interrupt the
 * process before waiting for it to die.  A detached process will shutdown
 * when interrupted, unlike a connected subprocess which merely processes
 * the interrupt and continues execution.  The process itself deletes the
 * bkgfile before exiting.
 */
int
ZCLDPR (
  XINT	*jobcode,
  XINT	*killflag,
  XINT	*exit_status
)
{
	int	pid = *jobcode;


	/* If killing process do not wait for it to die.
	 */
	if (*killflag == XYES) {
	    if (kill (pid, SIGTERM) == ERR) {
		*exit_status = XERR;
		return (XERR);
	    } else {
		pr_release (pid);
		*exit_status = X_INT;
		return (*exit_status);
	    }
	}

	if ((*exit_status = pr_wait (pid)) == ERR)
	    *exit_status = XERR;

	return (*exit_status);
}
