/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#define	import_kernel
#define	import_knames
#define	import_error
#define import_spp
#include <iraf.h>

static	int lastsig;
extern	int pr_onint(int usig, int *hwcode, int *scp);

/* #define	vfork	fork */

extern void pr_enter (int pid, int inchan, int outchan);
extern int  pr_wait (int pid);



/* ZOSCMD -- Send a (machine dependent) command to the host operating
 * system.  If nonnull stdout or stderr filenames are given, try to spool
 * the output in these files.
 */
int
ZOSCMD (
  PKCHAR  *oscmd,
  PKCHAR  *stdin_file, 
  PKCHAR  *stdout_file, 
  PKCHAR  *stderr_file,
  XINT	  *status
)
{
	char	*shell, *sh = "/bin/sh";
	char	*sin, *sout, *serr, *cmd;
	struct	rlimit rlim;
	int	maxfd, fd, pid;
	struct sigaction oldact;

	extern  int _u_fmode(int mode);


	cmd  = (char *)oscmd;
	sin  = (char *)stdin_file;
	sout = (char *)stdout_file;
	serr = (char *)stderr_file;

	/* The Bourne shell SH is used if the first character of the cmd
	 * is '!' or if the user does not have SHELL defined in their
	 * environment.
	 */
	if (*cmd == '!') {
	    shell = sh;
	    cmd++;
	} else if ((shell = getenv ("SHELL")) == NULL)
	    shell = sh;

	sigaction (SIGINT, NULL, &oldact);

	/* Vfork is faster if we can use it.
	 */
	if (*sin == EOS && *sout == EOS && *serr == EOS) {
	    while ((pid = vfork()) == ERR)
		sleep (2);
	} else {
	    while ((pid =  fork()) == ERR)
		sleep (2);
	}

	if (pid == 0) {
	    /* Child.
	     */

	    /* Run the system call.  Let child inherit the parents standard
	     * input unless redirected by nonnull stdin_file.  Set standard
	     * output and error output streams if filenames given, else write
	     * to same files (i.e., terminal) as parent.
	     */
	    if (*sin != EOS) {					/* stdin */
		fd = open (sin, O_RDONLY);
		if (fd == ERR) {
		    fprintf (stderr, "cannot open `%s'\n", sin);
		    _exit (1);
		}
		close (0); dup (fd); close (fd);
	    }

	    if (*sout != EOS) {					/* stdout */
		fd = creat (sout, _u_fmode(FILE_MODEBITS));
		if (fd == ERR)
		    fprintf (stderr, "cannot create `%s'\n", sout);
		else {
		    close (1); dup (fd); close (fd);
		}
	    }

	    if (*serr != EOS) {					/* stderr */
		/* If stdout and stderr are to go to the same file,
		 * dup stdout file descriptor as stderr.
		 */
		if (strcmp (sout, serr) == 0) {
		    close (2); dup (1);
		} else {
		    fd = creat (serr, _u_fmode(FILE_MODEBITS));
		    if (fd == ERR)
			fprintf (stderr, "cannot create `%s'\n", serr);
		    else {
			close (2); dup (fd); close (fd);
		    }
		}
	    }

	    if (getrlimit (RLIMIT_NOFILE, &rlim))
		maxfd = MAXOFILES;
	    else
		maxfd = rlim.rlim_cur;

	    /* Arrange for the local file descriptors of the parent to be closed
	     * in the child if the exec succeeds.  If this is not done the child
	     * may run out of file descriptors.
	     */
	    for (fd=3;  fd < min(MAXOFILES,maxfd);  fd++)
		fcntl (fd, F_SETFD, 1);

	    /* Spawn a shell to execute the command.
	     */

	    /* Setting old_sigint here doesn't make sense if we will be
	     * execl-ing a different process.  Use SIG_DFL instead.
	    signal (SIGINT, old_sigint);
	     */
	    signal (SIGINT, SIG_DFL);

	    execl (shell, shell, "-c", cmd, (char *) 0);

	    /* NOTREACHED (unless execl fails) */
	    _exit (1);
	}

	/* Parent: wait for child to finish up.  Parent process should ignore
	 * interrupts; OS process will handle interrupts and return at the
	 * proper time.  The parent is out of the picture while the OS process
	 * is running (except for the pr_onint interrupt handler, below).
	 */
	pr_enter (pid, 0, 0);
	lastsig = 0;

	*status = pr_wait (pid);

	/* If the OS command was interrupted, ignore its exit status and return
	 * the interrupt exception code to the calling program.  Do not return
	 * the interrupt code unless an interrupt occurs.
	 */
	if (*status == SYS_XINT)
	    *status = 1;
	if (lastsig == SIGINT)
	    *status = SYS_XINT;

	sigaction (SIGINT, &oldact, NULL);

	return (XOK);
}


/* PR_ONINT -- Special interrupt handler for ZOSCMD.  If the OS command is
 * interrupted, post a flag to indicate this to ZOSCMD when the pr_wait()
 * returns.
 */
int
pr_onint (
  int	usig,				/* SIGINT, SIGFPE, etc.		*/
  int	*hwcode,			/* not used */
  int	*scp 				/* not used */
)
{
	lastsig = usig;
	/* return to wait() */

	return (XOK);
}
