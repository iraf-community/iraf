/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <fcntl.h>
#include <signal.h>

#define	import_kernel
#define	import_knames
#define	import_error
#define import_spp
#include <iraf.h>

static	int lastsig;
extern	int pr_onint();


/* ZOSCMD -- Send a (machine dependent) command to the host operating
 * system.  If nonnull stdout or stderr filenames are given, try to spool
 * the output in these files.
 */
ZOSCMD (oscmd, stdin_file, stdout_file, stderr_file, status)
PKCHAR	*oscmd;
PKCHAR	*stdin_file, *stdout_file, *stderr_file;
XINT	*status;
{
	PFI	old_sigint;
	char	*shell, *sh = "/bin/sh";
	char	*sin, *sout, *serr, *cmd;
	char	*getenv();
	int	fd, pid;

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

	old_sigint = (PFI) signal (SIGINT, SIG_IGN);

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
		fd = open (sin, 0);
		if (fd == ERR) {
		    fprintf (stderr, "cannot open `%s'\n", sin);
		    _exit (1);
		}
		close (0); dup (fd); close (fd);
	    }

	    if (*sout != EOS) {					/* stdout */
		fd = creat (sout, FILE_MODEBITS);
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
		    fd = creat (serr, FILE_MODEBITS);
		    if (fd == ERR)
			fprintf (stderr, "cannot create `%s'\n", serr);
		    else {
			close (2); dup (fd); close (fd);
		    }
		}
	    }

	    /* Arrange for the local file descriptors of the parent to be closed
	     * in the child if the exec succeeds.  If this is not done the child
	     * may run out of file descriptors.
	     */
	    for (fd=3;  fd < min(MAXOFILES,getdtablesize());  fd++)
		fcntl (fd, F_SETFD, 1);

	    /* Spawn a shell to execute the command.
	     */
	    signal (SIGINT, old_sigint);
	    execl (shell, shell, "-c", cmd, 0);

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
	if (old_sigint != (PFI) SIG_IGN)
	    signal (SIGINT, pr_onint);

	*status = pr_wait (pid);

	/* If the OS command was interrupted, ignore its exit status and return
	 * the interrupt exception code to the calling program.  Do not return
	 * the interrupt code unless an interrupt occurs.
	 */
	if (*status == SYS_XINT)
	    *status = 1;
	if (lastsig == SIGINT)
	    *status = SYS_XINT;

	signal (SIGINT, old_sigint);
}


/* PR_ONINT -- Special interrupt handler for ZOSCMD.  If the OS command is
 * interrupted, post a flag to indicate this to ZOSCMD when the pr_wait()
 * returns.
 */
pr_onint (usig, hwcode, scp)
int	usig;				/* SIGINT, SIGFPE, etc.		*/
int	hwcode;				/* VAX hardware trap/fault codes */
struct	sigcontext *scp;
{
	lastsig = usig;
	/* return to wait() */
}
