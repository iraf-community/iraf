/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#if (defined(LINUX) || defined(CYGWIN))
#define USE_SIGACTION
#endif

#define import_kernel
#define import_knames
#define import_error
#define import_spp
#include <iraf.h>

#ifdef SYSV
#define vfork	fork
#else
#  ifdef sun
#  include <vfork.h>
#  endif
#endif

#include "zos.h"

#if (defined(MACOSX) && defined(OLD_MACOSX))
void pr_onint ( int, int, struct sigcontext * );
#else
void pr_onint ( int );
#endif

static	int lastsig;

/* ZOSCMD -- Send a (machine dependent) command to the host operating
 * system.  If nonnull stdout or stderr filenames are given, try to spool
 * the output in these files.
 */
int ZOSCMD ( PKCHAR *oscmd, PKCHAR *stdin_file, PKCHAR *stdout_file, 
	     PKCHAR *stderr_file, XINT *status )
{
	const char *shell, *sh = "/bin/sh";
	const char *sin, *sout, *serr, *cmd;
	struct rlimit rlim;
	int maxfd, fd, pid;
#ifdef USE_SIGACTION
	struct sigaction oldact;
#else
	signal_handler_t old_sigint;
#endif

	cmd  = (const char *)oscmd;
	sin  = (const char *)stdin_file;
	sout = (const char *)stdout_file;
	serr = (const char *)stderr_file;

	/* The Bourne shell SH is used if the first character of the cmd
	 * is '!' or if the user does not have SHELL defined in their
	 * environment.
	 */
	if (*cmd == '!') {
	    shell = sh;
	    cmd++;
	} else if ((shell = getenv ("SHELL")) == NULL)
	    shell = sh;

#ifdef USE_SIGACTION
	sigaction (SIGINT, NULL, &oldact);
#else
	old_sigint = signal (SIGINT, SIG_IGN);
#endif

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

#ifndef SYSV
	/* This doesn't appear to work on SysV systems, I suspect that wait()
	 * is not being reentered after the signal handler below.  This could
	 * probably be fixed by modifying the signal handling but I am not
	 * sure the parent needs to intercept errors in any case, so lets
	 * try really ignoring errors in the parent instead, on SYSV systems.
	 */
#ifdef USE_SIGACTION
	if (oldact.sa_handler != SIG_IGN) {
	    struct sigaction sig;
	    sig.sa_handler = &pr_onint;
	    sigemptyset (&sig.sa_mask);
	    sig.sa_flags = SA_NODEFER;
	    sigaction (SIGINT, &sig, NULL);
	}
#else
	if (old_sigint != SIG_IGN)
	    signal (SIGINT, &pr_onint);
#endif
#endif

	*status = pr_wait (pid);

	/* If the OS command was interrupted, ignore its exit status and return
	 * the interrupt exception code to the calling program.  Do not return
	 * the interrupt code unless an interrupt occurs.
	 */
	if (*status == SYS_XINT)
	    *status = 1;
	if (lastsig == SIGINT)
	    *status = SYS_XINT;

#ifdef USE_SIGACTION
	sigaction (SIGINT, &oldact, NULL);
#else
	signal (SIGINT, old_sigint);
#endif

	return *status;
}


/* PR_ONINT -- Special interrupt handler for ZOSCMD.  If the OS command is
 * interrupted, post a flag to indicate this to ZOSCMD when the pr_wait()
 * returns.
 */
#if (defined(MACOSX) && defined(OLD_MACOSX))
void pr_onint ( int usig, int code, struct sigcontext *scp )
#else
void pr_onint ( int usig )
#endif
{
	lastsig = usig;
	/* return to wait() */
}
