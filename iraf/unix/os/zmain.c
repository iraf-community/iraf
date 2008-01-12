/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#define import_spp
#define import_kernel
#define import_prtype
#define import_knames
#define import_xnames
#include <iraf.h>

#include "zos.h"

/*
 * ZMAIN.C -- C main for IRAF processes.
 */

#define LOGIPC	"LOGIPC"		/* define to enable IPC logging. */

static	XCHAR x_os_process_name[SZ_FNAME];
static	XCHAR x_osfn_bkgfile[SZ_PATHNAME];
static	XINT x_ipc_in = 0, x_ipc_out = 0;
static	XINT x_ipc_isatty = NO;
static	XINT x_prtype;


/* MAIN -- UNIX Main routine for IRAF processes.  The process is a C process
 * to UNIX, even though nearly all the code is Fortran.  The process main
 * determines whether the process is a connected subprocess, a detached
 * process, or a process spawned by the host system.  We must set up the
 * process standard i/o channels then call the IRAF Main to complete process
 * initialization.  Control returns when the IRAF Main shuts down the process
 * in response to a BYE request.  The only other way a process can exit is
 * if a panic abort occurs.
 *
 * The following switches are recognized:
 *	-C			debug -c (IPC) protocol from a terminal
 *	-c			connected subprocess
 *	-d bkgfile		detached subprocess
 *	-h			host process (default)
 *	-w			permit writing into shared image (debugging)
 */
int main ( int argc, char *argv[] )
{
	XINT inchan=0, outchan=1;	/* process stdin, stdout	*/
	XINT errchan=2;			/* process std error output	*/
	XINT driver;			/* EPA i/o chan device driver	*/
	XINT devtype;			/* device type (text or binary)	*/
	XINT jobcode;			/* bkg jobcode, if detached pr	*/
	XINT errstat;
	int len_irafcmd, nchars;
	XCHAR *irafcmd;
	const char *ip;

	int arg = 1;

	if ( sizeof(XINT) < sizeof(void *) ) {
	    fprintf(stderr,"[ERROR] ZLOCPR:\n");
	    fprintf(stderr,"The sizeof(XINT) is not equal to the address size.\n");
	    fprintf(stderr,"Sorry, the IRAF cannot work on your machine.\n");
	    exit (1);
	}

	/* The following flag must be set before calling ZZSTRT. */
	if (argc > 1 && strcmp (argv[arg], "-w") == 0) {
	    sh_debug++;
	    arg++;
	}

	ZZSTRT();

	safe_strcpy ((char *)x_os_process_name, SZ_FNAME, argv[0]);
	safe_strcpy ((char *)x_osfn_bkgfile, SZ_PATHNAME, "");

	/* Determine process type.  If we were spawned by the host the TTY
	 * driver is used regardless of whether the standard i/o device is
	 * a tty or a file.  Otherwise the IPC driver is used.  If we are a
	 * detached process the standard input is connected to /dev/null,
	 * which will cause the IPC driver to return EOF if a task tries to
	 * read from stdin.
	 */

        /* Default if no arguments (same as -h, or host process). */
	x_prtype = PR_HOST;
	ZLOCPR ((PFU)(&ZGETTY), &driver);
	devtype = TEXT_FILE;

	if (arg < argc) {
	    if (strcmp (argv[arg], "-C") == 0) {
		x_ipc_isatty = 1;
		arg++;
		goto ipc_;

	    } else if (strcmp (argv[arg], "-c") == 0) {
		/* Disable SIGINT so that child process does not die when the
		 * parent process is interrupted.  Parent sends SIGTERM to
		 * interrupt a child process.
		 */
		signal (SIGINT, SIG_IGN);
		arg++;

		/* Check if we want IPC debug logging. */
		if (getenv (LOGIPC)) {
		    char   fname[SZ_FNAME];

		    snprintf (fname, SZ_FNAME, "%d.in", getpid());
		    x_ipc_in = creat (fname, 0644);
		    snprintf (fname, SZ_FNAME, "%d.out", getpid());
		    x_ipc_out = creat (fname, 0644);
		}

ipc_:
		x_prtype = PR_CONNECTED;
		ZLOCPR ((PFU)(&ZARDPR), &driver);
		devtype = BINARY_FILE;

	    } else if (strcmp (argv[arg], "-d") == 0) {
		signal (SIGINT,  SIG_IGN);
		signal (SIGTSTP, SIG_IGN);
		arg++;

		/* Put this background process in its own process group,
		 * so that it will be unaffected by signals sent to the
		 * parent's process group, and to prevent the detached process
		 * from trying to read from the parent's terminal.
		 * [Sun/IRAF Note - this is necessary to prevent SunView from
		 * axeing bkg jobs when "Exit Suntools" is selected from the
		 * root menu].
		 */
		jobcode = getpid();
#ifdef SYSV
		setpgrp ();
#else
		setpgrp (0, jobcode);
#endif

		freopen ("/dev/null", "r", stdin);
		x_prtype = PR_DETACHED;
		ZLOCPR ((PFU)(&ZGETTX), &driver);
		devtype = TEXT_FILE;

		/* Copy the bkgfile to PKCHAR buffer to avoid the possibility
		 * that argv[2] is not PKCHAR aligned.
		 */
		safe_strcpy ((char *)x_osfn_bkgfile, SZ_PATHNAME, argv[arg]);
		arg++;

	    } else if (strcmp (argv[arg], "-h") == 0) {
		/* Default case. */
		arg++;
	    }
	}

	len_irafcmd = SZ_LINE;
	irafcmd = (XCHAR *) malloc (len_irafcmd * sizeof(XCHAR));
	if ( irafcmd == NULL ) {
	    fprintf(stderr,"[ERROR] main: malloc() failed\n");
	    exit (1);
	}

	/* If there are any additional arguments on the command line pass
	 * these on to the IRAF main as the IRAF command to be executed.
	 */
	if (arg < argc) {
	    for (nchars=0;  arg < argc;  arg++) {
		while ( len_irafcmd < nchars + strlen(argv[arg]) + 1 ) {
		    void *tmp_ptr;
		    len_irafcmd += 1024;
		    tmp_ptr = realloc (irafcmd, len_irafcmd * sizeof(XCHAR));
		    if ( tmp_ptr == NULL ) {
			fprintf(stderr,"[ERROR] main: realloc() failed\n");
			exit (1);
		    }
		    irafcmd = (XCHAR *)tmp_ptr;
		}
		for ( ip=argv[arg] ; (*ip) ; ip++, nchars++ )
		    irafcmd[nchars] = *ip;
		irafcmd[nchars++] = ' ';
	    }

	    irafcmd[nchars?nchars-1:0] = XEOS;
	} else
	    irafcmd[0] = XEOS;

	/* Pass some parameters into the kernel; avoid a global reference to
	 * the actual external parmeters (which may be in a shared library
	 * and hence inaccessible).
	 */
	ZZSETK (x_os_process_name, x_osfn_bkgfile, &x_prtype,
	    &x_ipc_isatty, &x_ipc_in, &x_ipc_out);

	/* Call the IRAF Main, which does all the real work.  Return status
	 * OK when the main returns.  The process can return an error status
	 * code only in the event of a panic.
	 */
	errstat = IRAF_MAIN (irafcmd, &inchan, &outchan, &errchan,
			     &driver, &devtype, &x_prtype, x_osfn_bkgfile, &jobcode, 
			     (PFI)(&SYSRUK), (PFI)(&ONENTRY));

	/* Normal process shutdown.  Our only action is to delete the bkgfile
	 * if run as a detached process (see also zpanic).
	 */
	if (x_prtype == PR_DETACHED)
	    unlink ((const char *)x_osfn_bkgfile);

	exit (errstat);
}
