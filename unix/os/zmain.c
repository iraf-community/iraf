#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#define import_spp
#define	import_kernel
#define import_prtype
#define	import_knames
#define	import_xnames
#include <iraf.h>

extern	unsigned USHLIB[];
#define	sh_debug USHLIB[2]

static	char os_process_name[SZ_FNAME];
static	char osfn_bkgfile[SZ_PATHNAME];
static	ipc_isatty = NO;
static	int prtype;


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
 *	-w			permit writing into shared image (debugging)
 */
main (argc, argv)
int	argc;
char	*argv[];
{
	XINT	inchan=0, outchan=1;	/* process stdin, stdout	*/
	XINT	errchan=2;		/* process std error output	*/
	XINT	driver;			/* EPA i/o chan device driver	*/
	XINT	devtype;		/* device type (text or binary)	*/
	XINT	jobcode;		/* bkg jobcode, if detached pr	*/
	XINT	wsetsize=0L, junk;	/* for ZAWSET			*/
	int	len_irafcmd, nchars;
	XCHAR	*irafcmd;
	char	*ip;

	int	arg = 1;
#ifdef apollo
 	int	(*epaptr)(), (*epaptr2)();
#endif
	int	ZGETTX(), ZGETTY(), ZARDPR(), SYSRUK(), ONENTRY();

	/* The following flag must be set before calling ZZSTRT. */
	if (argc > 1 && strcmp (argv[arg], "-w") == 0) {
	    sh_debug++;
	    arg++;
	}

	ZZSTRT();

	strcpy (os_process_name, argv[0]);
	strcpy ((char *)osfn_bkgfile, "");

	/* Determine process type.  If we were spawned by the host the TTY
	 * driver is used regardless of whether the standard i/o device is
	 * a tty or a file.  Otherwise the IPC driver is used.  If we are a
	 * detached process the standard input is connected to /dev/null,
	 * which will cause the IPC driver to return EOF if a task tries to
	 * read from stdin.
	 */

        /* Default if no -cCd process type flags. */
	prtype = PR_HOST;

/* In Domain/OS, referencing an argument in a procedure that was declared
 * int ROUTINE() in the caller results in a segmentation violation.  This
 * was the problem that broke both debuggers and the domain kernel when such
 * a process was debugged on a dn3500 with fpa under sr10.1.
 */
#ifdef apollo
 	epaptr = ZGETTY;
 	ZLOCPR (&epaptr, &driver);
#else
	ZLOCPR (ZGETTY, &driver);
#endif
	devtype = TEXT_FILE;

	if (arg < argc) {
	    if (strncmp (argv[arg], "-C", 2) == 0) {
		ipc_isatty = 1;
		arg++;
		goto ipc_;

	    } else if (strncmp (argv[arg], "-c", 2) == 0) {
		/* Disable SIGINT so that child process does not die when the
		 * parent process is interrupted.  Parent sends SIGTERM to
		 * interrupt a child process.
		 */
		signal (SIGINT, SIG_IGN);
		arg++;
ipc_:
		prtype = PR_CONNECTED;
#ifdef apollo
 		epaptr = ZARDPR;
 		ZLOCPR (&epaptr, &driver);
#else
		ZLOCPR (ZARDPR, &driver);
#endif
		devtype = BINARY_FILE;

	    } else if (strncmp (argv[arg], "-d", 2) == 0) {
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
		setpgrp (0, jobcode);

		freopen ("/dev/null", "r", stdin);
		prtype = PR_DETACHED;
#ifdef apollo
 		epaptr = ZGETTX;
 		ZLOCPR (&epaptr, &driver);
#else
		ZLOCPR (ZGETTX, &driver);
#endif
		devtype = TEXT_FILE;

		/* Copy the bkgfile to PKCHAR buffer to avoid the possibility
		 * that argv[2] is not PKCHAR aligned.
		 */
		strcpy ((char *)osfn_bkgfile, argv[arg]);
		arg++;

	    } else if (argv[arg][0] == '-') {
		fprintf (stderr,
		    "usage: task [-c | -C | -d bkgfile] [irafcmd...]");
		exit (1);
	    }
	}

	len_irafcmd = SZ_LINE;
	irafcmd = (XCHAR *) malloc (len_irafcmd * sizeof(XCHAR));

	/* If there are any additional arguments on the command line pass
	 * these on to the IRAF main as the IRAF command to be executed.
	 */
	if (arg < argc) {
	    for (nchars=0;  arg < argc;  arg++) {
		while (nchars + strlen(argv[arg]) > len_irafcmd) {
		    len_irafcmd += 1024;
		    irafcmd = (XCHAR *) realloc ((char *)irafcmd,
			len_irafcmd * sizeof(XCHAR));
		}
		for (ip=argv[arg];  (irafcmd[nchars] = *ip++);  nchars++)
		    ;
		irafcmd[nchars++] = ' ';
	    }

	    irafcmd[nchars?nchars-1:0] = XEOS;
	} else
	    irafcmd[0] = XEOS;

	/* Pass some parameters into the kernel; avoid a global reference to
	 * the actual external parmeters (which may be in a shared library
	 * and hence inaccessible).
	 */
	ZZSETK (os_process_name, osfn_bkgfile, prtype, ipc_isatty);

	/* Call the IRAF Main, which does all the real work.  Return status
	 * OK when the main returns.  The process can return an error status
	 * code only in the event of a panic.
	 */
#ifdef apollo
 	epaptr = SYSRUK;
 	epaptr2 = ONENTRY;
	IRAF_MAIN (irafcmd, &inchan, &outchan, &errchan, &driver,
	    &devtype, &prtype, osfn_bkgfile, &jobcode, &epaptr, &epaptr2);
#else
	IRAF_MAIN (irafcmd, &inchan, &outchan, &errchan,
	    &driver, &devtype, &prtype, osfn_bkgfile, &jobcode, SYSRUK,ONENTRY);
#endif

	/* Normal process shutdown.  Our only action is to delete the bkgfile
	 * if run as a detached process (see also zpanic).
	 */
	if (prtype == PR_DETACHED)
	    unlink ((char *)osfn_bkgfile);

	exit (XOK);
}
