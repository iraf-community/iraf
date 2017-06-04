/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <fcntl.h>

#define	import_kernel
#define	import_knames
#define import_prtype
#define import_spp
#include <iraf.h>

extern	char os_process_name[];		/* process name, set in zmain	*/
extern	PKCHAR osfn_bkgfile[];		/* bkgfile fname if detached	*/
extern	int save_prtype;		/* process type saved by zmain	*/
extern	int debug_sig;


/* ZPANIC -- Unconditionally terminate process.  Normal termination occurs
 * when the IRAF Main returns to the zmain.  We are called if a nasty error
 * occurs in the kernel (a "can't happen" type error) or if an error occurs
 * during error recovery, and error recursion would otherwise result.
 */
int
ZPANIC (
  XINT	  *errcode,		/* integer error code at time of crash	*/
  PKCHAR  *errmsg 		/* packed error message string		*/
)
{
	char	msg[512];
	int	fd;


	/* \nPANIC in `procname': error message\n
	 */
	strcpy (msg, "\n");
	strcat (msg, "PANIC in `");
	strcat (msg, os_process_name);
	strcat (msg, "': ");
	strcat (msg, (char *)errmsg);
	strcat (msg, "\n");

	write (2, msg, strlen(msg));

	/* Echo the error message on the console if we are a bkg process,
	 * in case the user has logged off.
	 */
	if (save_prtype == PR_DETACHED) {
	    fd = open ("/dev/console", 1);
	    if (fd > 0) {
		write (fd, &msg[1], strlen(&msg[1]));
		close (fd);
	    }
	}

	/* Delete the bkgfile if run as a detached process.  Deletion of the
	 * bkgfile signals process termination.
	 */
	if (save_prtype == PR_DETACHED)
	    unlink ((char *)osfn_bkgfile);

	/* Terminate process with a core dump if the debug_sig flag is set.
	 */
	if (debug_sig) {
#ifdef __linux__
	    signal (SIGABRT, SIG_DFL);
	    kill (getpid(), SIGABRT);
#else
	    signal (SIGEMT, SIG_DFL);
	    kill (getpid(), SIGEMT);
#endif
	} else
	    _exit ((int)*errcode);

	return (XOK);
}


/* KERNEL_PANIC -- Called by a kernel routine if a fatal error occurs in the
 * kernel.
 */
int
kernel_panic (char *errmsg)
{
	XINT	errcode = 0;
	PKCHAR	pkmsg[SZ_LINE];
	register char	*ip, *op;

	extern  int ZPANIC();


	/* It is necessary to copy the error message string to get a PKCHAR
	 * type string since misalignment is possible when coercing from char
	 * to PKCHAR.
	 */
	for (ip=errmsg, op=(char *)pkmsg;  (*op++ = *ip++) != EOS;  )
	    ;
	ZPANIC (&errcode, pkmsg);

	return (XOK);
}
