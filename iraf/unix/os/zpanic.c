/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>

#define import_kernel
#define import_knames
#define import_prtype
#define import_spp
#include <iraf.h>

#include "zos.h"

/* ZPANIC -- Unconditionally terminate process.  Normal termination occurs
 * when the IRAF Main returns to the zmain.  We are called if a nasty error
 * occurs in the kernel (a "can't happen" type error) or if an error occurs
 * during error recovery, and error recursion would otherwise result.
 */
int ZPANIC ( XINT *errcode, PKCHAR *errmsg )
/* errcode : integer error code at time of crash	*/
/* errmsg  : packed error message string		*/
{
	char	msg[512];
	int	fd;

	/* \nPANIC in `procname': error message\n
	 */
	strcpy (msg, "\n");
	strcat (msg, "PANIC in `");
	safe_strcat (msg, 512, os_process_name);
	safe_strcat (msg, 512, "': ");
	safe_strcat (msg, 512, (const char *)errmsg);
	safe_strcat (msg, 512, "\n");

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
	    unlink ((const char *)osfn_bkgfile);

	/* Terminate process with a core dump if the debug_sig flag is set.
	 */
	if (debug_sig) {
#ifdef LINUX
	    signal (SIGABRT, SIG_DFL);
	    kill (getpid(), SIGABRT);
#else
	    signal (SIGEMT, SIG_DFL);
	    kill (getpid(), SIGEMT);
#endif
	} else
	    _exit ((int)*errcode);

	return XOK;
}


/* KERNEL_PANIC -- Called by a kernel routine if a fatal error occurs in the
 * kernel.
 */
int kernel_panic ( const char *errmsg )
{
	XINT errcode = 0;
	PKCHAR pkmsg[SZ_LINE];
	const char *ip;
	char *op, *maxop;

	/* It is necessary to copy the error message string to get a PKCHAR
	 * type string since misalignment is possible when coercing from char
	 * to PKCHAR.
	 */
	maxop = (char *)pkmsg + SZ_LINE -1;
	ip = errmsg;
	for ( op=(char *)pkmsg ; op < maxop && *ip != EOS ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	ZPANIC (&errcode, pkmsg);

	return 0;
}
