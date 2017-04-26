/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <signal.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZINTPR -- Interrupt a connected subprocess, i.e., raise the exception X_INT
 * in the subprocess.  On the UNIX system subprocesses ignore the UNIX SIGINT
 * exception, hence we send SIGTERM instead and the exception handling code
 * maps both to X_INT.
 */
int
ZINTPR (
  XINT	*pid,
  XINT	*exception,		/* not used at present */
  XINT	*status 
)
{
	if (kill (*pid, SIGTERM) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return (*status);
}
