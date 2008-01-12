/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_knames
#include <iraf.h>

/* SYSTEM -- Send a command to the host system.  OK is returned if the command
 * executes properly, else a positive integer error code identifying the error
 * which occurred.
 */
/* cmd : command to be sent to host system */
int system ( const char *cmd )
{
	PKCHAR	nullstr[1];
	XINT	status;

	nullstr[0] = EOS;
	ZOSCMD ((PKCHAR *)cmd, nullstr, nullstr, nullstr, &status);

	return (status);
}
