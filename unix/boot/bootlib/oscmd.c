/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

#define	SZ_CMD		2048

/* OS_CMD -- Send a command to the host system.
 */
os_cmd (cmd)
char	*cmd;
{
	PKCHAR	x_cmd[SZ_CMD+1];
	PKCHAR	nullstr[1];
	int	status;

	strncpy ((char *)x_cmd, cmd, SZ_CMD);
	nullstr[0] = 0;

	/* Terminate the parent process if the OS command is interrupted.
	 */
	ZOSCMD (x_cmd, nullstr, nullstr, nullstr, &status);
	return (status);
}
