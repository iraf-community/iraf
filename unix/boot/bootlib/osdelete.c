/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_DELETE -- Delete a file.
 */
os_delete (fname)
char	*fname;
{
	int	status;

	ZFDELE ((PKCHAR *)vfn2osfn (fname, 0), &status);
	return (status);
}
