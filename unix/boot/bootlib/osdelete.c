/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"


/* OS_DELETE -- Delete a file.
 */
int
os_delete (char *fname)
{
	XINT	status;


	ZFDELE ((PKCHAR *)vfn2osfn (fname, 0), &status);
	return (status);
}
