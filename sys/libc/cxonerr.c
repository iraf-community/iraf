/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#define import_xnames
#include <iraf.h>

/* C_XONERR -- Call any error handler procedures posted with ONERROR.
 */
c_xonerr (errcode)
int	errcode;
{
	XONERR (&errcode);
}
