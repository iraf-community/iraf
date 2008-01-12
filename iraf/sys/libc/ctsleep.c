/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TSLEEP -- Suspend process execution for the specified number of seconds.
 */
void c_tsleep ( int nseconds )
{
	XINT x_nseconds = nseconds;
	TSLEEP (&x_nseconds);
}
