/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_CLOSE -- FIO file close.
 */
/* fd : FIO file descriptor */
int c_close ( int fd )
{
	XINT x_fd = fd;

	iferr (CLOSE (&x_fd))
	    return (ERR);
	else
	    return (OK);
}
