/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_CLOSE -- FIO file close.
*/
int
c_close (
  XINT	fd			/* FIO file descriptor		*/
)
{
	XINT  x_fd = fd;

	iferr (CLOSE (&x_fd))
	    return (ERR);
	else
	    return (OK);
}
