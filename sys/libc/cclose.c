/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_CLOSE -- FIO file close.
 */
c_close (fd)
int	fd;			/* FIO file descriptor		*/
{
	iferr (CLOSE (&fd))
	    return (ERR);
	else
	    return (OK);
}
