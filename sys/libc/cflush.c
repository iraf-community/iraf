/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_FLUSH -- FIO file flush.
 */
c_flush (fd)
int	fd;			/* FIO file descriptor		*/
{
	FLUSH (&fd);
}
