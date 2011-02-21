/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_FLUSH -- FIO file flush.
*/
void
c_flush (
  XINT	fd			/* FIO file descriptor		*/
)
{
	XINT  x_fd = fd;

	FLUSH (&x_fd);
}
