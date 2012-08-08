/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYINIT -- Output the initialization sequence, if any, to the output
** file.
*/
void
c_ttyinit (
  XINT	fd,			/* output file			*/
  XINT	tty			/* tty descriptor		*/
)
{
	XINT  x_fd = fd, x_tty = tty;

	TTYINIT (&x_fd, &x_tty);
}
