/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYCLEAR -- Clear the screen.
*/
void
c_ttyclear (
  XINT	fd,			/* output file			*/
  XINT	tty			/* tty descriptor		*/
)
{
	XINT  x_fd = fd, x_tty = tty;

	TTYCLEAR (&x_fd, &x_tty);
}
