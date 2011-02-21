/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYCLEARLN -- Clear the current line.  The cursor is left positioned to
** the left margin.
*/
void
c_ttyclearln (
  XINT	fd,			/* output file			*/
  XINT	tty			/* tty descriptor		*/
)
{
	XINT  x_fd = fd, x_tty = tty;

	TTYCLEARLN (&x_fd, &x_tty);
}
