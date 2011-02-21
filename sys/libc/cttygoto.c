/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYGOTO -- Move the cursor to the indicated X,Y position (one-indexed).
*/
void
c_ttygoto (
  XINT	fd,			/* output file			*/
  XINT	tty,			/* tty descriptor		*/
  int	col,			/* x coordinate			*/
  int	line			/* y coordinate			*/
)
{
	XINT  x_fd = fd, x_tty = tty, x_col = col, x_line = line;

	TTYGOTO (&x_fd, &x_tty, &x_col, &x_line);
}
