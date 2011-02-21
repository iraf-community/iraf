/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYSO -- Turn standout mode (reverse video, underline, or upper case
** depending on the device) on or off.
*/
void
c_ttyso (
  XINT	fd,			/* output file			*/
  XINT	tty,			/* tty descriptor		*/
  int	onoff			/* 1=on, 0=off			*/
)
{
	XINT  x_fd = fd, x_tty = tty, x_onoff = onoff;

	TTYSO (&x_fd, &x_tty, &x_onoff);
}
