/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYSO -- Turn standout mode (reverse video, underline, or upper case
 * depending on the device) on or off.
 */
c_ttyso (fd, tty, onoff)
int	fd;			/* output file			*/
int	tty;			/* tty descriptor		*/
int	onoff;			/* 1=on, 0=off			*/
{
	TTYSO (&fd, &tty, &onoff);
}
