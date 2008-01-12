/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYSO -- Turn standout mode (reverse video, underline, or upper case
 * depending on the device) on or off.
 */
/* fd    : output file    */
/* tty   : tty descriptor */
/* onoff : 1=on, 0=off    */
void c_ttyso ( int fd, int tty, int onoff )
{
	XINT x_fd = fd;
	XPOINTER x_tty = tty;
	XINT x_onoff = onoff;

	TTYSO (&x_fd, &x_tty, &x_onoff);
}
