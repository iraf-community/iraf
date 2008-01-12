/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYCLEAR -- Clear the screen.
 */
/* fd  : output file    */
/* tty : tty descriptor */
void c_ttyclear ( int fd, int tty )
{
	XINT x_fd = fd;
	XPOINTER x_tty = tty;

	TTYCLEAR (&x_fd, &x_tty);
}
