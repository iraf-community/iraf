/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYINIT -- Output the initialization sequence, if any, to the output
 * file.
 */
/* fd  : output file    */
/* tty : tty descriptor */
void c_ttyinit ( int fd, int tty )
{
	XINT x_fd = fd;
	XPOINTER x_tty = tty;

	TTYINIT (&x_fd, &x_tty);
}
