/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYCLEARLN -- Clear the current line.  The cursor is left positioned to
 * the left margin.
 */
/* fd  : output file    */
/* tty : tty descriptor */
void c_ttyclearln ( int fd, int tty )
{
	XINT x_fd = fd;
	XPOINTER x_tty = tty;

	TTYCLEARLN (&x_fd, &x_tty);
}
