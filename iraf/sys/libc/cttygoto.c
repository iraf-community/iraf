/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYGOTO -- Move the cursor to the indicated X,Y position (one-indexed).
 */
/* fd   : output file    */
/* tty  : tty descriptor */
/* col  : x coordinate   */
/* line : y coordinate   */
void c_ttygoto ( int fd, int tty, int col, int line )
{
	XINT x_fd = fd;
	XPOINTER x_tty = tty;
	XINT x_col = col;
	XINT x_line = line;

	TTYGOTO (&x_fd, &x_tty, &x_col, &x_line);
}
