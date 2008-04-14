/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYPUTS -- Put a control sequence obtained in a prior call to C_TTYGETS
 * to the output file.  The baud rate (extracted from the environment at
 * TTYODES time or set in a C_TTYSETI call) determines the number of pad
 * characters output for delays.
 */
/* fd       : output file              */
/* tty      : tty descriptor           */
/* cap      : two char capability name */
/* afflncnt : number of lines affected */
int c_ttyputs ( int fd, void *tty, const char *cap, int afflncnt )
{
	XINT x_fd = fd;
	XPOINTER x_tty = (XPOINTER)tty;
	XINT x_afflncnt = afflncnt;

	iferr (TTYPUTS (&x_fd, &x_tty, c_sppstr(cap), &x_afflncnt))
	    return (ERR);
	else
	    return (OK);
}
