/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYCTRL -- Lookup a capability in the termcap entry for a device and
 * output the associated control string to the given output file.  The baud
 * rate (extracted from the environment at TTYODES time or set in a C_TTYSETI
 * call) determines the number of pad characters output for delays.  ERR is
 * returned if the control sequence could not be output.
 */
/* fd       : output file              */
/* tty      : tty descriptor           */
/* cap      : two char capability name */
/* afflncnt : number of lines affected */
int c_ttyctrl ( int fd, int tty, const char *cap, int afflncnt )
{
	XINT x_fd = fd;
	XPOINTER x_tty = tty;
	XINT x_afflncnt = afflncnt;

	return (TTYCTRL (&x_fd, &x_tty, c_sppstr(cap), &x_afflncnt));
}
