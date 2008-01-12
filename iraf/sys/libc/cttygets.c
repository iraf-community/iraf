/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

#define SZ_CAPSTR	128
static	XCHAR	buf[SZ_CAPSTR+1];
static	XINT	szbuf = 128;


/* C_TTYGETS -- Get the value of a termcap capability as a character string,
 * suitable for subsequent output to the device with TTYPUTS (assuming the
 * capability is a control function).  The number of characters in the output
 * string is returned as the function value.
 */
/* tty     : tty descriptor           */
/* cap     : two char capability name */
/* outstr  : output string            */
/* bufsize : buffer size of outstr    */
ssize_t c_ttygets ( int tty, const char *cap, char *outstr, size_t bufsize )
{
	XINT nchars;
	XPOINTER x_tty = tty;

	nchars = TTYGETS (&x_tty, c_sppstr(cap), buf, &szbuf);
	c_strpak (buf, outstr, bufsize);

	return (nchars);
}
