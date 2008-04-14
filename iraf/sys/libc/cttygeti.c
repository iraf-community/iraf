/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYGETI -- Get the value of a termcap capability of type integer.
 * Zero is returned if the device does not have such a capability or if
 * the the value string cannot be interpreted as an integer.
 */
/* tty : tty descriptor           */
/* cap : two char capability name */
int c_ttygeti ( void *tty, const char *cap )
{
	XPOINTER x_tty = (XPOINTER)tty;

	return (TTYGETI (&x_tty, c_sppstr(cap)));
}
