/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYGETB -- Determine if the named capability exists for a device.
 * Presence of the capability in the termcap entry for the device results
 * in a return value of YES (=1), regardless of the actual datatype of
 * the parameter.
 */
/* tty : tty descriptor           */
/* cap : two char capability name */
int c_ttygetb ( int tty, const char *cap )
{
	XPOINTER x_tty = tty;
	XBOOL x_ret;

	x_ret = TTYGETB (&x_tty, c_sppstr(cap));
	return (BTOI (&x_ret));
}
