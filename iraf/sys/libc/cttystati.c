/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYSTATI -- Return the value of a TTY integer interface parameter.
 */
/* tty   : tty descriptor          */
/* param : code of param to be set */
int c_ttystati ( void *tty, int param )
{
	XPOINTER x_tty = (XPOINTER)tty;
	XINT x_param = param;

	return (TTYSTATI (&x_tty, &x_param));
}
