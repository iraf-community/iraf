/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYSETI -- Set a TTY interface parameter of type integer.
 */
/* tty   : tty descriptor          */
/* param : code of param to be set */
/* value : value to be set         */
void c_ttyseti ( int tty, int param, int value )
{
	XPOINTER x_tty = tty;
	XINT x_param = param;
	XINT x_value = value;

	TTYSETI (&x_tty, &x_param, &x_value);
}
