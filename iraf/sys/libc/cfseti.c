/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FSETI -- FIO set integer file parameter.
 */
/* fd    : FIO file descriptor */
/* param : param to be set     */
/* value : new value           */
void c_fseti ( int fd, int param, int value )
{
	XINT x_fd = fd;
	XINT x_param = param;
	XINT x_value = value;
	FSETI (&x_fd, &x_param, &x_value);
}
