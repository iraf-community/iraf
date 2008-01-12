/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FSTATI -- FIO get integer file parameter.
 */
/* fd    : FIO file descriptor */
/* param : param to be queried */
int c_fstati ( int fd, int param )
{
	XINT x_fd = fd;
	XINT x_param = param;
	return (FSTATI (&x_fd, &x_param));
}
