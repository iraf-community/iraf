/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FLUSH -- FIO file flush.
 */
/* fd : FIO file descriptor */
void c_flush ( int fd )
{
	XINT x_fd = fd;
	FLUSH (&x_fd);
}
