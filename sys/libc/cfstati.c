/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_FSTATI -- FIO get integer file parameter.
*/
int
c_fstati (
  XINT	fd,			/* FIO file descriptor		*/
  int	param			/* param to be queried		*/
)
{
	XINT  x_fd = fd,  x_param = param;

	return (FSTATI (&x_fd, &x_param));
}
