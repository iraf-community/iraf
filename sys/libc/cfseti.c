/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_FSETI -- FIO set integer file parameter.
*/
void
c_fseti (
  XINT	fd,			/* FIO file descriptor		*/
  int	param,			/* param to be set		*/
  int	value			/* new value			*/
)
{
	XINT  x_fd = fd,  x_param = param,  x_value = value;

	FSETI (&x_fd, &x_param, &x_value);
}
