/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_FSETI -- FIO set integer file parameter.
 */
c_fseti (fd, param, value)
int	fd;			/* FIO file descriptor		*/
int	param;			/* param to be set		*/
int	value;			/* new value			*/
{
	FSETI (&fd, &param, &value);
}
