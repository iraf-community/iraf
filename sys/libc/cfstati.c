/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_FSTATI -- FIO get integer file parameter.
 */
c_fstati (fd, param)
int	fd;			/* FIO file descriptor		*/
int	param;			/* param to be queried		*/
{
	return (FSTATI (&fd, &param));
}
