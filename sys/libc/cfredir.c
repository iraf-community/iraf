/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#include <iraf.h>


/* C_FREDIR -- FIO redirect an open i/o stream to a named file.  Most commonly
** used to redirect one of the standard i/o streams to a file.  The named file
** need not be of the same type as the old stream.
*/
int
c_fredir (
  XINT	fd,			/* stream to be redirected	*/
  char	*fname,			/* name of file to be opened	*/
  int	mode,			/* access mode			*/
  int	type			/* file type			*/
)
{
	XINT  x_fd = fd, x_type = type, x_mode = mode;

	iferr (FREDIR (&x_fd, c_sppstr(fname), &x_mode, &x_type))
	    return (ERR);
	else
	    return (fd);
}


/* C_FREDIRO -- FIO redirect an open i/o stream to another open stream.
*/
int
c_frediro (
  XINT	fd,			/* stream to be redirected	*/
  XINT	newfd			/* where it is to be redirected	*/
)
{
	XINT  x_fd = fd, x_newfd = newfd;

	iferr (FREDIRO (&x_fd, &x_newfd))
	    return (ERR);
	else
	    return (fd);
}
