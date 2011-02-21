/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_OPEN -- FIO file open.
*/
int
c_open (
  char	*fname,			/* name of file to be opened	*/
  int	mode,			/* access mode			*/
  int	type			/* file type			*/
)
{
	int	fd;
	XINT  x_mode = mode, x_type = type;

	iferr (fd = (int) OPEN (c_sppstr(fname), &x_mode, &x_type))
	    return (ERR);
	else
	    return (fd);
}
