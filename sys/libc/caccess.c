/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_ACCESS -- FIO file access.
*/
int 
c_access (
  char	*fname,			/* name of file to be accessed	*/
  int	mode,			/* access mode			*/
  int	type			/* file type			*/
)
{
	XINT x_mode = mode, x_type = type;

	return (ACCESS (c_sppstr(fname), &x_mode, &x_type));
}
