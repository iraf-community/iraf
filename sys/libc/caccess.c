/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_ACCESS -- FIO file access.
 */
c_access (fname, mode, type)
char	*fname;			/* name of file to be accessed	*/
int	mode;			/* access mode			*/
int	type;			/* file type			*/
{
	return (ACCESS (c_sppstr(fname), &mode, &type));
}
