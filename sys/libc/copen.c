/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_OPEN -- FIO file open.
 */
c_open (fname, mode, type)
char	*fname;			/* name of file to be opened	*/
int	mode;			/* access mode			*/
int	type;			/* file type			*/
{
	int	fd;

	iferr (fd = OPEN (c_sppstr(fname), &mode, &type))
	    return (ERR);
	else
	    return (fd);
}
