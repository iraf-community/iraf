/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_FREDIR -- FIO redirect an open i/o stream to a named file.  Most commonly
 * used to redirect one of the standard i/o streams to a file.  The named file
 * need not be of the same type as the old stream.
 */
c_fredir (fd, fname, mode, type)
int	fd;			/* stream to be redirected	*/
char	*fname;			/* name of file to be opened	*/
int	mode;			/* access mode			*/
int	type;			/* file type			*/
{
	iferr (FREDIR (&fd, c_sppstr(fname), &mode, &type))
	    return (ERR);
	else
	    return (fd);
}
