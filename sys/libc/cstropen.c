/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_STROPEN -- FIO string-file open.
 */
c_stropen (obuf, maxch, mode)
XCHAR	*obuf;			/* string file-buffer	*/
int	maxch;			/* max chars in string	*/
int	mode;			/* file access mode	*/
{
	int	fd;

	iferr (fd = STROPEN (obuf, &maxch, &mode))
	    return (ERR);
	else
	    return (fd);
}
