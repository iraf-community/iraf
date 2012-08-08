/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_STROPEN -- FIO string-file open.
*/
int
c_stropen (
  XCHAR	*obuf,			/* string file-buffer	*/
  int	maxch,			/* max chars in string	*/
  int	mode			/* file access mode	*/
)
{
	XINT  x_maxch = maxch, x_mode = mode;
	int   fd;

	iferr (fd = (int) STROPEN (obuf, &x_maxch, &x_mode))
	    return (ERR);
	else
	    return (fd);
}
