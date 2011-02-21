/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_DELETE -- FIO delete file.
*/
int
c_delete (
  char	*fname			/* name of file to be opened	*/
)
{
	iferr (DELETE (c_sppstr(fname)))
	    return (ERR);
	else
	    return (OK);
}
