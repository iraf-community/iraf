/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define import_xnames
#include <iraf.h>


/* C_FCHDIR -- Change directory.
*/
int
c_fchdir (char *newdir)
{
	iferr (FCHDIR (c_sppstr (newdir)))
	    return (ERR);
	else
	    return (OK);
}
