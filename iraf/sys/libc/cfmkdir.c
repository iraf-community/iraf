/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FMKDIR -- FIO procedure to create a new directory.
 */
/* newdir : name of the new directory */
int c_fmkdir ( const char *newdir )
{
	iferr (FMKDIR (c_sppstr(newdir)))
	    return (ERR);
	else
	    return (OK);
}
