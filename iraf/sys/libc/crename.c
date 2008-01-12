/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_RENAME -- FIO rename file.
 */
/* old_fname : current name of file */
/* new_fname : new file name        */
int c_rename ( const char *old_fname, const char *new_fname )
{
	XCHAR	spp_new_fname[SZ_FNAME];
	XINT	bufsize = SZ_FNAME;

	c_strupk (new_fname, spp_new_fname, bufsize);
	iferr (RENAME (c_sppstr(old_fname), spp_new_fname))
	    return (ERR);
	else
	    return (OK);
}
