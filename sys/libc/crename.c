/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_RENAME -- FIO rename file.
 */
c_rename (old_fname, new_fname)
char	*old_fname;		/* current name of file		*/
char	*new_fname;		/* new file name		*/
{
	XCHAR	spp_new_fname[SZ_FNAME];
	XINT	maxch = SZ_FNAME;

	c_strupk (new_fname, spp_new_fname, maxch);
	iferr (RENAME (c_sppstr(old_fname), spp_new_fname))
	    return (ERR);
	else
	    return (OK);
}
