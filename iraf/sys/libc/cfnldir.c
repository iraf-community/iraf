/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FNLDIR -- Extract the directory prefix substring from a filename.
 */
/* vfn     : filename                  */
/* ldir    : directory prefix (output) */
/* bufsize : buffer size of ldir       */
int c_fnldir ( const char *vfn, char *ldir, size_t bufsize )
{
	XCHAR spp_ldir[SZ_FNAME+1];
	XINT x_maxch = SZ_FNAME, nchars;

	nchars = FNLDIR (c_sppstr(vfn), spp_ldir, &x_maxch);
	c_strpak (spp_ldir, ldir, bufsize);

	return (nchars);
}
