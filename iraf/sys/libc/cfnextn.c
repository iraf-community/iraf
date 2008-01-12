/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FNEXTN -- Extract the filename extension substring from a filename.
 */
/* vfn     : filename                    */
/* extn    : filename extension (output) */
/* bufsize : buffer size of extn         */
int c_fnextn ( const char *vfn, char *extn, size_t bufsize )
{
	XCHAR spp_extn[SZ_FNAME+1];
	XINT x_maxch = SZ_FNAME, nchars;

	nchars = FNEXTN (c_sppstr(vfn), spp_extn, &x_maxch);
	c_strpak (spp_extn, extn, bufsize);

	return (nchars);
}
