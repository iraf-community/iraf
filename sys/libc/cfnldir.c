/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define import_xnames
#include <iraf.h>


/* C_FNLDIR -- Extract the directory prefix substring from a filename.
*/
int
c_fnldir (
  char	*vfn,			/* filename			*/
  char	*ldir,			/* directory prefix (output)	*/
  int	maxch			/* max chars out		*/
)
{
	XCHAR	spp_ldir[SZ_FNAME+1];
	XINT	x_maxch = SZ_FNAME, nchars;

	nchars = FNLDIR (c_sppstr(vfn), spp_ldir, &x_maxch);
	c_strpak (spp_ldir, ldir, maxch);

	return (nchars);
}
