/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define import_xnames
#include <iraf.h>

/* C_FNROOT -- Extract the root filename substring from a filename.
*/
int
c_fnroot (
  char	*vfn,			/* filename			*/
  char	*root,			/* root filename (output)	*/
  int	maxch			/* max chars out		*/
)
{
	XCHAR	spp_root[SZ_FNAME+1];
	XINT	x_maxch = SZ_FNAME, nchars;

	nchars = FNROOT (c_sppstr(vfn), spp_root, &x_maxch);
	c_strpak (spp_root, root, maxch);

	return (nchars);
}
