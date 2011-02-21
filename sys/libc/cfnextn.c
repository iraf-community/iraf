/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define import_xnames
#include <iraf.h>


/* C_FNEXTN -- Extract the filename extension substring from a filename.
*/
int
c_fnextn (
  char	*vfn,			/* filename			*/
  char	*extn,			/* filename extension (output)	*/
  int	maxch			/* max chars out		*/
)
{
	XCHAR	spp_extn[SZ_FNAME+1];
	XINT	x_maxch = SZ_FNAME, nchars;

	nchars = (int) FNEXTN (c_sppstr(vfn), spp_extn, &x_maxch);
	c_strpak (spp_extn, extn, maxch);

	return (nchars);
}
