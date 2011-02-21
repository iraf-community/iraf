/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_FPATHNAME -- Map a VFN (virtual filename) into a pathname (filename
** specification which is independent of the current directory).
*/
int
c_fpathname (
  char	*vfn,			/* virtual filename		*/
  char	*osfn,			/* OS filename			*/
  int	maxch
)
{
	XCHAR	x_osfn[SZ_PATHNAME+1];
	XINT	x_maxch = SZ_PATHNAME;


	/* The OSFN is returned as a packed string in the XCHAR array x_osfn.
	 * An intermediate buffer is used to avoid char->xchar alignment
	 * problems of upward pointer coercion on some machines.
	 */
	iferr (FPATHNAME (c_sppstr(vfn), x_osfn, &x_maxch))
	    osfn[0] = EOS;
	else
	    c_strpak (x_osfn, osfn, maxch);
	
	return (strlen (osfn));
}
