/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_FMAPFN -- Map a VFN (virtual filename) into an OSFN (host system filename).
*/
int
c_fmapfn (
  char	*vfn,			/* virtual filename		*/
  char	*osfn,			/* OS filename			*/
  int	maxch
)
{
	XCHAR	x_osfn[SZ_PATHNAME+1];
	XINT	sz_path = SZ_PATHNAME;


	/* The OSFN is returned as a packed string in the XCHAR array x_osfn.
	 * An intermediate buffer is used to avoid char->xchar alignment
	 * problems of upward pointer coercion on some machines.
	 */
	if (maxch)
	    iferr (FMAPFN (c_sppstr(vfn), x_osfn, &sz_path))
		osfn[0] = EOS;
	    else {
		(void) strncpy (osfn, (char *)x_osfn, maxch);
		osfn[maxch-1] = EOS;
	    }
	
	return (strlen (osfn));
}
