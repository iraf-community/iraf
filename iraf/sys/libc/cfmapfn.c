/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FMAPFN -- Map a VFN (virtual filename) into an OSFN (host system filename).
 */
/* vfn  : virtual filename */
/* osfn : OS filename      */
int c_fmapfn ( const char *vfn, char *osfn, size_t bufsize )
{
	XCHAR	x_osfn[SZ_PATHNAME+1];
	XINT	sz_path = SZ_PATHNAME;

	/* The OSFN is returned as a packed string in the XCHAR array x_osfn.
	 * An intermediate buffer is used to avoid char->xchar alignment
	 * problems of upward pointer coercion on some machines.
	 */
	if (bufsize) {
	    iferr (FMAPFN (c_sppstr(vfn), x_osfn, &sz_path))
		osfn[0] = EOS;
	    else {
		strncpy (osfn, (char *)x_osfn, bufsize);
		osfn[bufsize-1] = EOS;
	    }
	}
	
	return (strlen (osfn));
}
