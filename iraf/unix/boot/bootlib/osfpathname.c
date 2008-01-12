/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_FPATHNAME -- Map a VFN (virtual filename) into a pathname (filename
 * specification which is independent of the current directory).
 */
/* vfn  : virtual filename		*/
/* osfn : OS filename			*/
int os_fpathname ( const char *vfn, char *osfn, size_t bufsize )
{
	XCHAR	x_vfn[SZ_PATHNAME+1];
	XCHAR	x_osfn[SZ_PATHNAME+1];
	XINT	x_maxch = SZ_PATHNAME, x_nchars;

	if (vfn[0])
	    os_strupk (vfn2osfn(vfn,0), x_vfn, SZ_PATHNAME+1);
	else
	    x_vfn[0] = 0;

	if (vfn[0] == '.' && (vfn[1] == EOS || vfn[2] == EOS)) {
	    ZFGCWD (x_osfn, &x_maxch, &x_nchars);
	    os_strupk ((char *)x_osfn, x_osfn, SZ_PATHNAME+1);
	    if (vfn[1] == '.') {
		os_strupk (vfn, x_vfn, SZ_PATHNAME+1);
		ZFSUBD (x_osfn, &x_maxch, x_vfn, &x_nchars);
	    }
	} else
	    ZFPATH (x_vfn, x_osfn, &x_maxch, &x_nchars);

	os_strpak (x_osfn, osfn, bufsize);
	return (x_nchars);
}
