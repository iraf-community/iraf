/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"


/* OS_FPATHNAME -- Map a VFN (virtual filename) into a pathname (filename
 * specification which is independent of the current directory).
 */
int
os_fpathname (
  char	*vfn,			/* virtual filename		*/
  char	*osfn,			/* OS filename			*/
  int	maxch 
)
{
	XCHAR	x_vfn[SZ_PATHNAME+1];
	XCHAR	x_osfn[SZ_PATHNAME+1];
	XINT	x_maxch = SZ_PATHNAME, x_nchars;

	extern  int ZFGCWD(PKCHAR *outstr, XINT *maxch, XINT *status);
	extern	int ZFSUBD(XCHAR  *osdir, XINT *maxch, XCHAR *subdir, XINT *nchars);
	extern	int ZFPATH(XCHAR *osfn, XCHAR *pathname, XINT *maxch, XINT  *nchars);


	if (vfn[0])
	    os_strupk (vfn2osfn(vfn,0), x_vfn, x_maxch);
	else
	    x_vfn[0] = 0;

	if (vfn[0] == '.' && (vfn[1] == EOS || vfn[2] == EOS)) {
	    ZFGCWD (x_osfn, &x_maxch, &x_nchars);
	    os_strupk ((char *)x_osfn, x_osfn, x_maxch);
	    if (vfn[1] == '.') {
		os_strupk (vfn, x_vfn, x_maxch);
		ZFSUBD (x_osfn, &x_maxch, x_vfn, &x_nchars);
	    }
	} else
	    ZFPATH (x_vfn, x_osfn, &x_maxch, &x_nchars);

	os_strpak (x_osfn, osfn, maxch);
	return (x_nchars);
}
