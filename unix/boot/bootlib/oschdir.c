/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"


extern int  os_fpathname (char *vfn, char *osfn, int maxch);



/* OS_CHDIR -- Change the current default directory.  Note that the kernel
 * procedure ZFCHDR should only be called with the full pathname of a 
 * directory.
 */
int
os_chdir (char *dir)
{
	XCHAR	dpath[SZ_PATHNAME+1];
	XCHAR	osdir[SZ_PATHNAME+1];
	XINT	sz_dpath, sz_osdir, status, x_maxch=SZ_PATHNAME;

	extern  int ZFXDIR(XCHAR *osfn, XCHAR  *osdir, XINT *maxch, XINT   *nchars);
	extern	int ZFGCWD(PKCHAR  *outstr, XINT *maxch, XINT *status);
	extern	int ZFSUBD(XCHAR *osdir, XINT *maxch, XCHAR *subdir, XINT *nchars);
	extern	int ZFCHDR(PKCHAR  *newdir, XINT *status);


	sz_dpath = os_fpathname (dir, (char *)dpath, SZ_PATHNAME);
	os_strupk ((char *)dpath, osdir, SZ_PATHNAME);
	ZFXDIR (osdir, osdir, &x_maxch, &sz_osdir);

	if (sz_osdir <= 0) {
	    /* Dir is a subdirectory, not a full pathname.  Note that this
	     * only works for an immediate subdirectory, and does not work
	     * for paths relative to the cwd.
	     */
	    ZFGCWD (osdir, &x_maxch, &sz_osdir);
	    os_strupk ((char *)osdir, osdir, SZ_PATHNAME);
	    os_strupk (dir, dpath, SZ_PATHNAME);
	    ZFSUBD (osdir, &x_maxch, dpath, &sz_osdir);
	    os_strpak (osdir, (char *)dpath, SZ_PATHNAME);
	}

	ZFCHDR (dpath, &status);
	return (status);
}
