/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_CHDIR -- Change the current default directory.  Note that the kernel
 * procedure ZFCHDR should only be called with the full pathname of a 
 * directory.
 */
int os_chdir ( const char *dir )
{
	XCHAR	dpath[SZ_PATHNAME+1];
	XCHAR	osdir[SZ_PATHNAME+1];
	int	sz_dpath;
	XINT	sz_osdir, status, x_maxch=SZ_PATHNAME;

	sz_dpath = os_fpathname (dir, (char *)dpath, SZ_PATHNAME+1);
	os_strupk ((const char *)dpath, osdir, SZ_PATHNAME+1);
	ZFXDIR (osdir, osdir, &x_maxch, &sz_osdir);

	if (sz_osdir <= 0) {
	    /* Dir is a subdirectory, not a full pathname.  Note that this
	     * only works for an immediate subdirectory, and does not work
	     * for paths relative to the cwd.
	     */
	    ZFGCWD (osdir, &x_maxch, &sz_osdir);
	    os_strupk ((const char *)osdir, osdir, SZ_PATHNAME+1);
	    os_strupk (dir, dpath, SZ_PATHNAME+1);
	    ZFSUBD (osdir, &x_maxch, dpath, &sz_osdir);
	    os_strpak (osdir, (char *)dpath, SZ_PATHNAME+1);
	}

	ZFCHDR (dpath, &status);
	return (status);
}
