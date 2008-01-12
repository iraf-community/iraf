/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_SUBDIR -- Fold a subdirectory name into a directory pathname and return
 * a pointer to the pathname of the subdirectory.
 */
/* dir    : OS pathname of directory	*/
/* subdir : name of subdirectory	*/
const char *os_subdir ( const char *dir, const char *subdir )
{
	static	XCHAR x_path[SZ_PATHNAME+1];
	XCHAR	x_subdir[SZ_FNAME+1];
	XINT	x_maxch = SZ_PATHNAME, x_nchars;

	os_strupk (dir,    x_path,   SZ_PATHNAME+1);
	os_strupk (subdir, x_subdir, SZ_FNAME+1);

	ZFSUBD (x_path, &x_maxch, x_subdir, &x_nchars);

	if (x_nchars > 0)
	    return (os_strpak (x_path, (char *)x_path, SZ_PATHNAME+1));
	else
	    return (NULL);
}
