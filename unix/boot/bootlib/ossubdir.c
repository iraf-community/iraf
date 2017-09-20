/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"


/* OS_SUBDIR -- Fold a subdirectory name into a directory pathname and return
 * a pointer to the pathname of the subdirectory.
 */
char *
os_subdir (
  char	*dir,			/* OS pathname of directory	*/
  char	*subdir 		/* name of subdirectory		*/
)
{
	static	XCHAR x_path[SZ_PATHNAME+1];
	XCHAR	x_subdir[SZ_FNAME+1];
	XINT	x_maxch = SZ_PATHNAME, x_nchars;
	extern  int ZFSUBD(XCHAR *osdir, XINT *maxch, XCHAR *subdir, XINT *nchars);


	os_strupk (dir,    x_path,   SZ_PATHNAME);
	os_strupk (subdir, x_subdir, SZ_FNAME);

	ZFSUBD (x_path, &x_maxch, x_subdir, &x_nchars);

	if (x_nchars > 0)
	    return (os_strpak (x_path, (char *)x_path, SZ_PATHNAME));
	else
	    return (NULL);
}
