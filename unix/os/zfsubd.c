/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#define import_spp
#define	import_kernel
#define import_knames
#include <iraf.h>

/* ZFSUBD -- Fold a subdirectory into an OS directory.  If osdir is null the
 * current directory is assumed.  If subdir is null osdir is modified as
 * necessary to make it a legal directory prefix, e.g., if osdir = "/usr/iraf"
 * and subdir = "", we would return osdir = "/usr/iraf/".  The subdirectory
 * "." refers to the current directory and ".." to the previous directory,
 * but this is consistent with UNIX so we need not recognize these as special
 * cases.  The return OS directory prefix need not be an absolute pathname.
 */
int
ZFSUBD (
  XCHAR	 *osdir,		/* pathname		[NOT PACKED]	*/
  XINT	 *maxch,		/* max xchars in osdir			*/
  XCHAR	 *subdir,		/* subdirectory name	[NOT PACKED]	*/
  XINT	 *nchars		/* receives lenght of osdir		*/
)
{
	register XCHAR	*ip, *op;
	register int	n;
	PKCHAR	cwd[SZ_PATHNAME+1];
	XINT	x_maxch = SZ_PATHNAME;
	XCHAR	*slash;
	char	*cp;

	extern  int ZFGCWD(PKCHAR *outstr, XINT *maxch, XINT *status);


	/* If osdir is null, use the current directory.
	 */
	if (osdir[0] == XEOS) {
	    ZFGCWD (cwd, &x_maxch, nchars);
	    if (*nchars == XERR)
		return (XERR);
	    n = *maxch;
	    for (cp=(char *)cwd, op=osdir;  --n >= 0 && (*op = *cp++);  op++)
		;
	    *op = XEOS;
	}

	/* Find the end of the OSDIR string and the index of the / preceeding
	 * the last directory name, e.g., if "a/b/", slash=2.
	 */
	slash = NULL;
	for (op=osdir;  *op != XEOS;  op++)
	    if (*op == '/' && *(op+1) != XEOS)
		slash = op;

	/* Make sure the OSDIR ends with a '/'.
	 */
	if (op > osdir && *(op-1) != '/')
	    *op++ = '/';

	n = *maxch - (op - osdir);

	/* Concatenate the subdirectory. The "subdirectories "." or ".." are
	 * special cases.
	 */
	for (ip=subdir;  *ip == ' ';  ip++)
	    ;

	if (*ip == '.') {
	    switch (*(ip+1)) {
	    case '.':
		if (*(ip+2) == XEOS && slash != NULL && *(slash+1) != '.') {
		    op = slash + 1;
		    n = *maxch - (op - osdir);
		} else
		    goto subdir_;
		break;
	    case EOS:
		break;
	    default:
		goto subdir_;
	    }
	} else {
subdir_:    while (--n >= 0 && (*op = *ip++) != XEOS)
		op++;
	}

	/* If OSDIR is the null string return the pathname of the current
	 * working directory, i.e., "./".
	 */
	if (op == osdir && --n >= 0)
	    *op++ = '.';

	/* Make sure the OSDIR ends with a '/'
	 */
	if (*(op-1) != '/' && --n >= 0)
	    *op++ = '/';

	*op = XEOS;
	*nchars = op - osdir;

	return (XOK);
}
