/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#define import_spp
#define import_kernel
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
/* osdir  : pathname		[NOT PACKED]	*/
/* maxch  : max xchars in osdir			*/
/* subdir : subdirectory name	[NOT PACKED]	*/
/* nchars : receives lenght of osdir		*/
int ZFSUBD ( XCHAR *osdir, XINT *maxch, XCHAR *subdir, XINT *nchars )
{
	const XCHAR *ip;
	XCHAR *op, *maxop;
	PKCHAR cwd[SZ_PATHNAME+1];
	XINT x_maxch = SZ_PATHNAME;
	XCHAR *slash;
	const char *cp;
	size_t bufsize = *maxch + 1;

	/* If osdir is null, use the current directory.
	 */
	if (osdir[0] == XEOS) {
	    ZFGCWD (cwd, &x_maxch, nchars);
	    if (*nchars == XERR)
		return XERR;
	    maxop = osdir + bufsize -1;
	    for ( cp=(const char *)cwd, op=osdir ; op < maxop && (*cp) ; op++, cp++ )
		*op = *cp;
	    if ( op <= maxop ) *op = XEOS;
	}

	/* Find the end of the OSDIR string and the index of the / preceeding
	 * the last directory name, e.g., if "a/b/", slash=2.
	 */
	slash = NULL;
	maxop = osdir + bufsize -1;
	for (op=osdir;  *op != XEOS;  op++)
	    if (*op == '/' && *(op+1) != XEOS)
		slash = op;

	/* Make sure the OSDIR ends with a '/'.
	 */
	if ( osdir < op && *(op-1) != '/' && op < maxop )
	    *op++ = '/';

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
		} else
		    goto subdir_;
		break;
	    case EOS:
		break;
	    default:
		goto subdir_;
	    }
	} else {
subdir_:
	    for ( ; op < maxop && *ip != XEOS ; op++, ip++ )
		*op = *ip;
	}

	/* If OSDIR is the null string return the pathname of the current
	 * working directory, i.e., "./".
	 */
	if ( op == osdir && op < maxop ) {
	    *op++ = '.';
	}

	/* Make sure the OSDIR ends with a '/'
	 */
	if ( osdir < op && *(op-1) != '/' && op < maxop ) {
	    *op++ = '/';
	}

	if ( op <= maxop ) *op = XEOS;
	*nchars = op - osdir;

	return XOK;
}
