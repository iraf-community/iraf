/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

/* ZFPATH -- Return the absolute pathname equivalent of an OSFN.  If the null
 * string is given the OSFN of the current working directory is returned.
 */
/* osfn     : input OS filename	[NOT PACKED]	*/
/* pathname : output pathname	[NOT PACKED]	*/
int ZFPATH ( XCHAR *osfn, XCHAR *pathname, XINT *maxch, XINT *nchars )
{
	const char *cp;
	const XCHAR *ip;
	PKCHAR *cwd;
	XCHAR *op, *maxop;
	size_t bufsize = *maxch + 1;

	op = pathname;
	maxop = pathname + bufsize -1;
	for (ip=osfn;  *ip == ' ';  ip++)
	    ;

	/* If the OSFN begins with a / it is already an absolute pathname.
	 */
	if (*ip != '/') {
	    cwd = malloc(bufsize*sizeof(*cwd));
	    if ( cwd == NULL ) {
		*nchars = 0;
		return XERR;
	    }
	    ZFGCWD (cwd, maxch, nchars);
	    for ( cp=(const char *)cwd ; op < maxop && (*cp) ; op++, cp++ )
		*op = *cp;
	    free(cwd);
	}

	/* Append the filename */
	for ( ; op < maxop && *ip != XEOS ; op++, ip++ )
	    *op = *ip;

	if ( op <= maxop ) *op = XEOS;
	*nchars = (op - pathname);

	return XOK;
}
