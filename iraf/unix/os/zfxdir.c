/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

/* ZFXDIR -- Extract OS directory prefix from OSFN.  The null string is
 * returned if there is no directory prefix.  The status value is the number
 * of characters in the output string.
 */
/* osfn  : OS filename		[NOT PACKED]	*/
/* osdir : receives osdir	[NOT PACKED]	*/
int ZFXDIR ( XCHAR *osfn, XCHAR *osdir, XINT *maxch, XINT *nchars )
{
	const XCHAR *ip;
	XCHAR *op, *maxop;
	XCHAR *last_slash;
	size_t bufsize = *maxch + 1;

	for (ip=osfn;  *ip == ' ';  ip++)
	    ;

	/* A UNIX pathname must begin with a / (anything else is considered an
	 * IRAF pathname).  The OSDIR part includes everything up to the
	 * rightmost /.  A string of the form "/name" has the directory prefix
	 * "/", i.e. "name" is considered a filename not a subdirectory name.
	 */
	last_slash = NULL;
	op = osdir;
	maxop = osdir + bufsize -1;

	if (*ip == '/') {
	    for ( ; op < maxop && (*ip) ; op++, ip++ ) {
		*op = *ip;
		if (*op == '/') last_slash = op;
	    }
	}

	if (last_slash != NULL)
	    op = last_slash + 1;

	if ( op <= maxop ) *op = XEOS;
	*nchars = op - osdir;

	return XOK;
}
