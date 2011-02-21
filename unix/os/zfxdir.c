/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#define import_spp
#define	import_kernel
#define import_knames
#include <iraf.h>

/* ZFXDIR -- Extract OS directory prefix from OSFN.  The null string is
 * returned if there is no directory prefix.  The status value is the number
 * of characters in the output string.
 */
int
ZFXDIR (
  XCHAR	 *osfn,			/* OS filename		[NOT PACKED]	*/
  XCHAR	 *osdir, 		/* receives osdir	[NOT PACKED]	*/
  XINT	 *maxch, 
  XINT   *nchars
)
{
	register XCHAR	*ip, *op;
	register int	n = *maxch;
	XCHAR	*last_slash;


	for (ip=osfn;  *ip == ' ';  ip++)
	    ;

	/* A UNIX pathname must begin with a / (anything else is considered an
	 * IRAF pathname).  The OSDIR part includes everything up to the
	 * rightmost /.  A string of the form "/name" has the directory prefix
	 * "/", i.e. "name" is considered a filename not a subdirectory name.
	 */
	last_slash = NULL;
	op = osdir;

	if (*ip == '/')
	    for (;  --n >= 0 && (*op = *ip++);  op++)
		if (*op == '/')
		    last_slash = op;

	if (last_slash != NULL)
	    op = last_slash + 1;

	*op = XEOS;
	*nchars = op - osdir;

	return (XOK);
}
