/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#define	import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

/* ZFPATH -- Return the absolute pathname equivalent of an OSFN.  If the null
 * string is given the OSFN of the current working directory is returned.
 */
int
ZFPATH (
  XCHAR	*osfn,			/* input OS filename	[NOT PACKED]	*/
  XCHAR	*pathname,		/* output pathname	[NOT PACKED]	*/
  XINT	*maxch,
  XINT	*nchars
)
{
	register char	*cp;
	register XCHAR	*ip, *op;
	register int	n = *maxch;
	PKCHAR	cwd[SZ_PATHNAME+1];

	extern  int ZFGCWD(PKCHAR  *outstr, XINT *maxch, XINT *status);


	op = pathname;
	for (ip=osfn;  *ip == ' ';  ip++)
	    ;

	/* If the OSFN begins with a / it is already an absolute pathname.
	 */
	if (*ip != '/') {
	    ZFGCWD (cwd, maxch, nchars);
	    for (cp=(char *)cwd;  --n >= 0 && (*op = *cp++);  op++)
		;
	}

	/* Append the filename */
	while (--n >= 0 && (*op = *ip++) != XEOS)
	    op++;

	*op = XEOS;
	*nchars = (op - pathname);

	return (XOK);
}
