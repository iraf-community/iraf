/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

extern	char oscwd[];			/* See ZFCHDR		*/

/* ZFGCWD -- Get working (current) UNIX directory.  The current working
 * directory, once set, is saved in oscwd.
 */
ZFGCWD (outstr, maxch, status)
PKCHAR	*outstr;
XINT	*maxch, *status;
{
	register char	*ip, *op;
	register int	n;
	char	dirname[1025];
#ifdef SOLARIS
	char	*getcwd();
#else
	char	*getwd();
#endif

	/* If cwd is already known, just return the name.  Reconstructing
	 * the pathname of the cwd is expensive on some systems.
	 */
	if (oscwd[0] != EOS)
	    ip = oscwd;
	else {
#ifdef SOLARIS
	    ip = getcwd (dirname, 1024);
#else
	    ip = getwd (dirname);
#endif
	    if (ip == NULL) {
		*status = XERR;
		return;
	    } else
		strcpy (oscwd, dirname);
	}

	op = (char *)outstr;
	for (n = *maxch;  --n >= 0 && (*op = *ip++) != EOS;  )
	    op++;

	/* Make sure a concatenatable directory prefix is returned.
	 */
	if (*(op-1) != '/') {
	    *op++ = '/';
	    *op = EOS;
	}

	*status = op - (char *)outstr;
}
