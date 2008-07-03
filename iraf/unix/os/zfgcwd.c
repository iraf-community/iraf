/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

#include "zos.h"

/* ZFGCWD -- Get working (current) UNIX directory.  The current working
 * directory, once set, is saved in oscwd.
 */
int ZFGCWD ( PKCHAR *outstr, XINT *maxch, XINT *status )
{
	const char *ip;
	char *op, *maxop;
	char dirname[1025];
	size_t bufsize = *maxch + 1;

	/* If cwd is already known, just return the name.  Reconstructing
	 * the pathname of the cwd is expensive on some systems.
	 */
	if (oscwd[0] != EOS)
	    ip = oscwd;
	else {
	    dirname[0] = EOS;
#ifdef POSIX
	    ip = getcwd (dirname, 1025);
#else
	    ip = getwd (dirname);
#endif
	    if (ip == NULL) {
		*status = XERR;
		return *status;
	    } else
		safe_strcpy (oscwd, SZ_PATHNAME+1, dirname);
	}

	maxop = (char *)outstr + bufsize -1;
	for ( op=(char *)outstr ; op < maxop && *ip != EOS ; op++, ip++ )
	    *op = *ip;
	if ( op <= maxop ) *op = EOS;

	/* Make sure a concatenatable directory prefix is returned.
	 */
	if ( (char *)outstr < op && *(op-1) != '/' && op < maxop ) {
	    *op++ = '/';
	    *op = EOS;
	}

	*status = op - (char *)outstr;

	return *status;
}
