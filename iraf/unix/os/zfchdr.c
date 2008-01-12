/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

#include "zos.h"


/* ZFCHDR -- Change the current working directory.  Save directory name,
 * excluding the trailing "/", in oscwd so that a subsequent call to ZFGCWD
 * will be able to return directory name without a big hassle.
 */
int ZFCHDR ( PKCHAR *newdir, XINT *status )
{
	const char *ip;
	char *op, *maxop;
	char dirname[SZ_PATHNAME+1];

	/* Change pathnames like "a/b/c/" to "a/b/c".
	 */
	maxop = dirname + SZ_PATHNAME+1 -1;
	for ( ip=(const char *)newdir, op=dirname ; op < maxop && *ip != EOS ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	if ( (1 < op - dirname) && (*(op-1) == '/') )
	    *(op-1) = EOS;

	/* Ask UNIX to change the cwd to newdir.
	 */
	if (chdir (dirname) == ERR) {
	    *status = XERR;

	} else if (dirname[0] == '/') {
	    /* Save pathname of directory.
	     */
	    strcpy (oscwd, dirname);
	    *status = XOK;

	} else {
	    /* Concatenate subdir name to current directory pathname.
	     */
	    maxop = oscwd + SZ_PATHNAME+1 -1;
	    for (op=oscwd;  *op;  op++)
		;
	    if ( oscwd < op && *(op-1) != '/' && op < maxop ) {
		*op++ = '/';
	    }
	    for ( ip=dirname ; op < maxop && (*ip) ; op++, ip++ )
		*op = *ip;
	    *op = EOS;
	    *status = XOK;
	}

	return *status;
}
