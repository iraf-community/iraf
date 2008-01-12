/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

#include "zos.h"

/* ZFMKDR -- Create a new directory.
 */
int ZFMKDR ( PKCHAR *newdir, XINT *status )
{
	char osdir[SZ_PATHNAME];
	char *op, *maxop;
	const char *ip;

	/* Change pathnames like "a/b/c/" to "a/b/c".  Probably not necessary,
	 * but...
	 */
	maxop = osdir + SZ_PATHNAME -1;
	ip = (const char *)newdir;
	for ( op=osdir ; op < maxop && *ip != EOS ; op++, ip++ )
	    *op = *ip;
	*op = EOS;
	if ( osdir < op && *--op == '/' )
	    *op = EOS;

	if (mkdir (osdir, _u_fmode(0777)) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return *status;
}
