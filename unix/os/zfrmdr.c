/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/stat.h>
#include <sys/types.h>

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZFRMDR -- Remove an existing directory.
 */
int
ZFRMDR (
  PKCHAR  *dir,
  XINT	  *status
)
{
	char	osdir[SZ_PATHNAME];
	register char *ip, *op;


	/* Change pathnames like "a/b/c/" to "a/b/c".  Probably not necessary,
	 * but...
	 */
	for (ip=(char *)dir, op=osdir;  (*op = *ip++) != EOS;  op++)
	    ;
	if (*--op == '/')
	    *op = EOS;

	if (rmdir (osdir) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return (*status);
}
