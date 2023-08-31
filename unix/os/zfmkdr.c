/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/stat.h>
#include <sys/types.h>

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZFMKDR -- Create a new directory.
 */
int
ZFMKDR (
  PKCHAR  *newdir,
  XINT	  *status
)
{
	char	osdir[SZ_PATHNAME];
	register char *ip, *op;

	extern  int _u_fmode(int mode);


	/* Change pathnames like "a/b/c/" to "a/b/c".  Probably not necessary,
	 * but...
	 */
	for (ip=(char *)newdir, op=osdir;  (*op = *ip++) != EOS;  op++)
	    ;
	if (*--op == '/')
	    *op = EOS;

	if (mkdir (osdir, _u_fmode(0777)) == ERR)
	    *status = XERR;
	else {
	    if (strncmp (osdir, "/tmp", 4) == 0)
	        chmod (osdir, _u_fmode(0777));
	    *status = XOK;
	}

	return (*status);
}
