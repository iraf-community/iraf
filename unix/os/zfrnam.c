/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_protect
#define import_spp
#include <iraf.h>

/* ZFRNAM -- Rename a file.  Do nothing to original file if operation
 * fails.  File must retain all attributes; special action is required
 * to transfer file protection.
 */
int
ZFRNAM (
  PKCHAR  *oldname, 
  PKCHAR  *newname,
  XINT	  *status
)
{
	static	XINT queryprot  = QUERY_PROTECTION;
	static	XINT removeprot = REMOVE_PROTECTION;
	static	XINT setprot    = SET_PROTECTION;
	XINT	protected;

	extern  int ZFPROT(PKCHAR *fname, XINT *action, XINT *status);


	/* Most remove file protection before renaming the file, else
	 * zfprot will not find the file and will refuse to delete the
	 * .. link to the original file.
	 */
	ZFPROT (oldname, &queryprot, &protected);
	if (protected == XYES)
	    ZFPROT (oldname, &removeprot, status);

	if (rename ((char *)oldname, (char *)newname) == ERR) {
	    if (protected == XYES)
		ZFPROT (oldname, &setprot, status);
	    *status = XERR;
	} else {
	    if (protected == XYES)
		ZFPROT (newname, &setprot, status);
	    else
		*status = XOK;
	}

	return (*status);
}
