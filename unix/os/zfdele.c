#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZFDELE -- Delete a file.
 */
ZFDELE (fname, status)
PKCHAR	*fname;
XINT	*status;
{
	if (unlink ((char *)fname) == ERR)
	    *status = XERR;
	else
	    *status = XOK;
}
