/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZRALOC -- Reallocate space on the heap (change the size of the area).
 */
ZRALOC (buf, nbytes, status)
XINT	*buf;			/* receives address of buffer */
XINT	*nbytes;		/* buffer size, machine bytes */
XINT	*status;		/* status return: XOK or XERR		*/
{
	register char *bufptr;
	char    *realloc();

	bufptr = realloc (LOC_TO_ADDR (*buf, char), (int)*nbytes);
	if (bufptr != NULL) {
	    *buf = ADDR_TO_LOC (bufptr);
	    *status = XOK;
	} else
	    *status = XERR;
}
