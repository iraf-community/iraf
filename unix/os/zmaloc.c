/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZMALOC -- Allocate space on the heap.  NULL is returned if the buffer
 * cannot be allocated, otherwise the address of the buffer is returned
 * in "buf".
 */
ZMALOC (buf, nbytes, status)
XINT	*buf;			/* receives address of buffer		*/
XINT	*nbytes;		/* buffer size, machine bytes		*/
XINT	*status;		/* status return: XOK or XERR		*/
{
	register char *bufptr;
	char    *malloc();

	bufptr = malloc ((int)*nbytes);
	if (bufptr != NULL) {
	    *buf = ADDR_TO_LOC (bufptr);
	    *status = XOK;
	} else
	    *status = XERR;
}
