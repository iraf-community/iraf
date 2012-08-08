/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>



/* ZMALOC -- Allocate space on the heap.  NULL is returned if the buffer
 * cannot be allocated, otherwise the address of the buffer is returned
 * in "buf".
 */
int
ZMALOC (
  XINT	*buf,			/* receives address of buffer		*/
  XINT	*nbytes,		/* buffer size, machine bytes		*/
  XINT	*status 		/* status return: XOK or XERR		*/
)
{
	register char *bufptr;
	int      stat;

	bufptr = malloc ((size_t)*nbytes);
	if (bufptr != NULL) {
	    *buf = ADDR_TO_LOC(bufptr);
            if (*buf > 0)
                *status = XOK;
            else
                *status = XERR;
	} else
	    *status = XERR;

	stat = *status;
	return (stat);
}
