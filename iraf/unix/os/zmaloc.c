/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

/* ZMALOC -- Allocate space on the heap.  NULL is returned if the buffer
 * cannot be allocated, otherwise the address of the buffer is returned
 * in "buf".
 */
/* buf    : receives address of buffer		*/
/* nbytes : buffer size, machine bytes		*/
/* status : status return: XOK or XERR		*/
int ZMALOC ( XPOINTER *buf, XINT *nbytes, XINT *status )
{
	char *bufptr;

	bufptr = malloc (*nbytes);
	if (bufptr != NULL) {
	    *buf = ADDR_TO_LOC (bufptr);
	    *status = XOK;
	} else
	    *status = XERR;

	return *status;
}
