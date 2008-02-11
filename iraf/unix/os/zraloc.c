/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

/* ZRALOC -- Reallocate space on the heap (change the size of the area).
 */
/* buf    : receives address of buffer	*/
/* nbytes : buffer size, machine bytes	*/
/* status : status return: XOK or XERR	*/
int ZRALOC ( XPOINTER *buf, XSIZE_T *nbytes, XINT *status )
{
	char *bufptr;

	bufptr = realloc (LOC_TO_ADDR (*buf, char), *nbytes);
	if (bufptr != NULL) {
	    *buf = ADDR_TO_LOC (bufptr);
	    *status = XOK;
	} else
	    *status = XERR;

	return *status;
}
