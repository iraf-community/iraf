/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZRALOC -- Reallocate space on the heap (change the size of the area).
 */
int
ZRALOC (
  XINT	*buf,			/* receives address of buffer 		*/
  XINT	*nbytes,		/* buffer size, machine bytes 		*/
  XINT	*status 		/* status return: XOK or XERR		*/
)
{
	register char *bufptr;
	char  *ptr = (void *) NULL;
	int   zstat;

        ptr = LOC_TO_ADDR(*buf,char);
	bufptr = realloc (ptr, (size_t)*nbytes);

	if (bufptr != NULL) {
            *buf = ADDR_TO_LOC(bufptr);
            if (*buf > 0)
                *status = XOK;
            else
                *status = XERR;
	} else
	    *status = XERR;

	zstat =  *status;
	return (zstat);
}
