/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* ../memio/calloc.x */
int CALLOC ( XPOINTER *, XINT *, XINT * );

/* CALLOC -- Allocate memory for NELEM elements of size ELSIZE bytes per
 * element.  The space is initialized to all zeros.
 */
void *calloc ( size_t nelems, size_t elsize )
{
	XINT	nchars = (nelems*elsize + sizeof(XCHAR)-1) / sizeof(XCHAR);
	XINT	ptr, dtype = TY_CHAR;

	iferr (CALLOC (&ptr, &nchars, &dtype))
	    return (NULL);
	else
	    return ((char *)&Memc[ptr]);
}
