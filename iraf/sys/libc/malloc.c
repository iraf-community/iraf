/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* MALLOC -- Allocate an uninitialized block of memory at least nbytes in size.
 */
void *malloc ( size_t nbytes )
{
	XSIZE_T nchars = (nbytes + sizeof(XCHAR)-1) / sizeof(XCHAR);
	XPOINTER ptr;
	XINT dtype = TY_CHAR;

	iferr (MALLOC (&ptr, &nchars, &dtype))
	    return (NULL);
	else
	    return ((void *)&Memc[ptr]);
}
