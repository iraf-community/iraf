/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* MALLOC -- Allocate an uninitialized block of memory at least nbytes in size.
 */
char *
malloc (nbytes)
unsigned nbytes;
{
	XINT	nchars = (nbytes + sizeof(XCHAR)-1) / sizeof(XCHAR);
	XINT	ptr, dtype = TY_CHAR;

	iferr (MALLOC (&ptr, &nchars, &dtype))
	    return (NULL);
	else
	    return ((char *)&Memc[ptr]);
}
