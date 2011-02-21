/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* MALLOC -- Allocate an uninitialized block of memory at least nbytes in size.
*/
char *
malloc (
  unsigned nbytes
)
{
	XINT	x_nchars = (nbytes + sizeof(XCHAR)-1) / sizeof(XCHAR);
	XINT	x_ptr, x_dtype = TY_CHAR;

	iferr (MALLOC (&x_ptr, &x_nchars, &x_dtype))
	    return (NULL);
	else
	    return ((char *)&Memc[x_ptr]);
}
