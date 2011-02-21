/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* CALLOC -- Allocate memory for NELEM elements of size ELSIZE bytes per
** element.  The space is initialized to all zeros.
*/
char *
calloc (
  unsigned int nelems,
  unsigned int elsize
)
{
	XINT	nchars = (nelems*elsize + sizeof(XCHAR)-1) / sizeof(XCHAR);
	XINT	ptr, dtype = TY_CHAR;


	iferr (CALLOC (&ptr, &nchars, &dtype))
	    return (NULL);
	else
	    return ((char *)&Memc[ptr]);
}
