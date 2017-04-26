/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* REALLOC -- Reallocate a buffer, i.e., change the size of an already
** allocated buffer.  If necessary the buffer is moved, preserving any
** data in the buffer.
*/
char *
realloc (
  char	*buf,
  unsigned newsize
)
{
	XINT	x_nchars = (newsize + sizeof(XCHAR)-1) / sizeof(XCHAR);
	XINT	x_ptr, x_dtype = TY_CHAR;

	x_ptr = buf ? Memcptr(buf) : NULL;
	iferr (REALLOC (&x_ptr, &x_nchars, &x_dtype))
	    return (NULL);
	else
	    return ((char *)&Memc[x_ptr]);
}
