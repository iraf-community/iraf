/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* REALLOC -- Reallocate a buffer, i.e., change the size of an already
 * allocated buffer.  If necessary the buffer is moved, preserving any
 * data in the buffer.
 */
void *realloc ( void *buf, size_t newsize )
{
	XSIZE_T	nchars = (newsize + sizeof(XCHAR)-1) / sizeof(XCHAR);
	XPOINTER ptr;
	XINT	dtype = TY_CHAR;

	ptr = buf ? Memcptr(buf) : 0;
	iferr (REALLOC (&ptr, &nchars, &dtype))
	    return (NULL);
	else
	    return ((void *)&Memc[ptr]);
}
