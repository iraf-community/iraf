/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* FREE -- Free a block of storage previously allocated by malloc, calloc,
 * or realloc.
 */
free (buf)
char	*buf;
{
	XINT	ptr, dtype = TY_CHAR;

	ptr = Memcptr (buf);
	MFREE (&ptr, &dtype);
}
