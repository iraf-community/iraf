/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* FREE -- Free a block of storage previously allocated by malloc, calloc,
 * or realloc.
 */
void free ( void *buf )
{
	XPOINTER ptr;
	XINT dtype = TY_CHAR;

	ptr = Memcptr (buf);
	MFREE (&ptr, &dtype);
}
