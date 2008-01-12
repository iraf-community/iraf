/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#include <iraf.h>

/* STRLEN -- Length of a string.
 */
size_t strlen ( const char *s )
{
	const char *ip=s;

	while (*ip)
	    ip++;

	return (ip - s);
}
