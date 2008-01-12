/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#include <iraf.h>

/* STRNCPY -- Copy exactly N characters from S2 to S1, truncating or null
 * padding S2; the output string may not be null terminated if the length
 * of S2 is N or more.
 */
/* s1: output string */
/* s2: string to be moved */
char *strncpy ( char *s1, const char *s2, size_t n )
{
	const char *ip;
	char *op, *maxop;

	maxop = s1 + n -1;
	for ( ip=s2, op=s1 ; op <= maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	for ( ; op <= maxop ; op++ )
	    *op = '\0';

	return (s1);
}
