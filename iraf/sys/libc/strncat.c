/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#include <iraf.h>

/* STRNCAT -- Concatenate at most N chars from S2 onto S1.
 */
/* s1 : output string             */
/* s2 : string to be appended     */
/* n  : max length to be appended */
char *strncat ( char *s1, const char *s2, size_t n )
{
	const char *ip, *maxip;
	char *op;

	for ( op=s1 ; (*op) ; op++ )
	    ;
	maxip = s2 + n -1;
	for ( ip=s2 ; ip <= maxip && (*ip) ; op++, ip++ )
	    *op = *ip;
	*op = '\0';

	return (s1);
}
