/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#include <iraf.h>

/* STRCAT -- Concatenate S2 onto S1.
 */
/* s1 : output string         */
/* s2 : string to be appended */
char *strcat ( char *s1, const char *s2 )
{
	const char *ip;
	char *op;

	for ( op=s1 ; (*op) ; op++ )
	    ;
	for ( ip=s2 ; (*op = *ip) ; op++, ip++ )
	    ;

	return (s1);
}
