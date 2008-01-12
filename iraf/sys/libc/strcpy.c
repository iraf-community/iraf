/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#include <iraf.h>

/* STRCPY -- Copy string S2 to S1.
 */
/* s1 : output string      */
/* s2 : string to be moved */
char *strcpy ( char *s1, const char *s2 )
{
	const char *ip;
	char *op;

	for (ip=s2, op=s1;  (*op++ = *ip++);  )
	    ;

	return (s1);
}
