/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#include <iraf.h>

/* STRNCMP -- Compare up to the first N characters of two strings.  -N is
 * returned if S1 < S2, 0 if S1 == S2, and +N if S1 > S2.
 */
int strncmp ( const char *s1, const char *s2, size_t n )
{
	const char *maxs1 = s1 + n -1;
	for ( ; s1 <= maxs1 && *s1 == *s2 ; s1++, s2++ )
	    if (*s1 == '\0')
		return (0);

	return ( maxs1 < s1 ? 0 : *s1 - *s2);
}
