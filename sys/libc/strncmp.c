/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_libc
#include <iraf.h>

/* STRNCMP -- Compare up to the first N characters of two strings.  -N is
 * returned if S1 < S2, 0 if S1 == S2, and +N if S1 > S2.
 */
strncmp (s1, s2, n)
register char	*s1;
register char	*s2;
register int	n;
{
	while (--n >= 0 && *s1 == *s2++)
	    if (*s1++ == '\0')
		return (0);

	return (n < 0 ? 0 : *s1 - *--s2);
}
