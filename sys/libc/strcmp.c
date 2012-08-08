/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#include <iraf.h>


/* STRCMP -- Compare two strings.  -N is returned if S1 < S2, 0 if S1 == S2,
** and +N if S1 > S2.
*/
int
strcmp (
  char	*s1,
  char	*s2
)
{
	while (*s1 == *s2++)
	    if (*s1++ == '\0')
		return (0);

	return (*s1 - *--s2);
}
