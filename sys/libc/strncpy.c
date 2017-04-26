/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#include <iraf.h>


/* STRNCPY -- Copy exactly N characters from S2 to S1, truncating or null
** padding S2; the output string may not be null terminated if the length
** of S2 is N or more.
*/
char *
strncpy (
  char	*s1,			/* output string		*/
  char	*s2,			/* string to be moved		*/
  int   n
)
{
	register char	*ip, *op;

	for (ip=s2, op=s1;  --n >= 0 && (*op++ = *ip++);  )
	    ;
	while (--n >= 0)
	    *op++ = '\0';

	return (s1);
}
