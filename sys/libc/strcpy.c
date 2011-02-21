/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#include <iraf.h>

/* STRCPY -- Copy string S2 to S1.
*/
char *
strcpy (
  char	*s1,			/* output string		*/
  char	*s2			/* string to be moved		*/
)
{
	register char	*ip, *op;

	for (ip=s2, op=s1;  (*op++ = *ip++);  )
	    ;

	return (s1);
}
