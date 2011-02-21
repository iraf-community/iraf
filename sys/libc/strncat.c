/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#include <iraf.h>


/* STRNCAT -- Concatenate at most N chars from S2 onto S1.
*/
char *
strncat (
  char	*s1,			/* output string		*/
  char	*s2,			/* string to be appended	*/
  int	n		/* max length of S1		*/
)
{
	register char	*ip, *op;

	for (op=s1;  *op++;  )
	    ;
	for (--op, ip=s2;  (*op++ = *ip++) && --n >= 0;  )
	    ;
	*--op = '\0';

	return (s1);
}
