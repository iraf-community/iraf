/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#include <iraf.h>


/* STRCAT -- Concatenate S2 onto S1.
*/
char *
strcat (
  char	*s1,			/* output string		*/
  char	*s2			/* string to be appended	*/
)
{
	register char	*ip, *op;

	for (op=s1;  *op++;  )
	    ;
	for (--op, ip=s2;  (*op++ = *ip++);  )
	    ;

	return (s1);
}
