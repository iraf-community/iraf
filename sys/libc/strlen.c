/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_libc
#include <iraf.h>

/* STRLEN -- Length of a string.
 */
strlen (s)
char	*s;
{
	register char	*ip=s;

	while (*ip++)
	    ;

	return (ip - s - 1);
}
