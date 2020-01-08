/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#ifdef LINUX
#define NOINDEX
#endif
#ifdef MACOSX 
/* The following effectively disables the local version. */
#define rindex strrindex
#endif

#ifndef NOINDEX

/* RINDEX -- Return pointer to the last occurrence of a character in a string,
 * or null if the char is not found.
 */
char *
rindex (
    char *str,
    int   ch
)
{
	register char	*ip;
	register int	cch;
	char	*last;

	for (ip=str, last=0;  (cch = *ip);  ip++)
	    if (cch == ch)
		last = ip;

	return (last);
}

#else
#include <strings.h>

char *
strrindex (
    char *str,
    int   ch
)
{
    return (rindex (str, ch));
}
#endif
