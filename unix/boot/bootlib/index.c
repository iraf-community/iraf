/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#ifdef LINUX
#define NOINDEX
#endif
#ifdef MACOSX
/* The following effectively disables the local version. */
#define index strindex
#endif

/* index and rindex are provided by most systems and are redundantly defined
 * here only in case they are missing (we probably should be using the more
 * modern strchr etc. but that is another thing).  Linux (slackware at least)
 * defines these in the same libc.a object module as strchr etc. (this is a
 * bug), which causes a library conflict.  Hence on Linux systems we omit
 * these functions.
 */
#ifndef NOINDEX

/* INDEX -- Return pointer to the first occurrence of a character in a string,
 * or null if the char is not found.
 */
char *
index ( char *str, int ch)
{
	register char	*ip;
	register int	cch;

	for (ip=str;  (cch = *ip);  ip++)
	    if (cch == ch)
		return (ip);

	return (NULL);
}

#endif
