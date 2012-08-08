/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#include <iraf.h>

/* STRDUP -- Save a copy of a string.
*/
char *
strdup (
  char	*str			/* string to copy		*/
)
{
    register char *ip, *op, *out;
    int  len = strlen (str);

    out = calloc (1, strlen (str) + 1);
    for (ip=str, op=out;  (*op++ = *ip++);  )
        ;

    return (out);
}
