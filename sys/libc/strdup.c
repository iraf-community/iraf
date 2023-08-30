/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#include <iraf.h>

/* STRDUP -- Save a copy of a string.
*/
char *
u_strdup (
  char	*str			/* string to copy		*/
)
{
    register char *ip, *op, *out;
    int  u_strlen (char *s);

    out = calloc (1, u_strlen(str) + 1);
    for (ip=str, op=out;  (*op++ = *ip++);  )
        ;

    return (out);
}
