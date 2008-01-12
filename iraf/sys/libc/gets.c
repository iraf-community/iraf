/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

/* GETS -- Read a newline terminated sequence from the standard input and
 * return the resultant string minus the newline to the user.
 */
/* buf : user supplied output buffer */
char *gets ( char *buf )
{
	FILE *fp = stdin;
	char *op = buf;
	int ch;

	while ((ch = getc (fp)) != EOF) {
	    if (ch == '\n')
		break;
	    *op++ = ch;
	}
	*op = EOS;

	if (ch == EOF && op == buf)
	    return (NULL);
	else
	    return (buf);
}
