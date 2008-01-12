/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

/* FGETS -- Return a newline delimited string in the user buffer.  If the
 * buffer fills before newline is seen the string will not be newline
 * delimited.
 */
/* buf     : user supplied output buffer */
/* bufsize : buffer size of buf          */
/* fp      : input file                  */
char *fgets ( char *buf, int bufsize, FILE *fp )
{
	int ch = '\0';
	char *op, *maxop;

	maxop = buf + bufsize -1;
	for ( op = buf ; op < maxop && 0 <= (ch = getc (fp)) ; ) {
	    *op++ = ch;
	    if (ch == '\n')
		break;
	}
	if ( op <= maxop ) *op = EOS;

	if (ch == EOF && op == buf)
	    return (NULL);
	else {
	    return (buf);
	}
}
