/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>

/* FGETS -- Return a newline delimited string in the user buffer.  If the
 * buffer fills before newline is seen the string will not be newline
 * delimited.
 */
char *
fgets (buf, maxch, fp)
char	*buf;			/* user supplied output buffer	*/
int	maxch;			/* max chars out (incl EOS)	*/
register FILE	*fp;		/* input file			*/
{
	register int	ch, n = maxch - 1;
	register char	*op = buf;

	while (--n >= 0 && (ch = getc (fp)) >= 0) {
	    *op++ = ch;
	    if (ch == '\n')
		break;
	}

	if (ch == EOF && op == buf)
	    return (NULL);
	else {
	    *op = EOS;
	    return (buf);
	}
}
