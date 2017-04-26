/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/*  FGETS -- Return a newline delimited string in the user buffer.  If the
**  buffer fills before newline is seen the string will not be newline
**  delimited.
*/
char *
fgets (
  char	*buf,			/* user supplied output buffer	*/
  int	maxch,			/* max chars out (incl EOS)	*/
  FILE	*fp			/* input file			*/
)
{
	register int	ch = 0, lastch = 0, n = maxch - 1;
	register char	*op = buf;

	while (--n >= 0 && (ch = getc (fp)) >= 0) {
	    lastch = ch;
	    if (ch == '\r')		/* handle DOS-style CR-NL	*/
		continue;
	    *op++ = ch;
	    if (ch == '\n')
		break;
	}

	if (ch == EOF && op == buf)
	    return ((char *) NULL);
	else {
#ifdef ADD_NEWLINE
	    if (lastch != '\n')		/* handle missing NL at EOF	*/
	        *op++ = '\n';
#endif
	    *op = EOS;
	    return (buf);
	}
}
