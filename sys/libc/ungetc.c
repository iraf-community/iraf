/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#include <iraf.h>

/* UNGETC -- Push a character back into the input stream.  Pushback is last
 * in first out, i.e., the last character pushed back is the first one
 * read by GETC.  Characters (and strings) may be pushed back until the
 * FIO pushback buffer overflows.
 */
ungetc (ch, fp)
int	ch;
FILE	*fp;
{
	XINT	fd = fileno(fp);
	XCHAR	xch = ch;

	iferr (UNGETC (&fd, &xch))
	    return (EOF);
	else
	    return (ch);
}
