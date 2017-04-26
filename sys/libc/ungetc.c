/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#include <iraf.h>


/* UNGETC -- Push a character back into the input stream.  Pushback is last
** in first out, i.e., the last character pushed back is the first one
** read by GETC.  Characters (and strings) may be pushed back until the
** FIO pushback buffer overflows.
*/
int
ungetc (
  int	ch,
  FILE	*fp
)
{
	XINT	x_fd = fileno(fp);
	XCHAR	x_ch = ch;

	iferr (UNGETC (&x_fd, &x_ch))
	    return (EOF);
	else
	    return (ch);
}
