/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>

/* FPUTC -- Put a character to the output file.  Offered as a functionally
 * equivalent alternative to the macro PUTC.
 */
fputc (ch, fp)
char	ch;
FILE	*fp;
{
	return (putc (ch, fp));
}
