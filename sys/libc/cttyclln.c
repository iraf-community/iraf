/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYCLEARLN -- Clear the current line.  The cursor is left positioned to
 * the left margin.
 */
c_ttyclearln (fd, tty)
int	fd;			/* output file			*/
int	tty;			/* tty descriptor		*/
{
	TTYCLEARLN (&fd, &tty);
}
