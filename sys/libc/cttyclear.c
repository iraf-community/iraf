/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYCLEAR -- Clear the screen.
 */
c_ttyclear (fd, tty)
int	fd;			/* output file			*/
int	tty;			/* tty descriptor		*/
{
	TTYCLEAR (&fd, &tty);
}
