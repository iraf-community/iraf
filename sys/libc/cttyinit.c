/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYINIT -- Output the initialization sequence, if any, to the output
 * file.
 */
c_ttyinit (fd, tty)
int	fd;			/* output file			*/
int	tty;			/* tty descriptor		*/
{
	TTYINIT (&fd, &tty);
}
