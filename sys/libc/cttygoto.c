/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYGOTO -- Move the cursor to the indicated X,Y position (one-indexed).
 */
c_ttygoto (fd, tty, col, line)
int	fd;			/* output file			*/
int	tty;			/* tty descriptor		*/
int	col;			/* x coordinate			*/
int	line;			/* y coordinate			*/
{
	TTYGOTO (&fd, &tty, &col, &line);
}
