/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYPUTS -- Put a control sequence obtained in a prior call to C_TTYGETS
 * to the output file.  The baud rate (extracted from the environment at
 * TTYODES time or set in a C_TTYSETI call) determines the number of pad
 * characters output for delays.
 */
c_ttyputs (fd, tty, cap, afflncnt)
int	fd;			/* output file			*/
int	tty;			/* tty descriptor		*/
char	*cap;			/* two char capability name	*/
int	afflncnt;		/* number of lines affected	*/
{
	iferr (TTYPUTS (&fd, &tty, c_sppstr(cap), &afflncnt))
	    return (ERR);
	else
	    return (OK);
}
