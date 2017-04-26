/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYPUTS -- Put a control sequence obtained in a prior call to C_TTYGETS
** to the output file.  The baud rate (extracted from the environment at
** TTYODES time or set in a C_TTYSETI call) determines the number of pad
** characters output for delays.
*/
int
c_ttyputs (
  XINT	fd,			/* output file			*/
  XINT	tty,			/* tty descriptor		*/
  char	*cap,			/* two char capability name	*/
  int	afflncnt		/* number of lines affected	*/
)
{
	XINT  x_fd = fd, x_tty = tty, x_afflncnt = afflncnt;

	iferr (TTYPUTS (&x_fd, &x_tty, c_sppstr(cap), &x_afflncnt))
	    return (ERR);
	else
	    return (OK);
}
