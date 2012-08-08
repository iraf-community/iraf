/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* STGIO.C -- STDGRAPH graphics terminal i/o interface.  This is a C binding
** for the VOS STDGRAPH graphics kernel text i/o routines.  These are used to
** do direct i/o to the graphics terminal, whether or not the terminal is in
** graphics mode.  When the terminal is not in graphics mode, i.e., when the
** "workstation is deactivated", these routines are equivalent to the FIO
** putline and getline procedures.
**
**	nchars|EOF = c_stggetline (STDIN, buf)
**		     c_stgputline (STDOUT, buf)
*/


/* C_STGGETLINE -- Get a line of text from the graphics terminal.
*/
int
c_stggetline (
  XINT	fd,			/* input stream		*/
  char	*buf,			/* user string buffer	*/
  int   maxch			/* max bufer size	*/
)
{
	XCHAR	xbuf[maxch+1];
	XINT    x_fd = fd;
	int	status;

	iferr (status = STG_GETLINE (&x_fd, xbuf))
	    return (EOF);
	else {
	    c_strpak (xbuf, buf, maxch);
	    return (status);
	}
}


/* C_STGPUTLINE -- Put a line of text to the graphics terminal.
*/
int
c_stgputline (
  XINT	fd,			/* input stream		*/
  char	*buf			/* user string buffer	*/
)
{
	XCHAR	xbuf[SZ_LINE+1];
	XINT    x_fd = fd;

	c_strupk (buf, xbuf, SZ_LINE);
	iferr (STG_PUTLINE (&x_fd, xbuf))
	    return (ERR);
	else
	    return (OK);
}
