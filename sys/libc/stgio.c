/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/*
 * STGIO.C -- STDGRAPH graphics terminal i/o interface.  This is a C binding
 * for the VOS STDGRAPH graphics kernel text i/o routines.  These are used to
 * do direct i/o to the graphics terminal, whether or not the terminal is in
 * graphics mode.  When the terminal is not in graphics mode, i.e., when the
 * "workstation is deactivated", these routines are equivalent to the FIO
 * putline and getline procedures.
 *
 *	nchars|EOF = c_stggetline (STDIN, buf)
 *		     c_stgputline (STDOUT, buf)
 */


/* C_STGGETLINE -- Get a line of text from the graphics terminal.
 */
c_stggetline (fd, buf)
int	fd;			/* input stream		*/
char	*buf;			/* user string buffer	*/
{
	XCHAR	xbuf[SZ_LINE+1];
	int	status;

	iferr (status = STG_GETLINE (&fd, xbuf))
	    return (EOF);
	else {
	    c_strpak (xbuf, buf, SZ_LINE);
	    return (status);
	}
}


/* C_STGPUTLINE -- Put a line of text to the graphics terminal.
 */
c_stgputline (fd, buf)
int	fd;			/* input stream		*/
char	*buf;			/* user string buffer	*/
{
	XCHAR	xbuf[SZ_LINE+1];

	c_strupk (buf, xbuf, SZ_LINE);
	iferr (STG_PUTLINE (&fd, xbuf))
	    return (ERR);
	else
	    return (OK);
}
