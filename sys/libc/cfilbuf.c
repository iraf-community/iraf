/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#include <iraf.h>


/* C_FILBUF -- Fill the FIO file buffer.  Called by the GETC macro to fill
** the file buffer when the end of the buffer is reached.  The function
** value is either the first char in the refilled buffer or EOF.  If the
** file is connected to a child process the filter PRFILBUF is called to
** handle the XMIT and XFER requests.
*/
int
c_filbuf (FILE *fp)
{
	register int	nchars;
	XINT	x_fd = fileno(fp);
	XINT	(*fillbuffer)();
	XINT	PRFILBUF(), FILBUF();


	fillbuffer = ((fp->_fflags & _FIPC) ? PRFILBUF : FILBUF);

	iferr (nchars = (int) (*fillbuffer)(&x_fd)) {
	    fp->_fflags |= _FERR;
	    return (EOF);
	} else if (nchars == XEOF) {
	    fp->_fflags |= _FEOF;
	    return (EOF);
	} else
	    return (Memc[fp->_iop++] & 0377);
}
