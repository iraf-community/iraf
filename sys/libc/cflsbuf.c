/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#include <iraf.h>


/* C_FLSBUF -- Flush the FIO file buffer.  Called by the PUTC macro to flush
** the file buffer when it fills.  The function value returned is either the
** first char written to the buffer (passed as an argument) or EOF in the
** event of an error.
*/
int
c_flsbuf (
  unsigned int	ch,		/* char which caused the fault	*/
  FILE	*fp			/* output file			*/
)
{
	register int	buf_not_full;
	XINT	fd = fileno(fp);
	XINT	nreserve = 1;


	/* If we were called due to flush on newline and there is space in
	 * the buffer, put the ch in the buffer before flushing.
	 */
	buf_not_full = (fp->_iop < fp->_otop);
	if (buf_not_full)
	    Memc[fp->_iop++] = (unsigned)ch;

	iferr (FLSBUF (&fd, &nreserve)) {
	    fp->_fflags |= _FERR;
	    return (EOF);
	}

	if (!buf_not_full)
	    Memc[fp->_iop++] = (unsigned)ch;

	return (ch);
}
