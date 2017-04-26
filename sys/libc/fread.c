/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* FREAD -- Read a binary block of data from the input file.  To be consistent
** with UNIX we must read until nelem chars have been accumulated or until
** EOF is seen.  Hence, if reading from a record structured device such as a
** terminal, the read will not be terminated by end of record (newline).
** If the number of bytes (C chars) requested does not fill an integral number
** of XCHARS additional bytes will be read to fill out the last XCHAR.
*/
int
fread (
  char	*bp,			/* output buffer		*/
  int	szelem,			/* nbytes per element		*/
  int	nelem,			/* nelems to read		*/
  FILE	*fp
)
{
	register int nread, n;
	int	nbytes;
	XINT	fd = fileno (fp);
	char	*op = bp;


	fd     = fileno (fp);
	nbytes = nelem * szelem;
	nread  = 0;

	if (fp == stdin)
	    (void) fflush (stdout);
	if (szelem <= 0)
	    return (0);

	for (op = bp;  nread < nbytes;  op += n) {
	    iferr (n = c_read (fd, op, nbytes-nread)) {
		fp->_fflags |= _FERR;
		break;
	    } else if (n == EOF) {
		fp->_fflags |= _FEOF;
		break;
	    } else
		nread += n;
	}

	if (fp->_fflags & (_FEOF|_FERR))
	    return (nread ? nread / szelem : 0);
	else
	    return (nread / szelem);
}
