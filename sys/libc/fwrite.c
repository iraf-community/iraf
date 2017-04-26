/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* FWRITE -- Write a binary block of data to the output file.  If the number
** of bytes (C chars) specified does not fill an integral number of XCHARS
** additional bytes will be written to fill out the last XCHAR.  The actual
** number of elements written is returned as the function value.
*/
int
fwrite (
  char	*bp,			/* output buffer		*/
  int szelem,			/* nbytes per element		*/
  int	nelem,			/* nelems to read		*/
  FILE	*fp
)
{
	register int stat;
	XINT  fd = fileno (fp);


	if (szelem) {
	    stat = c_write (fd, bp, nelem * szelem);
	    if (stat == ERR) {
		fp->_fflags |= _FERR;
		return (0);
	    } else
		return (stat / szelem);
	} else
	    return (0);
}
