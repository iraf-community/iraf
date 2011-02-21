/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#define import_fset
#include <iraf.h>


/* C_SEEK -- FIO seek on a file.  If the actual file is a text file the
** seek offset is assumed to have been obtained by a prior call to FTELL
** and is passed on unchanged.  If the actual file is a binary file the
** seek offset is a zero-indexed byte offset.  This offset is converted
** to a one-indexed XCHAR offset; it is an error if the offset is not
** aligned to an XCHAR word boundary.
*/
int
c_seek (
  XINT	fd,			/* FIO file descriptor		*/
  long	offset			/* file offset			*/
)
{
	XLONG	x_char_offset = offset;
	XINT    x_fd = fd;
	int	bypass;


	bypass = (offset == BOFL || offset == EOFL
		|| c_fstati (fd, F_TYPE) == TEXT_FILE);

	if (!bypass) {
	    x_char_offset /= sizeof(XCHAR);
	    if ((x_char_offset++ * sizeof(XCHAR)) != offset)
		return ((long) ERR);
	}

	iferr (SEEK (&x_fd, &x_char_offset))
	    return (ERR);
	else
	    return (OK);
}
