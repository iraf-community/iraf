/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#define import_fset
#include <iraf.h>

/* C_SEEK -- FIO seek on a file.  If the actual file is a text file the
 * seek offset is assumed to have been obtained by a prior call to FTELL
 * and is passed on unchanged.  If the actual file is a binary file the
 * seek offset is a zero-indexed byte offset.  This offset is converted
 * to a one-indexed XCHAR offset; it is an error if the offset is not
 * aligned to an XCHAR word boundary.
 */
/* fd     : FIO file descriptor */
/* offset : file offset         */
int c_seek ( int fd, long offset )
{
	XLONG xchar_offset = offset;
	XINT x_fd = fd;
	int bypass;

	bypass = (offset == BOFL || offset == EOFL
		|| c_fstati (x_fd, F_TYPE) == TEXT_FILE);

	if (!bypass) {
	    xchar_offset /= sizeof(XCHAR);
	    if ((xchar_offset++ * sizeof(XCHAR)) != offset)
		return (ERR);
	}

	iferr (SEEK (&x_fd, &xchar_offset))
	    return (ERR);
	else
	    return (OK);
}
