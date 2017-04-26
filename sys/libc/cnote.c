/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#define import_fset
#include <iraf.h>


/* C_NOTE -- FIO note file offset.  If the actual file is a text file the
** seek offset is a magic number and is returned unchanged.  If the actual
** file is a binary file the seek offset is returned as a zero-indexed byte
** offset.
*/
long
c_note (
  XINT	fd			/* FIO file descriptor		*/
)
{
	long  xchar_offset;
	XINT  x_fd = fd;

	xchar_offset = (long) NOTE (&x_fd);
	if (c_fstati (fd, F_TYPE) == BINARY_FILE)
	    return ((xchar_offset - 1) * sizeof(XCHAR));
	else
	    return (xchar_offset);
}
