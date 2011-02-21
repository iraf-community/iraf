/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* ISATTY -- Test if the given file is a terminal.
*/
int
isatty (
  XINT	fd
)
{
	XINT  x_fd = fd;

	return (XISATTY (&x_fd));
}
