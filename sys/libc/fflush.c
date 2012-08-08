/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_xnames
#include <iraf.h>


/* FFLUSH -- Flush the output file.
*/
int
fflush (
  FILE	*fp
)
{
	XINT	x_fd = fileno(fp);

	iferr (FLUSH (&x_fd))
	    return (EOF);
	else
	    return (OK);
}
