/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_xnames
#include <iraf.h>

/* FFLUSH -- Flush the output file.
 */
fflush (fp)
FILE	*fp;
{
	XINT	fd = fileno(fp);

	iferr (FLUSH (&fd))
	    return (EOF);
	else
	    return (OK);
}
