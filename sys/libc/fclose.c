/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_xnames
#include <iraf.h>

/* FCLOSE -- Close a file opened with fopen.
 */
fclose (fp)
FILE	*fp;
{
	XINT	fd = fileno(fp);

	iferr (CLOSE (&fd))
	    return (EOF);
	else
	    return (OK);
}
