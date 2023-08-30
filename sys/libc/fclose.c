/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_xnames
#include <iraf.h>

/* FCLOSE -- Close a file opened with fopen.
*/
int
fclose (
  FILE	*fp
)
{
	XINT	x_fd = fileno(fp);
        int     CLOSE (XINT *fd_arg);
	
	iferr (CLOSE (&x_fd))
	    return (EOF);
	else
	    return (OK);
}
