/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "bootlib.h"


/* FDATE -- Get the date of last modification of a file.  [MACHDEP]
 */
long
os_fdate (char *fname)
{
	struct	stat buf;

	if (stat (vfn2osfn(fname,0), &buf) == ERR)
	    return (0);
	else
	    return (buf.st_mtime);
}
