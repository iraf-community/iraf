/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <utime.h>
#include "bootlib.h"


/* OS_SETMTIME -- Set the modification (update) time of a file.  Should only
 * be called when the named file is closed.  This is a desirable but
 * nonessential function to implement.
 */
int
os_setmtime (
  char	*fname,
  time_t mtime
)
{
	struct	utimbuf times;

	times.actime = mtime;
	times.modtime = mtime;
	return (utime (vfn2osfn(fname, 0), &times));
}
