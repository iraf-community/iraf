/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>
#include "bootlib.h"


/* OS_SETMTIME -- Set the modification (update) time of a file.  Should only
 * be called when the named file is closed.  This is a desirable but
 * nonessential function to implement.
 */
int
os_setmtime (
  char	*fname,
  long	mtime 
)
{
	struct	timeval tvp[2];

	tvp[0].tv_sec  = tvp[1].tv_sec  = mtime;
	tvp[0].tv_usec = tvp[1].tv_usec = 0L;

	return (utimes (vfn2osfn(fname,0), tvp));
}
