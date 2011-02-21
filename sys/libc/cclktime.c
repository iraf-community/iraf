/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* C_CLKTIME -- Return the clock time in long integer seconds since Jan. 1,
** 1980, minus the reference argument (0 for absolute time).
*/
long
c_clktime (
  long reftime			/* reference time		*/
)
{
	XLONG x_reftime = reftime;

	return (CLKTIME (&x_reftime));
}


/* C_CPUTIME -- Return the cpu time consumed by the current process and all
** subprocesses, in long integer milliseconds, minus the reference time.
*/
long
c_cputime (
  long	reftime			/* reference time		*/
)
{
	XLONG x_reftime = reftime;

	return (CPUTIME (&x_reftime));
}
