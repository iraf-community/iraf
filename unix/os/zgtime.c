/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/times.h>
#include <sys/time.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZGTIME -- Get the local standard (clock) time, in units of seconds
 * since 00:00:00 01-Jan-80.  Return the total cpu time consumed by the
 * process (and any subprocesses), in units of milliseconds.
 */
ZGTIME (clock_time, cpu_time)
XLONG	*clock_time;				/* seconds */
XLONG	*cpu_time;				/* milliseconds */
{
	struct	tms t;
	long	time();
	time_t	gmt_to_lst();

	times (&t);
	*clock_time = gmt_to_lst ((time_t)time(0));

	/* Eliminate floating computation for the Alliant:
	 */
	*cpu_time = (t.tms_utime + t.tms_cutime) * 1000 / HZ;
}
