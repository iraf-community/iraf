/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#include "osproto.h"

/* ZGTIME -- Get the local standard (clock) time, in units of seconds
 * since 00:00:00 01-Jan-80.  Return the total cpu time consumed by the
 * process (and any subprocesses), in units of milliseconds.
 */
int
ZGTIME (
  XLONG	 *clock_time,				/* seconds */
  XLONG	 *cpu_time 				/* milliseconds */
)
{
	struct	tms t;
	time_t	gmt_to_lst(time_t gmt);
	long	cpu, clkfreq;


	clkfreq = sysconf(_SC_CLK_TCK);

	times (&t);
	*clock_time = gmt_to_lst (time(0));

	/* We don't want any floating point in the kernel code so do the
	 * following computation using integer arithment, taking care to
	 * avoid integer overflow (unless unavoidable) or loss of precision.
	 */
	cpu = (t.tms_utime + t.tms_cutime);

	if (cpu > MAX_LONG/1000)
	  *cpu_time = cpu / clkfreq * 1000;
	else
	  *cpu_time = cpu * 1000 / clkfreq;

	return (XOK);
}
