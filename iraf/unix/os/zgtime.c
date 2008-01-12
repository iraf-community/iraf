/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#ifndef SYSV
#include <sys/timeb.h>
#endif
#include <sys/times.h>
#include <sys/time.h>
#include <time.h>

#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

#include "zos.h"

/* ZGTIME -- Get the local standard (clock) time, in units of seconds
 * since 00:00:00 01-Jan-80.  Return the total cpu time consumed by the
 * process (and any subprocesses), in units of milliseconds.
 */
/* clock_time : seconds */
/* cpu_time   : milliseconds */
int ZGTIME ( XLONG *clock_time, XLONG *cpu_time )
{
	struct	tms t;
	long	cpu, clkfreq;

#ifdef LINUX
	clkfreq = CLOCKS_PER_SEC;
#else
	clkfreq = CLKFREQ;
#endif

	times (&t);
	*clock_time = gmt_to_lst ((time_t)time(0));

	/* We don't want any floating point in the kernel code so do the
	 * following computation using integer arithment, taking care to
	 * avoid integer overflow (unless unavoidable) or loss of precision.
	 */
	cpu = (t.tms_utime + t.tms_cutime);

	if (cpu > MAX_LONG/1000)
	    *cpu_time = cpu / clkfreq * 1000;
	else
	    *cpu_time = cpu * 1000 / clkfreq;

	return XOK;
}
