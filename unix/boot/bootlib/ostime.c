/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/timeb.h>

#define	SECONDS_1970_TO_1980	315532800L


/* OS_UTIME -- Convert IRAF time (local standard, epoch 1980) to UNIX time
 * (greenwich mean time, epoch 1970).	[MACHDEP]
 *
 * NOTE: If this is difficult to implement on your system, you can probably
 * forget about the correction to Greenwich (e.g., 7 hours) and that for
 * daylight savings time (1 hour), and file times will come out a bit off
 * but it probably won't matter.
 */
long
os_utime (iraf_time)
long	iraf_time;
{
	struct	timeb time_info;
	struct	tm *localtime();
	time_t	time_var, lst;
	long	lstl;

	lst = (time_t)iraf_time;
	
	/* Add minutes westward from GMT */
	ftime (&time_info);
	time_var = lst + time_info.timezone * 60L;

	/* Correct for daylight savings time, if in effect */
	lstl = (long)lst;
	if (localtime(&lstl)->tm_isdst)
	    time_var += 60L * 60L;

	return ((long)time_var + SECONDS_1970_TO_1980);
}


/* OS_ITIME -- Convert UNIX time (gmt, epoch 1970) to IRAF time (lst, epoch
 * 1980).	[MACHDEP]
 */
long
os_itime (unix_time)
long	unix_time;
{
	struct	timeb time_info;
	struct	tm *localtime();
	time_t	time_var, gmt;
	long	gmtl;

	gmt = (time_t)unix_time;
	
	/* Subtract minutes westward from GMT */
	ftime (&time_info);
	time_var = gmt - time_info.timezone * 60L;

	/* Correct for daylight savings time, if in effect */
	gmtl = (long)gmt;
	if (localtime(&gmtl)->tm_isdst)
	    time_var -= 60L * 60L;

	return ((long)time_var - SECONDS_1970_TO_1980);
}
