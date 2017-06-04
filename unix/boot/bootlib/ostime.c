/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <time.h>

#define	SECONDS_1970_TO_1980	315532800L
static	long os_timezone();


/* OS_UTIME -- Convert IRAF time (local standard, epoch 1980) to UNIX time
 * (greenwich mean time, epoch 1970).	[MACHDEP]
 *
 * NOTE: If this is difficult to implement on your system, you can probably
 * forget about the correction to Greenwich (e.g., 7 hours) and that for
 * daylight savings time (1 hour), and file times will come out a bit off
 * but it probably won't matter.
 */
long
os_utime (long iraf_time)
{
	struct	tm *localtime();
	time_t	time_var, lst;

	lst = (time_t)iraf_time;
	
	/* Add minutes westward from GMT */
	time_var = lst + os_timezone();

	/* Correct for daylight savings time, if in effect */
	if (localtime(&lst)->tm_isdst)
	    time_var += 60L * 60L;

	return ((long)time_var + SECONDS_1970_TO_1980);
}


/* OS_ITIME -- Convert UNIX time (gmt, epoch 1970) to IRAF time (lst, epoch
 * 1980).	[MACHDEP]
 */
long
os_itime (long unix_time)
{
	struct	tm *localtime();
	time_t	time_var, gmt;

	gmt = (time_t)unix_time;
	
	/* Subtract minutes westward from GMT */
	time_var = gmt - os_timezone();

	/* Correct for daylight savings time, if in effect */
	if (localtime(&gmt)->tm_isdst)
	    time_var -= 60L * 60L;

	return ((long)time_var - SECONDS_1970_TO_1980);
}


/* OS_GTIMEZONE -- Get the local timezone, measured in seconds westward
 * from Greenwich, ignoring daylight savings time if in effect.
 */
static long
os_timezone()
{
	struct tm *tm;
	time_t clock;
	clock = time(NULL);
	tm = gmtime (&clock);
	return (-(tm->tm_gmtoff));
}
