/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#ifdef SYSV
#include <time.h>
#else
#include <sys/time.h>
#include <sys/timeb.h>
#endif

#ifdef MACOSX
#include <time.h>
#endif

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
#ifdef AUX
	long	lstl;
#endif

	lst = (time_t)iraf_time;
	
	/* Add minutes westward from GMT */
	time_var = lst + os_timezone();

	/* Correct for daylight savings time, if in effect */
#ifdef AUX
	lstl = (long)lst;
	if (localtime(&lstl)->tm_isdst)
#else
	if (localtime(&lst)->tm_isdst)
#endif
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
#ifdef AUX
	long	gmtl;
#endif

	gmt = (time_t)unix_time;
	
	/* Subtract minutes westward from GMT */
	time_var = gmt - os_timezone();

	/* Correct for daylight savings time, if in effect */
#ifdef AUX
	gmtl = (long)gmt;
	if (localtime(&gmtl)->tm_isdst)
#else
	if (localtime(&gmt)->tm_isdst)
#endif
	    time_var -= 60L * 60L;

	return ((long)time_var - SECONDS_1970_TO_1980);
}


/* OS_GTIMEZONE -- Get the local timezone, measured in seconds westward
 * from Greenwich, ignoring daylight savings time if in effect.
 */
static long
os_timezone()
{
#ifdef CYGWIN
	extern	long _timezone;
	return (_timezone);
#else
#if defined(SOLARIS) && defined(X86)
	extern	long timezone;
	return (timezone);

#else
#if defined(SYSV) || defined(MACOSX)
	struct tm *tm;
	time_t clock;
	clock = time(NULL);
	tm = gmtime (&clock);
	return (-(tm->tm_gmtoff));
#else
	struct	timeb time_info;
	ftime (&time_info);
	return (time_info.timezone * 60);
#endif
#endif
#endif
}
