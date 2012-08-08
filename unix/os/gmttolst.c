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
static	long get_timezone();

/* GMT_TO_LST -- Convert gmt to local standard time, epoch 1980.
 */
time_t
gmt_to_lst (gmt)
time_t	gmt;
{
	struct	tm *localtime();
	time_t	time_var;
	long	gmtl;
	
	/* Subtract seconds westward from GMT */
	time_var = gmt - get_timezone();

	/* Correct for daylight savings time, if in effect */
	gmtl = (long)gmt;

#ifndef MACOSX
	/* Mac systems already include the DST offset in the GMT offset */
	if (localtime(&gmtl)->tm_isdst)
	    time_var += 60L * 60L;
#endif

	return (time_var - SECONDS_1970_TO_1980);
}


/* _TIMEZONE -- Get the local timezone, measured in seconds westward
 * from Greenwich, ignoring daylight savings time if in effect.
 */
static long
get_timezone()
{
#ifdef CYGWIN
	extern	long _timezone;
	tzset();
	return (_timezone);
#else
#ifdef SYSV
	extern	long timezone;
	tzset();
	return (timezone);
#else
#ifdef MACOSX
	struct tm *tm;
	time_t clock = time(NULL);
	tm = localtime (&clock);
	return (-(tm->tm_gmtoff));
#else
	struct timeb time_info;
	ftime (&time_info);
	return (time_info.timezone * 60);
#endif
#endif
#endif
}
