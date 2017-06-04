/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <time.h>

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

	/* Mac systems already include the DST offset in the GMT offset */
	if (localtime(&gmtl)->tm_isdst)
	    time_var += 60L * 60L;

	return (time_var - SECONDS_1970_TO_1980);
}


/* _TIMEZONE -- Get the local timezone, measured in seconds westward
 * from Greenwich, ignoring daylight savings time if in effect.
 */
static long
get_timezone()
{
	extern	long timezone;
	tzset();
	return (timezone);
}
