/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/timeb.h>

#define	SECONDS_1970_TO_1980	315532800L

/* GMT_TO_LST -- Convert gmt to local standard time, epoch 1980.
 */
time_t
gmt_to_lst (gmt)
time_t	gmt;
{
	struct	timeb time_info;
	struct	tm *localtime();
	time_t	time_var;
	long	gmtl;
	
	/* Subtract minutes westward from GMT */
	ftime (&time_info);
	time_var = gmt - time_info.timezone * 60L;

	/* Correct for daylight savings time, if in effect */
	gmtl = (long)gmt;
	if (localtime(&gmtl)->tm_isdst)
	    time_var += 60L * 60L;

	return (time_var - SECONDS_1970_TO_1980);
}
