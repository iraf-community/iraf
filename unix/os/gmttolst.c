/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <time.h>

/* GMT_TO_LST -- Convert gmt to local standard time, epoch 1980.
 */
time_t
gmt_to_lst (gmt)
time_t	gmt;
{
	struct	tm epoch;
	time_t	epoch_sec;

	epoch.tm_sec = 0;
	epoch.tm_min = 0;
	epoch.tm_hour = 0;
	epoch.tm_mday = 1;
	epoch.tm_mon = 0;
	epoch.tm_year = 80;
	epoch.tm_isdst = 0;
	epoch_sec = mktime(&epoch);
	
	return (gmt - epoch_sec);
}
