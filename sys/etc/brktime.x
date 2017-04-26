# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <time.h>

define	SECONDS_PER_DAY		86400
define	SECONDS_PER_HOUR	3600
define	SECONDS_PER_MINUTE	60
define	MONDAY			2

# BRKTIME -- Break a long integer time (such as returned by GETTIME or FINFO)
# into the fields of the structure defined in <time.h>.  The procedure is
# valid from 00:00:00 01-Jan-1980 to 23:23:59 28-Feb-2100.

procedure brktime (ltime, tm)

long	ltime			# seconds since 00:00:00 01-Jan-1980
int	tm[LEN_TMSTRUCT]	# broken down time (output struct)

long	temp			# working variable
long	seconds			# seconds in current day
long	days			# whole days since Monday 00-Jan-1980
int	nights			# whole days since 00-Jan of current year

int	year			# year counter
int	days_per_year		# days per year

int	month			# month counter
int	days_per_month[12]	# days per month
data	days_per_month/31,0,31,30,31,30,31,31,30,31,30,31/

begin
        seconds = mod (ltime, SECONDS_PER_DAY)
        days = ltime / SECONDS_PER_DAY + 1

        # Break hours, minutes, seconds.

        TM_HOUR(tm) = seconds / SECONDS_PER_HOUR
        temp = seconds - TM_HOUR(tm) * SECONDS_PER_HOUR
        TM_MIN(tm) = temp / SECONDS_PER_MINUTE
        TM_SEC(tm) = temp - TM_MIN(tm) * SECONDS_PER_MINUTE

        # Break day of week.

        TM_WDAY(tm) = mod (days + MONDAY, 7)
	if (TM_WDAY(tm) == 0)
	    TM_WDAY(tm) = 7

        # Break year, day of year.

        temp = 0	# whole days since 00-Jan-1980 on last day of last year
        year = 1980
	days_per_year = 366
	while (days > temp + days_per_year) {
	    temp = temp + days_per_year
	    year = year + 1
	    if (mod (year, 4) == 0)
	        days_per_year = 366
	    else
	        days_per_year = 365
        }
        TM_YEAR(tm) = year
        TM_YDAY(tm) = days - temp

        # Break month, day of month.

	nights = TM_YDAY(tm)
	if (mod (TM_YEAR(tm), 4) == 0)
	    days_per_month[2] = 29
	else
	    days_per_month[2] = 28
	temp = 0	# whole days since 00-Jan on last day of last month
	month = 1
	while (nights > temp + days_per_month[month]) {
	    temp = temp + days_per_month[month]
	    month = month + 1
	}
	TM_MONTH(tm) = month
	TM_MDAY(tm) = nights - temp
end
