include <time.h>

# DP_DATE -- Create the date and time strings for the daophot output files.

procedure dp_date (date, time, maxch)

char	date[ARB]	# the date string
char	time[ARB]	# the time string
int	maxch		# the maximum number of character in the string

int	tm[LEN_TMSTRUCT]
long	ctime
long	clktime()
#long	lsttogmt()

begin
	ctime = clktime (long(0))
	#ctime = lsttogmt (ctime)
	call brktime (ctime, tm)
	call sprintf (date, maxch, "%04d-%02d-%02d")
	    call pargi (TM_YEAR(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_MDAY(tm))
	call sprintf (time, maxch, "%02d:%02d:%02d")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
end
