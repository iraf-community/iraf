include <time.h>

define	CENTURY	1900

# APDATE -- Fetch the date and time strings required for the apphot output
# files.

procedure apdate (date, time, maxch)

char	date[ARB]	# the date string
char	time[ARB]	# the time string
int	maxch		# the maximum number of character in the string

int	tm[LEN_TMSTRUCT]
long	clktime()

begin
	call brktime (clktime (long(0)), tm)
	call sprintf (date, maxch, "%02d-%02d-%02d")
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_MDAY(tm))
	    call pargi (TM_YEAR(tm) - CENTURY)
	call sprintf (time, maxch, "%02d:%02d:%02d")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
end
