# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<time.h>

define	SZ_WEEKDAY		3
define	SZ_MONTH		3

# CNVTIME -- Convert a time in integer seconds since midnight on Jan 1, 1980
# into a string, i.e., "Mon 16:30:05 17-Mar-2001".  The maximum length of the
# output string is given by the parameter SZ_TIME in <time.h>.

procedure cnvtime (ltime, outstr, maxch)

long	ltime			# seconds since 00:00:00 01-Jan-1980
char	outstr[ARB]
int	maxch
int	tm[LEN_TMSTRUCT]	# broken down time structure
string	weekday "SunMonTueWedThuFriSat"
string	month   "JanFebMarAprMayJunJulAugSepOctNovDec"

begin
	call brktime (ltime, tm)
	call sprintf (outstr, maxch, "%3.3s %02d:%02d:%02d %02d-%3.3s-%04d")
	    call pargstr (weekday [(TM_WDAY(tm) - 1) * SZ_WEEKDAY + 1])
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	    call pargi (TM_MDAY(tm))
	    call pargstr (month [(TM_MONTH(tm) - 1) * SZ_MONTH + 1])
	    call pargi (TM_YEAR(tm))
end
