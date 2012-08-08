# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<time.h>

define	SZ_WEEKDAY		3
define	SZ_MONTH		3

# CNVDATE -- Convert a time in integer seconds since midnight on Jan 1, 1980
# into a short string such as "May 15 18:24".  The length of the output
# string is given by the parameter SZ_DATE in <time.h>.  Note that CNVTIME
# is also available if a longer, more informative string is desired.

procedure cnvdate (ltime, outstr, maxch)

long	ltime			# seconds since 00:00:00 01-Jan-1980
char	outstr[ARB]
int	maxch

long	one_year_ago
int	fd, tm[LEN_TMSTRUCT]

long	clktime()
int	stropen()
string	month "JanFebMarAprMayJunJulAugSepOctNovDec"
data	one_year_ago /0/
errchk	stropen

begin
	if (one_year_ago == 0)
	    one_year_ago = clktime (0) - 3600 * 24 * (365 - 31)

	call brktime (ltime, tm)
	fd = stropen (outstr, maxch, NEW_FILE)

	call fprintf (fd, "%3.3s %2d ")
	    call pargstr (month [(TM_MONTH(tm) - 1) * SZ_MONTH + 1])
	    call pargi (TM_MDAY(tm))

	# If time is recent (within the past year), print the time of day,
	# otherwise print the year.

	if (ltime > one_year_ago) {
	    call fprintf (fd, "%2d:%02d")
		call pargi (TM_HOUR(tm))
		call pargi (TM_MIN(tm))
	} else {
	    call fprintf (fd, "%5d")
		call pargi (TM_YEAR(tm))
	}

	call strclose (fd)
end
