# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include <time.h>


# DTMCNV.X -- Date and time conversions.
# 
# The file contains the source for only the DTM routines listed below.  All
# the related system date and time routines are also summarized so that the
# whole (rather scattered) interface can be viewed at a glance.
# 
# FITS-Like Date and Time String Conversions
# 
#       status = dtm_decode (datestr, y,m,d, h, oldfits)
#       nchars = dtm_encode (datestr,maxch, y,m,d, h, precision, flags)
#   status = dtm_decode_hms (datestr, y,m,d, h,m,s, oldfits)
#   nchars = dtm_encode_hms (datestr,maxch, y,m,d, h,m,s, precision, flags)
# 
# 
# General Date and Time Conversions
# 
# 	          cnvdate (ltime, outstr, maxch)
# 		  cnvtime (ltime, outstr, maxch)
# 		  brktime (ltime, tm)
# 
# System Time
# 
#            lval = clktime (old_time)		# returns local time
# 	   lval = cputime (old_cputime)		# process cpu time, seconds
# 	   gmt = lsttogmt (lst)			# lst/gmt are in seconds
# 	   lst = gmttolst (gmt)			# lst/gmt are in seconds
# 
# 		sys_mtime (save_time)		# mark/print cpu time used
# 		sys_ptime (fd, opstr, save_time)
# 
# 
# Kernel Support
# 
# 		   zgtime (clktime, cputime)	# clock/cpu time in seconds
# 		   zgmtco (gmtcor)		# GMT = LST + gmtco (seconds)
# 
# LST here means local standard time (clock time), including any correction
# for daylight savings time.



# DTM_DECODE -- Decode the FITS format DATE-OBS string value into year,
# month, day and time fields.  OK is returned if the date string is
# successfully decoded, ERR if it is not.  The DATE-OBS string value may be
# in any of the following forms:  DD/MM/YY (flags = TF_OLDFITS), CCYY-MM-DD
# (flags = 0, time = INDEFD), or CCYY-MM-DDTHH:MM:SS[.SSS...] (flags=0,
# time = double precision number).  This routine verifies only the syntax.
# Routines in the SLALIB or ASTUTIL libraries can be used to check for
# valid year, month, day, or time values.

int procedure dtm_decode (datestr, year, month, day, time, flags)

char	datestr[ARB]	#I the input date-obs string
int	year		#O the output year (INDEFI if undefined)
int	month		#O the output month (INDEFI if undefined)
int	day		#O the output day (INDEFI if undefined)
double	time		#O the output time in hours (INDEFD if undefined)
int	flags		#O see <time.h>

double	dval
int	oldfits, ip, nchars, ival 
int	ctoi(), ctod()

begin
	# Initialize.
	year = INDEFI
	month = INDEFI
	day = INDEFI
	time = INDEFD
	flags = 0

	# Determine whether the format is old or new and get the day or
	# month accordingly.
	ip = 1
	nchars = ctoi (datestr, ip, ival)
	if (nchars == 2) {
	    flags = or (flags, TF_OLDFITS)
	    oldfits = YES
	    day = ival
	} else if (nchars == 4) {
	    flags = and (flags, not(TF_OLDFITS))
	    oldfits = NO
	    year = ival
	} else
	    return (ERR)

	# Check syntax.
	if (oldfits == NO && datestr[ip] == '-')
	    ip = ip + 1
	else if (oldfits == YES && datestr[ip] == '/')
	    ip = ip + 1
	else
	    return (ERR)

	# Get the month
	nchars = ctoi (datestr, ip, ival)
	if (nchars == 2) {
	    month = ival
	} else
	    return (ERR)
	if (oldfits == NO && datestr[ip] == '-')
	    ip = ip + 1
	else if (oldfits == YES && datestr[ip] == '/')
	    ip = ip + 1
	else
	    return (ERR)

	# Get the year or day.
	nchars = ctoi (datestr, ip, ival)
	if (nchars == 2) {
	    if (oldfits == YES)
		year = 1900 + ival
	    else
		day = ival
	} else
	    return (ERR)

	if (datestr[ip] != 'T' || oldfits == YES)
	    return (OK)

	# Get the time.
	ip = ip + 1
	nchars = ctod (datestr, ip, dval) 
	if (nchars < 8)
	    return (ERR)
	else
	    time = dval

	# Check for trailing garbage in the input string. Ignore whitespace.
	while (IS_WHITE(datestr[ip]))
	    ip = ip + 1

	if (datestr[ip] != EOS)
	    return (ERR)
	else
	    return (OK)
end


# DTM_DECODE_HMS -- Decode a FITS format DATE-OBS string into year, month,
# day, hours, minutes, and seconds fields.  OK is returned if the date string
# is successfully decoded, ERR if it is not.  The DATE-OBS string value may
# be in any of the following forms: DD/MM/YY (oldfits = YES), CCYY-MM-DD
# (oldfits = NO, hours = INDEFI, minutes = INDEFI, seconds = INDEFD), or
# CCYY-MM-DDTHH:MM:SS[.SSS...] (oldfits = NO, hours = integer, minutes =
# integer, seconds = double precision number).  This routine verifies only
# that the syntax is correct.  Routines in the SLALIB or ASTUTIL libraries
# can be used to check for valid year, month, day, or time values.

int procedure dtm_decode_hms (datestr,
	year, month, day, hours, minutes, seconds, flags)

char	datestr[ARB]	#I the input date-obs string
int	year		#O the output year (INDEFI if undefined)
int	month		#O the output month (INDEFI if undefined)
int	day		#O the output day (INDEFI if undefined)
int	hours		#O the output hours (INDEFI if undefined)
int	minutes		#O the output minutes (INDEFI if undefined)
double	seconds		#O the output seconds (INDEFD if undefined)
int	flags		#O see <time.h>

double	dval
int	oldfits, ip, nchars, ival 
int	ctoi(), ctod()

begin
	# Initialize.
	year = INDEFI
	month = INDEFI
	day = INDEFI
	hours = INDEFI
	minutes = INDEFI
	seconds = INDEFD
	flags = 0

	# Determine whether the format is old or new and get the day
	# or month accordingly.
	ip = 1
	nchars = ctoi (datestr, ip, ival)
	if (nchars == 2) {
	    flags = or (flags, TF_OLDFITS)
	    oldfits = YES
	    day = ival
	} else if (nchars == 4) {
	    flags = and (flags, not(TF_OLDFITS))
	    oldfits = NO
	    year = ival
	} else
	    return (ERR)

	# Check syntax.
	if (oldfits == NO && datestr[ip] == '-')
	    ip = ip + 1
	else if (oldfits == YES && datestr[ip] == '/')
	    ip = ip + 1
	else
	    return (ERR)

	# Get the month.
	nchars = ctoi (datestr, ip, ival)
	if (nchars == 2) {
	    month = ival
	} else
	    return (ERR)
	if (oldfits == NO && datestr[ip] == '-')
	    ip = ip + 1
	else if (oldfits == YES && datestr[ip] == '/')
	    ip = ip + 1
	else
	    return (ERR)

	# Get the year or day.
	nchars = ctoi (datestr, ip, ival)
	if (nchars == 2) {
	    if (oldfits == YES)
		year = 1900 + ival
	    else
		day = ival
	} else
	    return (ERR)

	if (datestr[ip] != 'T' || oldfits == YES)
	    return (OK)

	# Get the hours.
	ip = ip + 1
	nchars = ctoi (datestr, ip, ival) 
	if (nchars == 2)
	    hours = ival
	else
	    return (ERR)
	if (datestr[ip] != ':') 
	    return (ERR)

	# Get the minutes.
	ip = ip + 1
	nchars = ctoi (datestr, ip, ival) 
	if (nchars == 2)
	    minutes = ival
	else
	    return (ERR)
	if (datestr[ip] != ':') 
	    return (ERR)

	# Get the seconds.
	ip = ip + 1
	nchars = ctod (datestr, ip, dval) 
	if (nchars < 2)
	    return (ERR)
	else
	    seconds = dval

	# Check for trailing garbage in the input string. Ignore whitespace.
	while (IS_WHITE(datestr[ip]))
	    ip = ip + 1

	if (datestr[ip] != EOS)
	    return (ERR)
	else
	    return (OK)
end


# DTM_ENCODE -- Encode year, month, day and time fields into a valid FITS
# format DATE-OBS string value.  The number of characters in the output
# string is returned as the function value.  The returned DATE-OBS keyword
# value may be in any of the following formats: DD/MM/YY (oldfits = YES,
# 1900 <= year < 2000), CCYY-MM-DD (oldfits = NO, time = INDEFD), or
# CCYY-MM-DDTHH:MM:SS[.SSS...] (oldfits = NO, time = double precision
# number). This routine formats the string but does not check for valid
# input values.  Routines in the SLALIB or ASTUTIL libraries can be used
# to create valid year, month, day, or time values.

int procedure dtm_encode (datestr, maxch,
	year, month, day, time, precision, flags)

char	datestr[ARB]		#O the output date string
int	maxch			#I the maximum length of the output date string
int	year			#I the input year, e.g. 1999
int	month			#I the input month, e.g. 1-12
int	day			#I the input day, e.g. 1-31 
double	time			#I the input time in hours, INDEFD if undefined
int	precision		#I the precision of the output time field
int	flags			#I see <time.h>

int	oldfits, field
int	strlen(), btoi()

begin
	datestr[1] = EOS
	oldfits = btoi (and (flags, TF_OLDFITS) != 0)

	if (oldfits == YES) {
	    if (year >= 1900 && year < 2000) {
	        call sprintf (datestr, maxch, "%02d/%02d/%02d")
		    call pargi (day)
		    call pargi (month)
		    call pargi (mod (year, 1900))
	    }
	} else if (IS_INDEFD(time)) {
	    call sprintf (datestr, maxch, "%04d-%02d-%02d")
		call pargi (year)
		call pargi (month)
		call pargi (day)
	} else {
	    if (precision <= 0)
		field = 8
	    else
		field = 9 + precision
	    call sprintf (datestr, maxch, "%04d-%02d-%02dT%0*.*h")
	        call pargi (year)
	        call pargi (month)
	        call pargi (day)
	        call pargi (field)
	        call pargi (precision)
	        call pargd (time)
	}

	return (strlen (datestr))
end


# DTM_ENCODE_HMS -- Encode year, month, day, hours, minutes, and seconds
# fields into a valid FITS format DATE-OBS string value.  The number of
# characters in the output string is returned as the function value.  The
# returned DATE-OBS keyword value may be in any of the following formats:
# DD/MM/YY (oldfits = YES, 1900 <= year < 2000), CCYY-MM-DD (oldfits = NO,
# time = INDEFD), or CCYY-MM-DDTHH:MM:SS[.SSS...] (oldfits = NO, time =
# double precision number). This routine formats the string but does not
# check for valid input values.  Routines in the SLALIB or ASTUTIL libraries
# can be used to create valid year, month, day, or time values.

int procedure dtm_encode_hms (datestr, maxch,
	year, month, day, hours, minutes, seconds, precision, flags)

char	datestr[ARB]		#O the output date string
int	maxch			#I the maximum length of the output date string
int	year			#I the input year, e.g. 1999
int	month			#I the input month, e.g. 1-12
int	day			#I the input day, e.g. 1-31 
int	hours			#I the input hours field, INDEFI if undefined
int	minutes			#I the input minutes field, INDEFI if undefined
double	seconds			#I the input seconds field, INDEFD if undefined
int	precision		#I the precision of the output time field
int	flags			#I see <time.h>

int	oldfits, field
int	strlen(), btoi()

begin

	datestr[1] = EOS
	oldfits = btoi (and (flags, TF_OLDFITS) != 0)

	if (oldfits == YES) {
	    if (year >= 1900 && year < 2000) {
	        call sprintf (datestr, maxch, "%02d/%02d/%02d")
		    call pargi (day)
		    call pargi (month)
		    call pargi (mod (year, 1900))
	    }
	} else if (IS_INDEFI(hours) || IS_INDEFI(minutes) ||
	    IS_INDEFD(seconds)) {
	    call sprintf (datestr, maxch, "%04d-%02d-%02d")
		call pargi (year)
		call pargi (month)
		call pargi (day)
	} else {
	    if (precision <= 0)
		field = 2
	    else
		field = 3 + precision
	    call sprintf (datestr, maxch, "%04d-%02d-%02dT%02d:%02d:%0*.*f")
	        call pargi (year)
	        call pargi (month)
	        call pargi (day)
		call pargi (hours)
		call pargi (minutes)
	        call pargi (field)
	        call pargi (precision)
	        call pargd (seconds)
	}

	return (strlen (datestr))
end
