include <time.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"
include "rvcomdef.h"

define	SZ_DATEBUF		16

# RV_PARAM -- Procedure to write the rv parameters to a text file.

procedure rv_param (rv, out, task)

pointer	rv		# RV struct pointer
pointer	out		# database descriptor
char	task[ARB]	# task name

int	nchars
pointer	sp, outstr, date, time
int	envfind(), gstrcpy()

begin
	if (out == NULL)
	    return
	 
	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATEBUF, TY_CHAR)
	call salloc (time, SZ_DATEBUF, TY_CHAR)

	# Write the id.
	nchars = envfind ("version", Memc[outstr], SZ_LINE)
	if (nchars <= 0)
	    nchars = gstrcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call rv_sparam (out, "IRAF", Memc[outstr], "version",
	    "current version of IRAF")
	nchars = envfind ("userid", Memc[outstr], SZ_LINE)
	call rv_sparam (out, "USER", Memc[outstr], "name", "user id")
	call gethost (Memc[outstr], SZ_LINE)
	call rv_sparam (out, "HOST", Memc[outstr], "computer",
	    "IRAF host machine")
	call rv_date (Memc[date], Memc[time], SZ_DATEBUF)
	call rv_sparam (out, "DATE", Memc[date], "yyyy-mm-dd", "date")
	call rv_sparam (out, "TIME", Memc[time], "hh:mm:ss", "time")
	call rv_sparam (out, "PACKAGE", "rv", "name",
	    "name of IRAF package")
	call rv_sparam (out, "TASK", task, "name", "name of rv task")

	call sfree (sp)
end


# RV_DATE -- Procedure to produce the date and time strings for RV output files.

procedure rv_date (date, time, maxch)

char	date[SZ_LINE]	# the date string
char	time[SZ_LINE]	# the time string
int	maxch		# the maximum number of character in the string

int	tm[LEN_TMSTRUCT]
long	clktime()

begin
	call brktime (clktime (long(0)), tm)
	call sprintf (date, maxch, "%04d-%02d-%02d")
	    call pargi (TM_YEAR(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_MDAY(tm))
	call sprintf (time, maxch, "%02d:%02d:%02d")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
end


# RV_IPARAM -- Procedure to encode an rv integer parameter.

procedure rv_iparam (out, keyword, value, units, comments)

pointer	out		# output file descriptor
char	keyword[ARB]	# keyword string
int	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out, "#K%4t%-10.10s%14t = %17t%-15d\n")
	    call pargstr (keyword)
	    call pargi (value)
	    call pargstr (units)
end


# RV_SPARAM -- Procedure to encode an rv string parameter.

procedure rv_sparam (out, keyword, value, units, comments)

pointer	out		# output file descriptor
char	keyword[ARB]	# keyword string
char	value[ARB]	# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

bool	streq()

begin
	if (out == NULL)
	    return

	call strupr (keyword)
	if (streq(keyword,"REGIONS") ||
	    streq(keyword,"APNUM")) {
                call fprintf (out, "#K%4t%-10.10s%14t = %17t%-s\n")
	            call pargstr (keyword)
	            call pargstr (value)
	} else {
            call fprintf (out, "#K%4t%-10.10s%14t = %17t%-30.30s%48t%-10.10s\n")
	        call pargstr (keyword)
	        call pargstr (value)
	        call pargstr (units)
	}
end
